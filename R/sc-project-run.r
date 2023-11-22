#' @title run the synthetic control algorithm
#' @param project_data the output from `sc_project_data`
#' @param i numeric; the iteration number
#' @param seed numeric; the seed for the random number generator
#' @param predictors_list character; the list of predictors to use in the model
#' @param donor_prop numeric; the proportion of donors to use in the model
#' @param min_donors numeric; the minimum number of donors to use in the model
#' @return list of results
#'
synth_control_run <- function(
    project_data,
    i = 0,
    seed = 131313, # seed from paper
    predictors_list = c(
      "tree", "IL", "pa", "slope", "dem", "fric", "def",
      "defc", "mm", "buf10k_def", "buf10k_defc"
    ),
    donor_prop = 0.8,
    min_donors = 2) {
  set.seed(seed + i)

  d <- project_data$data
  controls_identifier <- project_data$control_id
  treatment_identifier <- project_data$treat_id
  project_start_date <- project_data$proj_start
  project_name <- project_data$proj_name

  fold <- as.data.frame(sample(
    controls_identifier,
    round(length(controls_identifier) * donor_prop, digits = 0)
  ))
  colnames(fold) <- "polygon_ID"
  fold$keep <- 1

  d$keep <- fold[match(with(d, polygon_ID), with(fold, polygon_ID)), ]$keep
  d <- subset(d, keep == 1 | REDD == 1)
  d_controls <- subset(d, REDD == 0)
  controls_identifier_loop <- unique(d_controls$polygon_ID)

  # data preparation
  dataprep.out <-
    dataprep(
      foo = project_data$data, # data name
      predictors = predictors_list,
      predictors.op = "mean", # variables based on means
      time.predictors.prior = 2001:c(project_start_date), # interval used for pretreatment matching, varies by project
      dependent = "defc", # deforestation from Global Forest Cover dataset
      unit.variable = "polygon_ID",
      unit.names.variable = "polygon_name",
      time.variable = "year",
      treatment.identifier = treatment_identifier, # polygon that is REDD+ project under evaluation
      controls.identifier = controls_identifier_loop, # polygons that are not REDD+ projects
      time.optimize.ssr = 2001:c(project_start_date), # interval used for matching
      time.plot = 2001:2020 # time interval of entire analysis
    )

  # PART 2: Synthetic control analysis-------------------------------------------------------------------------------------
  #
  synth.out <- synth(data.prep.obj = dataprep.out)
  # checking results based on https://cran.r-project.org/web/packages/MSCMT/vignettes/CheckingSynth.html
  synth.out <- improveSynth(synth.out, dataprep.out)

  # browser()

  # results
  gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
  gaps[, 1]
  # discrepancies in deforestation between REDD+ project and its synthetic control

  # result table
  synth.tables <- Synth::synth.tab(
    dataprep.res = dataprep.out,
    synth.res = synth.out
  )
  # OBS: synth.tables has many comparison stats
  synth.tables$tab.pred # pre-treatment stats (Sample Mean = mean of control candidates)
  synth.tables$run <- i
  # write.csv(synth.tables$tab.pred, file = paste("904_matching_results_", selected_list[L], ".csv", sep = ""))

  weights_tab <- subset(synth.tables$tab.w, w.weights != 0)
  print(weights_tab)
  n_donors <- nrow(weights_tab)
  if (n_donors < min_donors) {
    cli::cli_abort(
      "only {n_donors} donors used in the SC. This is fewer than the minimum number of donors ({min_donors})."
    )
  }
  # % contribution from the control candidates used
  # to construct the synthetic control
  # write.csv(subset(synth.tables$tab.w, w.weights != 0), file = paste("904_controls_used_", i, ".csv"))

  # plot pre- & post-deforestation trends
  path.plot(
    synth.res = synth.out,
    dataprep.res = dataprep.out,
    Ylab = "Deforestation (ha)", Xlab = "Year" # ,
    # Ylim = c(-10,650),
    # Legend.position = NA
  )
  abline(v = project_start_date, lty = "dotted", lwd = 2)

  # save results for ggplot
  out_df <- as.data.frame(dataprep.out$Y1plot)
  out_df[, 2] <- dataprep.out$Y0plot %*% synth.out$solution.w
  out_df[, 3] <- "856"
  out_df[, 4] <- project_start_date
  colnames(out_df) <- c("project.def", "synth.def", "project", "start")

  out_df$loss.v <- as.vector(synth.out$loss.v)
  out_df$loss.w <- as.vector(synth.out$loss.w)

  out_df$run <- i
  out_df$project <- project_name
  out_df$type <- "cross_validation"

  weights_df <- as.data.frame(synth.out$solution.w)
  weights_df$poly_id <- as.numeric(rownames(weights_df))
  rownames(weights_df) <- NULL

  return(list(
    exp_data = out_df,
    synth_result = synth.out,
    s_weights = weights_df,
    project_gaps = gaps
  ))
}

#' get the spatial locations of the donors for a given project.
#' @param sc the output from `synth_control_run`
#' @param west_spatial the output from `read_west_spatial`
#' @return an sf object with the spatial locations of the donors
locate_donors <- function(sc, west_spatial) {
  synth_donors <- sc$sc_result$s_weights

  west_spatial |>
    dplyr::filter(polygon_ID %in% synth_donors$poly_id) |>
    dplyr::left_join(synth_donors,
      by = c("polygon_ID" = "poly_id")
    ) |>
    dplyr::select(
      "Polygon-ID" = polygon_ID,
      "Synthetic control weight" = w.weight
    )
}

#' count the number of donors used in the SC
#' @param sc the output from `synth_control_run`
#' @return a list with the number of donors used in the SC
count_donors <- function(sc) {
  synth_donors <- sc$sc_result$s_weights
  n_donors <- nrow(synth_donors)
  n_weighted_donors <- nrow(synth_donors[synth_donors$w.weight > 0, ])
  return(list(
    n_pot = n_donors,
    n_weighted = n_weighted_donors
  ))
}
