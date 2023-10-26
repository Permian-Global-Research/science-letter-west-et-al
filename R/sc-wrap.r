#' @title Run the synthetic control algorithm
#' @param filepath the path to the input csv containing the sc data
#' @param dbf_file the dbf file associated with the shp file for the country
#' @param project_id the numeric ID of the project
#' @param project_start_date the numeric year start date of the project.
#' @param drop_controls numeric control Polygon ID numbers that should be
#' excluded. Unclear what the exact critera are to remove these...
#' @param i the iteration number added to the seed.
#' @param seed the seed for the random number generator.
#' @param predictors_list the list of predictors to use in the model.
#' @param donor_prop the proportion of donors to use in the model.
#' @param ... additional arguments passed to sc_project_data used internally.
#' @details cutoff is iterated recursively starting at 0.1. it can be specified
#' in ... but best not to in order to mathc the stated methods in the paper.
sc_wrap_recursive <- function(
    filepath,
    dbf_file,
    project_id,
    project_start_date,
    project_name,
    drop_controls = c(),
    i = 0,
    seed = 131313, # seed from paper
    predictors_list = c(
      "tree", "IL", "pa", "slope", "dem", "fric", "def",
      "defc", "mm", "buf10k_def", "buf10k_defc"
    ),
    donor_prop = 1,
    min_donors = 2,
    ...) {
  dots <- list(...)
  if (!"cutoff_start" %in% names(dots)) {
    cutoff_start <- 0.1
  } else {
    cutoff_start <- dots$cutoff_start
  }
  if (cutoff_start > 1) {
    cli::cli_abort("Could not make generate valid Synthetic Control.")
  }

  sc_wrap <- function(new_cutoff) {
    sc_data <- sc_project_data(
      filepath = filepath,
      dbf_file = dbf_file,
      project_id = project_id,
      project_start_date = project_start_date,
      project_name = project_name,
      cutoff = new_cutoff,
      drop_controls = drop_controls
    )

    synth_control_run(
      project_data = sc_data,
      i = i,
      seed = seed,
      predictors_list = predictors_list,
      donor_prop = donor_prop,
      min_donors = min_donors
    )
  }

  sc_result <- try(tryCatch(
    {
      sc_wrap(
        new_cutoff = cutoff_start
      )
    },
    # warning = function(w) {
    #   stop(w$message)
    # },
    error = function(e) {
      sc_fail(cutoff_start)
      stop(e$message)
    }
  ))

  if (inherits(sc_result, "try-error")) {
    sc_wrap_recursive(
      filepath = filepath,
      dbf_file = dbf_file,
      project_id = project_id,
      project_start_date = project_start_date,
      project_name = project_name,
      drop_controls = drop_controls,
      i = i,
      seed = seed,
      predictors_list = predictors_list,
      donor_prop = donor_prop,
      cutoff_start = cutoff_start + 0.1,
      min_donors = min_donors
    )
  } else {
    cli::cli_alert_success(
      "Synthetic Control succeeded with cutoff of {cutoff_start}."
    )
    return(list(sc_result = sc_result, cutoff = cutoff_start))
  }
}

sc_fail <- function(x) {
  cli::cli_alert_danger(
    "Synthetic Control failed with cutoff of {x}."
  )
}

sc_success <- function(x) {
  cli::cli_alert_success(
    "Synthetic Control succeeded with cutoff of {x}."
  )
}
