#' @title Generate dataframe for Synthetic Control Sim.
#' @description Create the dataframe input for the synthetic control simulation
#' at the project level. Refactored from "Colombia_856_k.R"
#' @param filepath the path to the input csv containing the sc data
#' @param dbf_file the dbf file associated with the shp file for the country
#' @param project_id the numeric ID of the project
#' @param project_start_date the numeric year start date of the project.
#' @param cutoff numeric between 0 and 1. the proportion of difference between
#' the buffer deforestation of the controls from the project's that is allowed.
#' article states this should start at 0.1 but the example uses 0.2.
#' @param drop_controls numeric control Polygon ID numbers that should be
#' excluded. Unclear what the exact critera are to remove these...
#'
sc_project_data <- function(
    filepath,
    dbf_file,
    project_id,
    project_start_date,
    project_name,
    cutoff = 0.2,
    drop_controls = c()) {
    redd_data <- read.csv(filepath)[, -1]
    redd_data$polygon_name <- as.character(redd_data$polygon_ID)

    # remove problematic control (varies by project/country) #WHY?
    redd_data <- subset(redd_data, !polygon_ID %in% drop_controls)

    projects <- subset(redd_data, REDD == 1)
    unique(projects$ID)
    # select one REDD project and delete the others and controls that should not be used

    redd_data <- subset(redd_data, ID == project_id) # keep only controls specific for the project
    project <- subset(redd_data, REDD == 1)

    # fix polygon area based on an updated shapefile
    # HG - file path naming doesn't match their data! Assuming this refers to the
    #      shapefiles so changing.
    new <- foreign::read.dbf(dbf_file)
    new$polygon_ha <- as.numeric(as.character(new$polygon_ha))
    redd_data$polygon_ha <- new[match(
        with(redd_data, polygon_ID),
        with(new, polygon_ID)
    ), ]$polygon_ha

    # set treatment_identifier
    treatment_identifier <- subset(redd_data, REDD == 1)[1, 5] # the polygon_ID

    # calculate deforestation risk before.data[0, ] project implementation
    risk_project <- plyr::ddply(
        .data = subset(project, year <= project_start_date),
        .(polygon_ID), .fun = summarise, risk = mean(buf10k_def)
    )
    controls <- subset(redd_data, REDD != 1 & polygon_ID != treatment_identifier)
    risk_control <- plyr::ddply(
        .data = subset(controls, year <= project_start_date),
        .(polygon_ID), .fun = summarise, risk = mean(buf10k_def)
    )

    # pre-subset

    risk_control$keep <- ifelse(
        risk_control$risk >= risk_project[1, 2] * (1 - cutoff) &
            risk_control$risk <= risk_project[1, 2] * (1 + cutoff),
        1,
        0
    )
    table(risk_control$keep)

    controls$keep <- risk_control[match(
        with(controls, polygon_ID),
        with(risk_control, polygon_ID)
    ), ]$keep
    controls <- subset(controls, keep == 1)
    project$keep <- 1
    redd_data <- rbind(project, controls)

    # set controls.identifier
    controls_identifier <- unique(controls$polygon_ID)

    unique(redd_data$polygon_ha)
    redd_data$polygon_ha <- round(redd_data$polygon_ha, digits = 0)

    out <- list(
        data = redd_data,
        control_id = controls_identifier,
        treat_id = treatment_identifier,
        proj_start = project_start_date,
        proj_name = project_name
    )

    class(out) <- "sc.project.data"

    return(out)
}
