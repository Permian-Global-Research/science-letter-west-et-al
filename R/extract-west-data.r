#' @title Extract West et al. (2023) data
#' @description Extracts the West et al. (2023) data from the compressed file
#'  'data/West et al. (2023) data.7z' into the 'west-data' directory.
extract_west_data <- function() {
    sys_call <- function(x) {
        system(x, ignore.stdout = TRUE, ignore.stderr = TRUE)
    }

    ndirs <- list.dirs("west-data", recursive = TRUE)
    x <- try({
        if (!dir.exists("west-data") || length(ndirs) != 5) {
            safe_create("west-data")
            sys_call("7z x -owest-data/ 'data/West et al. (2023) data.7z'")
            sys_call("7z x -'owest-data/West et al. (2023) data' 'west-data/West et al. (2023) data/Main datasets.7z'")
            sys_call("7z x -'owest-data/West et al. (2023) data' 'west-data/West et al. (2023) data/Shapefiles.7z'")
        } else {
            cli::cli_alert_success("Data already extracted")
        }
    })

    if (x == 2) {
        cli::cli_abort(
            c(
                "Could not unzip the file 'data/West et al. (2023) data.7z'",
                "i" = "Please unzip manually and try again"
            )
        )
    }
    return(invisible())
}
