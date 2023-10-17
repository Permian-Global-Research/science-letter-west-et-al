extract_west_data <- function() {
    if (!dir.exists("west-data")) {
        dir.create("west-data")
        system("7z x -owest-data/ 'data/West et al. (2023) data.7z'")
        system("7z x -'owest-data/West et al. (2023) data' 'west-data/West et al. (2023) data/Main datasets.7z'")
        system("7z x -'owest-data/West et al. (2023) data' 'west-data/West et al. (2023) data/Shapefiles.7z'")
    } else {
        cli::cli_alert_success("Data already extracted")
    }
}
