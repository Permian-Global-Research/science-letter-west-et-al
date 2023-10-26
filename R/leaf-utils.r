#' generate the base tmap object.
#' @import ggplot2
#' @import dplyr
#' @import magrittr
#' @importFrom ./R/workflow_setup.R set_path
tm_basic <- function() {
  tmap::tm_basemap(tmap::providers$CartoDB.Positron) +
    tmap::tm_basemap(tmap::providers$OpenStreetMap) +
    tmap::tm_basemap(tmap::providers$Esri.WorldImagery) +
    tmap::tm_basemap(tmap::providers$CartoDB.DarkMatter)
}
