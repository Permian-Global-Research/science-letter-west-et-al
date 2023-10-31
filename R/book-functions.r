#' @title generate county level map layers
#' @param country_name character; name of the country
#' @return list of sf objects
ountry_map_lyrs <- function(country_name) {
  safe_create("geo-data")
  # read the spatial data for Colombia from West
  country_dbf <- dp(
    paste0(country_name, "_polygons.dbf"),
    "west-data/West et al. (2023) data/Shapefiles"
  )
  ifelse(country_name == "Cambodia", "cambodia", country_name)
  country_csv <- dp(paste0(
    ifelse(country_name == "Cambodia", "cambodia", country_name),
    "_synth_control_data.csv"
  ))
  country_sc_sp <- read_west_spatial(paste0(country_name, "_polygons.shp"))

  regions <- states_and_country(tolower(country_name)) |>
    purrr::map(~ rmapshaper::ms_simplify(.x))
  country <- regions$country
  states <- regions$states |>
    dplyr::select(State = shapeName)

  er_path <- paste0("geo-data/", tolower(country_name), "_ecoregions.gpkg")
  if (!file.exists(er_path)) {
    country_er <- eco_regions(country) |>
      rmapshaper::ms_simplify() |>
      dplyr::select(Ecoregion = ECO_NAME, Biome = BIOME_NAME, Realm = REALM)
    sf::write_sf(country_er, er_path, delete_dsn = TRUE)
  } else {
    country_er <- sf::read_sf(er_path)
  }

  return(list(
    country = country,
    states = states,
    ecoregions = country_er,
    country_csv = country_csv,
    country_dbf = country_dbf,
    country_sc_sp = country_sc_sp
  ))
}

#' @title create interactive donor map.
#' @param proj_donors sf object of the donors
#' @param country_spat sf object of the country
#' @param country_name character; name of the country
#' @param project_area sf object of the project area
#' @param proj_num numeric; the project number
#' @param ecoregions sf object of the ecoregions
#' @param states sf object of the states
#' @return leaflet object
donor_leaf_map <- function(
    proj_donors,
    country_spat, country_name, project_area, proj_num,
    ecoregions, states) {
  project_name <- paste0("Project-", proj_num)
  project_area$`Project area` <- project_name
  tmap::tmap_mode("view")
  n_weights <- length(unique(proj_donors$`Donor mean SC weight`))
  donor_pal <- c("#bdbdbd", cols4all::c4a("tol.rainbow_wh_br", n_weights - 1))
  m <- tm_basic() +
    tmap::tm_shape(country_spat, name = country_name) +
    tmap::tm_polygons(
      col = "grey50", fill = NA,
      group.control = "none", popup.vars = FALSE
    ) +
    tmap::tm_shape(project_area, name = project_name) +
    tmap::tm_polygons(
      fill = "Project area", col = "#c100e7",
      fill.scale = tmap::tm_scale_categorical(values = "#c100e7"),
      fill_alpha = 0.8, popup.vars = FALSE,
      lwd = 5,
      fill.legend = tmap::tm_legend(title = "")
    ) +
    tmap::tm_shape(sf::st_make_valid(ecoregions), name = "Ecoregions") +
    tmap::tm_polygons(
      fill = "Ecoregion", col = "Ecoregion",
      fill_alpha = 0.2, popup.vars = TRUE,
      fill.scale = tmap::tm_scale_categorical(values = "tableau.classic20"),
      col.scale = tmap::tm_scale_categorical(values = "tableau.classic20"),
      fill.legend = tmap::tm_legend(show = FALSE),
      col.legend = tmap::tm_legend(show = FALSE)
    ) +
    tmap::tm_shape(states, name = "States") +
    tmap::tm_polygons(
      fill = "State", col = "State",
      fill_alpha = 0.2, popup.vars = TRUE,
      fill.scale = tmap::tm_scale_categorical(values = "tableau.classic20"),
      col.scale = tmap::tm_scale_categorical(values = "tableau.classic20"),
      fill.legend = tmap::tm_legend(show = FALSE),
      col.legend = tmap::tm_legend(show = FALSE)
    ) +
    tmap::tm_shape(proj_donors, name = paste0(project_name, " Donors")) +
    tmap::tm_polygons(
      fill = NA, col = "Donor mean SC weight", popup.vars = TRUE,
      col.scale = tmap::tm_scale_categorical(values = donor_pal),
      lwd = 5
    )
  ml <- tmap::tmap_leaflet(m) |>
    leaflet::hideGroup(c("Ecoregions", "States")) |>
    leaflet::addScaleBar(position = "bottomleft")
  ml
}

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
