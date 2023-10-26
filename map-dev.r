source("R/load-all.r")
# read the spatial data for Colombia from West
colmbia_lyrs <- country_map_lyrs("Colombia")

project_856 <- west_extract_proj_area(colmbia_lyrs$country_sc_sp, 856)
proj_donors <- sc_wrap_recursive(
  filepath = colmbia_lyrs$country_csv,
  dbf_file = colmbia_lyrs$country_dbf,
  project_id = 856,
  project_start_date = 2011,
  project_name = "Colombia 856",
  drop_controls = c(8851)
) |>
  locate_donors(colmbia_lyrs$country_sc_sp)

tmap::tmap_mode("view")
m <- tm_basic() +
  tmap::tm_shape(country, name = "Colombia") +
  tmap::tm_polygons(col = "grey50", fill = NA, group.control = "none", popup.vars = FALSE) +
  tmap::tm_shape(project_856, name = "Project-856") +
  tmap::tm_polygons(
    fill = "#D95F02", col = "#D95F02",
    fill_alpha = 0.8, popup.vars = FALSE
  ) +
  tmap::tm_shape(colombia_er, name = "Ecoregions") +
  tmap::tm_polygons(
    fill = "Ecoregion", col = "Ecoregion",
    fill_alpha = 0.4, popup.vars = TRUE,
    fill.scale = tmap::tm_scale_categorical(values = "tableau.20"),
    col.scale = tmap::tm_scale_categorical(values = "tableau.20"),
    fill.legend = tmap::tm_legend(show = FALSE),
    col.legend = tmap::tm_legend(show = FALSE)
    # group = "Regions", group.control = "check"
  ) +
  tmap::tm_shape(states, name = "States") +
  tmap::tm_polygons(
    fill = "State", col = "State",
    fill_alpha = 0.2, popup.vars = TRUE,
    fill.scale = tmap::tm_scale_categorical(values = "tableau.classic20"),
    col.scale = tmap::tm_scale_categorical(values = "tableau.classic20"),
    fill.legend = tmap::tm_legend(show = FALSE),
    col.legend = tmap::tm_legend(show = FALSE)
    # group = "Regions", group.control = "check"
  ) +
  tmap::tm_shape(proj_donors, name = paste0("Colombia-856", " Donors")) +
  tmap::tm_polygons(
    fill = "Donor mean SC weight", col = "Donor mean SC weight",
    fill_alpha = 0.4, popup.vars = TRUE,
    fill.scale = tmap::tm_scale_categorical(values = "viridis.magma"),
    col.scale = tmap::tm_scale_categorical(values = "viridis.magma"),
    lwd = 4
  )
ml <- tmap::tmap_leaflet(m) |>
  leaflet::hideGroup("States")
ml
library(mapview)
mapview(project_856)

tmap::tmap_save(m, file.path("leaf-maps/test.html"),
  width = "800px", height = "600px"
)
knitr::include_url(file.path("..", file.path("leaf-maps/test.html")),
  height = "500"
)

library(tmap)
map <- tm_shape(World) + tm_polygons()
tmap_leaflet(map, show = TRUE)
