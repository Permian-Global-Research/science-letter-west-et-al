# West packages
library(plyr)
library(Synth) # synthetic control package
library(MSCMT) # for a more robust optimization procedure
library(foreign)

# Added packages
# library(furrr) # not really needed now we're not running the full simulation.
# library(purrr)
# library(dplyr)
# library(sf)
# library(mapview) # turn off after dev.
# library(ggplot2)
# library(ggnewscale)
# library(ggspatial)
# library(terra)
# library(ezwarp) # see https://permian-global-research.github.io/ezwarp/
# library(rnaturalearth)

# Source all R files in the R directory
funcs <- list.files("R", pattern = ".r$", full.names = TRUE) |>
    purrr::walk(source)

# Set up out paths:
safe_create("geo-data")
safe_create("figures")

# Extract data
extract_west_data()

# - Colombia Project 856 -------------------------------------------------------

# get data for the Colombia 856 project
col_856_data <- sc_project_data(
    filepath = dp("Colombia_synth_control_data.csv"),
    dbf_file = dp(
        "Colombia_polygons.dbf",
        "west-data/West et al. (2023) data/Shapefiles"
    ),
    project_id = 856,
    project_start_date = 2011,
    project_name = "Colombia 856",
    cutoff = 0.2,
    drop_controls = c(8851)
)

col_856_donor_list <- synth_control_run(
    project_data = col_856_data,
    donor_prop = 1
)

# read the spatial data for Colombia from West
sc_columbia_sp <- read_west_spatial("Colombia_polygons.shp")

# extract the project area
project_area <- west_extract_proj_area(sc_columbia_sp, 856)

# locate the donors used in the SC
donors <- locate_donors(col_856_donor_list, sc_columbia_sp)

# extract colombia outline and states.
colombia_regions <- states_and_country("colombia")
# get the hillshade
hillshade <- generate_hillshade(colombia_regions$country)
# get the eco regions
col_eco_regions <- generate_eco_regions(
    project_area,
    donors,
    colombia_regions$country
)

p <- donor_ggplot_map(
    project_area,
    donors,
    colombia_regions$country,
    hillshade,
    col_eco_regions
)

ggsave("figures/colombia_856_donors.png", p,
    width = 10, height = 10, bg = "white"
)

# - DRC Project 1359 -----------------------------------------------------------

# get data for the DRC 1359 project
drc_1359_data <- sc_project_data(
    filepath = dp("DRC_synth_control_data.csv"),
    dbf_file = dp(
        "DRC_polygons.dbf",
        "west-data/West et al. (2023) data/Shapefiles"
    ),
    project_id = 1359,
    project_start_date = 2009,
    project_name = "DRC 1359",
    cutoff = 0.2,
    drop_controls = c()
)

drc_1359_donor_list <- synth_control_run(
    project_data = drc_1359_data,
    donor_prop = 1
)

# read the spatial data for DRC from West
sc_drc_sp <- read_west_spatial("DRC_polygons.shp")

# extract the project area
project_area <- west_extract_proj_area(sc_drc_sp, 1359)

# locate the donors used in the SC
donors <- locate_donors(drc_1359_donor_list, sc_drc_sp)

# extract DRC outline and states.
drc_regions <- states_and_country("DRC")

# get the hillshade
hillshade <- generate_hillshade(drc_regions$country)

# get the eco regions
drc_eco_regions <- generate_eco_regions(
    project_area,
    donors,
    drc_regions$country
)

p_drc <- donor_ggplot_map(
    project_area,
    donors,
    drc_regions$country,
    drc_eco_regions,
    hillshade,
    project_name = "DRC 1359",
    w_limits = c(0, 1),
    w_breaks = seq(0, 1, by = 0.25)
)

p_drc

mapview(donors, zcol = "Donor mean SC weight") +
    mapview(project_area, col.region = "red") +
    mapview(drc_eco_regions, zcol = "ECO_NAME")
# ----- no longer used -----

# syn_weight_list <- purrr::safely(function(data, i) {
#     synth_sym_find_donors(
#         project_data = data,
#         i = i,
#         donor_prop = 0.8
#     )$s_weights
# })

# This carries out the full simulation as in the paper.
# plan(multisession, workers = 18)
# synth_donors <- 1:100 |>
#     furrr::future_map(~ syn_weight_list(col_856_data, i = .x),
#         .progress = TRUE
#     ) |>
#     purrr::map(~ .x$result) |>
#     dplyr::bind_rows() |>
#     dplyr::group_by(poly_id) |>
#     dplyr::summarise(
#         w.weight = mean(w.weight)
#     )
