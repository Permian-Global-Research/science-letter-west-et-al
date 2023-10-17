renv::restore()
# West packages
library(plyr)
library(Synth) # synthetic control package
library(MSCMT) # for a more robust optimization procedure
library(foreign)

# additional packages are loaded from files in 'R/' directly.

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
    col_eco_regions,
    hillshade
)

ggplot2::ggsave("figures/colombia_856_donors.png", p,
    width = 10, height = 10, bg = "white"
)
