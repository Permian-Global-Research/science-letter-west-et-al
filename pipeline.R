# R script to produce the map presented in the main letter in response to
# West, et al. 2023. Focus here is on Colombia project 856.

# to generate reproducible R package versions.
renv::restore()
# Source all R files in the R directory and load necessary packages.
source("R/load-all.r")

# Set up out paths:
safe_create("geo-data")
safe_create("figures")

# Extract data included in the repo - downloaded from:
# https://dataverse.nl/dataset.xhtml?persistentId=doi:10.34894/IQC9LM
# Note this may fail if 7z is not installed on your system.
extract_west_data()

# - Colombia Project 856 -------------------------------------------------------
# run the synthetic control algorithm
col_856_donor_list <- sc_wrap_recursive(
  filepath = dp("Colombia_synth_control_data.csv"),
  dbf_file = dp(
    "Colombia_polygons.dbf",
    "west-data/West et al. (2023) data/Shapefiles"
  ),
  project_id = 856,
  project_start_date = 2011,
  project_name = "Colombia 856",
  drop_controls = c(8851),
  cutoff_start = 0.2
)

dc <- count_donors(col_856_donor_list)
cli::cli_alert_info(
  "{dc$n_weighted} donors used in the SC out of a pool of {dc$n_pot}"
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

ggplot2::ggsave("letter/figures/colombia_856_donors.png", p,
  width = 10, height = 10, bg = "white"
)
