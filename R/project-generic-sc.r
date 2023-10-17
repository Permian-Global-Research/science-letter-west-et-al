donor_map_wrapper <- function(sc_data, shp_file, country, proj_id, proj_start, diff_cutoff = 0.2) {
    dbf_file <- paste0(tools::file_path_sans_ext(shp_file), ".dbf")

    proj_sc_data <- sc_project_data(
        filepath = dp(sc_data),
        dbf_file = dp(
            dbf_file,
            "west-data/West et al. (2023) data/Shapefiles"
        ),
        project_id = proj_id,
        project_start_date = proj_start,
        project_name = paste0(country, proj_id),
        cutoff = diff_cutoff,
        drop_controls = c()
    )

    proj_donor_list <- synth_control_run(
        project_data = proj_sc_data,
        donor_prop = 1
    )

    # read the spatial data for DRC from West
    sc_drc_sp <- read_west_spatial(shp_file)

    # extract the project area
    project_area <- west_extract_proj_area(sc_drc_sp, proj_id)

    # locate the donors used in the SC
    donors <- locate_donors(proj_donor_list, sc_drc_sp)

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
}
