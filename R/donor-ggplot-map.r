#' @title donor_ggplot_map
#' @param project_area sf object of the project area
#' @param donors sf object of the donors
#' @param country sf object of the country
#' @param eco_regions sf object of the ecoregions
#' @param hs_df data frame of the hillshade
#' @return ggplot object
donor_ggplot_map <- function(
    project_area,
    donors,
    country,
    eco_regions,
    hs_df,
    project_name = "Colombia 856",
    w_limits = c(0, 0.5),
    w_breaks = seq(0, 0.5, by = 0.1)) {
    # get the bounding box for setting the map extent
    bbox <- sf::st_bbox(country)
    # get the project centroid
    project_area_centroid <- project_area |>
        sf::st_centroid()
    # get the centroids.
    donr_centroids <- donors |>
        sf::st_centroid()

    # plot the map
    ggplot2::ggplot() +
        # hillshade
        ggplot2::geom_raster(
            data = hs_df,
            ggplot2::aes(x = x, y = y, fill = hillshade),
            interpolate = TRUE,
            alpha = 0.8
        ) +
        ggplot2::scale_fill_gradientn(
            colours = c(hcl.colors(200, "Grays"), rep("white", 50)),
            guide = FALSE,
            trans = scales::pseudo_log_trans(sigma = 0.2, base = 100)
        ) +
        ggnewscale::new_scale_fill() +
        # eco regions
        ggplot2::geom_sf(
            data = eco_regions,
            mapping = ggplot2::aes(fill = select_eco_name), # type
            alpha = 0.5,
            lwd = 0.2,
            colour = "grey10"
        ) +
        ggplot2::scale_fill_brewer(
            palette = "Dark2",
            labels = function(breaks) {
                breaks[is.na(breaks)] <- "other"
                breaks
            },
        ) +
        ggplot2::labs(fill = "Ecoregion") +
        new_scale_fill() +
        # map donors
        ggplot2::geom_sf(
            data = donr_centroids,
            mapping = ggplot2::aes(
                fill = `Donor mean SC weight`,
                size = `Donor mean SC weight`
            ),
            colour = "black",
            pch = 21
        ) +
        ggplot2::scale_fill_gradientn(
            colours = c(
                "white",
                hcl.colors(100, "inferno")
            ),
            limits = w_limits,
            breaks = w_breaks
        ) +
        ggplot2::scale_size_continuous(
            range = c(2, 5),
            limits = w_limits,
            breaks = w_breaks
        ) +
        ggplot2::guides(
            fill = ggplot2::guide_legend(order = 1),
            size = ggplot2::guide_legend(order = 1),
            colour = ggplot2::guide_legend(order = 1)
        ) +
        ggnewscale::new_scale_fill() +
        # project area
        ggplot2::geom_sf(
            data = project_area_centroid,
            mapping = aes(fill = sprintf("Project Area (%s)", project_name)),
            colour = "white",
            size = 5,
            pch = 24
        ) +
        ggplot2::scale_fill_manual(values = "#ff0000") +
        ggplot2::labs(fill = "") +
        ggplot2::guides(fill = guide_legend(order = 1)) +
        ggplot2::theme_minimal() +
        ggspatial::annotation_scale() +
        ggspatial::annotation_north_arrow(
            pad_x = ggplot2::unit(-0.5, "cm"),
            pad_y = ggplot2::unit(0.8, "cm"),
            style = ggspatial::north_arrow_minimal(
                line_width = 2,
                text_col = NA
            )
        ) +
        ggplot2::coord_sf(
            xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]),
            expand = FALSE
        ) +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(
                fill = "#ffffff", colour = NA
            ),
            axis.title = ggplot2::element_blank()
        )
}

#' @title generate hillshade for a country
#' @param country sf object of the country
#' @param buff_dist numeric; buffer distance in meters to expand search area
#' @return data frame of the hillshade
generate_hillshade <- function(country, buff_dist = 100) {
    world <- rnaturalearth::ne_countries(
        scale = "medium", returnclass = "sf"
    ) |>
        sf::st_transform(4326) |>
        sf::st_make_valid() |>
        sf::st_filter(sf::st_buffer(country, buff_dist)) |>
        sf::st_make_valid() |>
        dplyr::summarise() |>
        sfheaders::sf_remove_holes()

    # generate a hillshade for the map
    dtm_ext <- sf::st_buffer(country, 2e5)

    dtm <- ezwarp::ezwarp(dem_src(), dtm_ext, 0.01, cutline = world)
    hillshade <- terra::shade(
        terra::terrain(dtm, "slope", unit = "radians"),
        terra::terrain(dtm, "aspect", unit = "radians"),
        angle = 30, direction = 300
    )
    hs_df <- as.data.frame(hillshade, xy = TRUE)

    return(hs_df)
}

#' @title generate ecoregions for a country
#' @param project_area sf object of the project area
#' @param donors sf object of the donors
#' @param country sf object of the country
#' @return sf object of the ecoregions
#' @details also adds a new column to identify ecoregions that intersect
#' weighted donors.
generate_eco_regions <- function(project_area, donors, country) {
    # get the ecoregions that intersect with Colombia and assign a dominant veg type
    eco_reg <- eco_regions(country) |>
        wk::wk_flatten() |>
        dplyr::mutate(
            id = dplyr::row_number()
        )

    donor_wgs <- sf::st_transform(
        filter(donors, zero == "non-zero"),
        sf::st_crs(eco_reg)
    ) |>
        dplyr::select(geometry) |>
        dplyr::bind_rows(select(
            sf::st_transform(project_area, st_crs(eco_reg)), geometry
        ))


    eco_reg2 <- eco_reg |>
        sf::st_filter(donor_wgs)

    eco_reg |>
        dplyr::mutate(
            contains_donor = dplyr::case_when(
                id %in% eco_reg2$id ~ TRUE,
                TRUE ~ FALSE
            ), select_eco_name = case_when(
                contains_donor == TRUE ~ ECO_NAME,
                TRUE ~ NA_character_
            )
        ) |>
        rmapshaper::ms_simplify()
}
