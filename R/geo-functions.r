#' @title get administritive outlines for a country
#' @description using the geoBoundaires API,
#' download the geojson outline of a country
#' @param country character vector of country names
#' @param admin_level character vector of admin levels to download
#' @return sf object of the outlines
#' @details check out the documentation for the geoboundaries API at:
#' geoBoundaries.org
#'
geo_bounds <- function(country, admin_level = c("ADM0", "ADM1", "ADM2")) {
  country <- countrycode::countrycode(country,
    origin = "country.name",
    destination = "iso3c"
  )
  url <- paste(
    "https://www.geoboundaries.org/api/current/gbOpen/",
    country, admin_level[1],
    sep = "/"
  )
  get <- httr::GET(url)
  cont <- httr::content(get, as = "parsed")
  area <- sf::read_sf(cont$gjDownloadURL)
  return(area)
}

#' @title get administritive outlines for a country and the country outline
#' @param country character;  a country
#' @param out_path character; path to save the output
#' @param overwrite logical; overwrite the output if it already exists
#' @return list of sf objects
states_and_country <- function(
    country,
    out_path = sprintf("geo-data/%s_regions.gpkg", tolower(country)),
    overwrite = FALSE) {
  if (!file.exists(out_path) || isTRUE(overwrite)) {
    states_spat <- geo_bounds(country, "ADM1")

    country_spat <- sf::st_union(states_spat) |>
      sf::st_cast("POLYGON") |>
      sf::st_as_sf()
    country_spat <- country_spat |>
      dplyr::mutate(area = sf::st_area(country_spat)) |>
      dplyr::arrange(dplyr::desc(area)) |>
      dplyr::slice(1)

    states_spat <- states_spat |>
      dplyr::filter(sf::st_intersects(states_spat,
        country_spat,
        sparse = FALSE
      )[, 1])

    sf::write_sf(country_spat, out_path,
      delete_dsn = TRUE, layer = "country"
    )
    sf::write_sf(states_spat, out_path, layer = "states")
  } else {
    country_spat <- sf::read_sf(out_path, layer = "country")
    states_spat <- sf::read_sf(out_path, layer = "states")
    cli::cli_inform(
      c(
        "v" = "{out_path} aready exists",
        "i" = "loading from source."
      )
    )
  }

  return(list(
    country = sf::st_make_valid(country_spat),
    states = sf::st_make_valid(states_spat)
  ))
}

#' @title get the ecoregions that intersect with a region
#' @param region sf object of the region to intersect with
#' @param eco_region_dir character; path of the directory to save the output
#' @param overwrite logical; overwrite the output if it already exists
#' @return sf object of the ecoregions that intersect with the region
#'
eco_regions <- function(
    region,
    eco_region_dir = "geo-data",
    overwrite = FALSE) {
  region <- sf::st_transform(region, 4326)
  safe_create(eco_region_dir)
  er_path <- file.path(eco_region_dir, "Ecoregions2017.gpkg")
  if (!file.exists(er_path) || isTRUE(overwrite)) {
    eco_regions <- sf::read_sf(paste0(
      "/vsizip/vsicurl/",
      "https://storage.googleapis.com/teow2016/Ecoregions2017.zip/",
      "Ecoregions2017.shp"
    )) |>
      sf::st_make_valid()

    sf::write_sf(
      eco_regions,
      er_path,
      delete_dsn = TRUE
    )
  } else {
    eco_regions <- sf::read_sf(
      er_path
    )
  }

  eco_regions |>
    dplyr::filter(
      sf::st_intersects(eco_regions,
        region,
        sparse = FALSE
      )[, 1]
    ) |>
    sf::st_intersection(region) |>
    sf::st_make_valid()
}


#' @title get a XML source for Amazon elevation data
#' @return character; XML source for Amazon elevation data
dem_src <- function() {
  '<GDAL_WMS>
  <Service name="TMS">
    <ServerUrl>https://s3.amazonaws.com/elevation-tiles-prod/geotiff/${z}/${x}/${y}.tif</ServerUrl>
  </Service>
  <DataWindow>
    <UpperLeftX>-20037508.34</UpperLeftX>
    <UpperLeftY>20037508.34</UpperLeftY>
    <LowerRightX>20037508.34</LowerRightX>
    <LowerRightY>-20037508.34</LowerRightY>
    <TileLevel>14</TileLevel>
    <TileCountX>1</TileCountX>
    <TileCountY>1</TileCountY>
    <YOrigin>top</YOrigin>
  </DataWindow>
  <Projection>EPSG:3857</Projection>
  <BlockSizeX>512</BlockSizeX>
  <BlockSizeY>512</BlockSizeY>
  <BandsCount>1</BandsCount>
  <DataType>Int16</DataType>
  <ZeroBlockHttpCodes>403,404</ZeroBlockHttpCodes>
  <DataValues>
    <NoData>-32768</NoData>
  </DataValues>
  <Cache/>
</GDAL_WMS>'
}

#' @title extract the project area from the West et al. (2023) data
#' @param west_spatial sf object of the West et al. (2023) data
#' @param project_id numeric; ID of the project to extract
#' @return sf object of the project area
west_extract_proj_area <- function(west_spatial, project_id) {
  west_spatial |>
    dplyr::filter(REDD == "1" & ID == project_id) |>
    dplyr::select(!dplyr::any_of("fid"))
}

#' @title read the spatial data from West et al. (2023)
#' @param filename character; name of the file to read
#' @return sf object of the spatial data
read_west_spatial <- function(filename) {
  sf::read_sf(
    dp(filename, "west-data/West et al. (2023) data/Shapefiles/")
  ) |>
    sf::st_make_valid()
}
