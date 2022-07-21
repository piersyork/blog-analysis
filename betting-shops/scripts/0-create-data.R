library(sf)
library(osmdata)
library(dplyr)

## 1. Get map data of UK Local Authority Districts from the geoportal API. 20m resolution ##
uk_map <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2021_GB_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  rename(area_code = LAD21CD, area_name = LAD21NM)

# Save as .rds
saveRDS(uk_map, file = "betting-shops/data/uk_local_authority_map.rds")

areas <- readr::read_csv("~/Downloads/Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_(December_2020)_Lookup_in_England_and_Wales.csv") |>
  select(lsoa = LSOA11CD, area_code = LAD20CD, area_name = LAD20NM, region = RGN20NM) |>
  distinct() |>
  as.data.table()

## 2. Get data on deprivation
deprivation_lsoa <- readxl::read_xlsx("~/Downloads/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet = 2) |>
  janitor::clean_names()


## 2. Get data for locations of betting shops from Open Street Map ##

# Get all points for england and wales
bookmakers <- opq("england") |> # includes data for wales
  add_osm_feature(key = "shop", value = "bookmaker") |>
  osmdata_sf()

# Get all points for scotland
bookmakers_scotland <- opq("scotland") |>
  add_osm_feature("shop", "bookmaker") |>
  osmdata_sf()

# osmdata returns some points that are the 4+ corners of a building polygon, increasing the number of points making
# up a single betting shop. This function solves that problem by transforming those polygons into single points.
remove_dups <- function(x) {
  x$osm_points |>
    st_join(x$osm_polygons |> mutate(building_poly = "yes"), st_intersects) |>
    filter(is.na(building_poly)) |>
    bind_rows(st_centroid(x$osm_polygons))
}

# combine data for Scotland, England and Wales
uk_bookies <- remove_dups(bookmakers_scotland) |>
  bind_rows(remove_dups(bookmakers)) |>
  distinct() |>
  select(name, addr.postcode) |>
  st_transform(st_crs(uk_map))

# get lsoa codes from postcodes.io
get_lsoa <- function(x) {
  lons <- unlist(purrr::map(x$geometry, 1))
  lats <- unlist(purrr::map(x$geometry, 2))

  lsoas <- vector(length = length(lons))
  lsoa_names <- vector(length = length(lons))
  lads <- vector(length = length(lons))
  lad_names <- vector(length = length(lons))
  country <- vector(length = length(lons))
  region <- vector(length = length(lons))

  pb <- txtProgressBar(style = 3)
  pb_add <- seq(1/length(lons), 1, 1/length(lons)) # amount to add each loop

  for (i in 1:length(lons)) {
    api_return <- tryCatch(
      jsonlite::fromJSON(glue::glue("https://api.postcodes.io/postcodes?lon={lons[i]}&lat={lats[i]}&limit=1"))$result,
      error = function(e) return(NULL)
    )
    if (is.null(api_return)) {
      lsoas[i] <- NA
      lsoa_names[i] <- NA
      lads[i] <- NA
      lad_names[i] <- NA
      country[i] <- NA
      region[i] <- NA
    } else {
      lsoas[i] <- api_return$codes$lsoa
      lsoa_names[i] <- api_return$lsoa
      lads[i] <- api_return$codes$admin_district
      lad_names[i] <- api_return$admin_district
      country[i] <- api_return$country
      region[i] <- api_return$region
    }
    setTxtProgressBar(pb, pb_add[i])
  }

  out <- x |>
    mutate(
      lsoa = lsoas,
      lsoa_name = lsoa_names,
      area_code = lads,
      area_name = lad_names,
      country = country,
      region = region
    )


  return(out)
}

# Get lsoa data (lower layer super output area) and filter out missing (to remove bookies outside UK)
bookies_in_uk <- uk_bookies |>
  get_lsoa() |>
  filter(!is.na(lsoa))

# Save as .rds
saveRDS(bookies_in_uk, "betting-shops/data/bookies_in_uk.rds")


## 3. Lower layer super output area
lsoa_map <- st_read("~/Downloads/Lower_Layer_Super_Output_Areas_(December_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.geojson")

bookies <- bookies_in_uk |>
  left_join(deprivation, by = c("lsoa" = "lsoa_code"))

lsoa_bookies <- as.data.table(bookies)[, .(n_bookies = .N), by = lsoa][as.data.table(lsoa_map)[, .(lsoa = LSOA11CD, lsoa_name = LSOA11NM, geometry)], on = "lsoa"]

lsoa_bookies[is.na(n_bookies), n_bookies := 0]

lsoa_bookies <- areas[lsoa_bookies, on = "lsoa"]

saveRDS(lsoa_bookies, file = "betting-shops/data/lsoa_bookies.rds")





