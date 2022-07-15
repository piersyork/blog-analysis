library(sf)
library(osmdata)
library(dplyr)

## 1. Get map data of UK Local Authority Districts from the geoportal API. 20m resolution ##

uk_map <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2021_GB_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |>
  rename(area_code = LAD21CD, area_name = LAD21NM)

# Save as .rds
saveRDS(uk_map, file = "betting-shops/data/uk_local_authority_map.rds")


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

# Filter out points that are not contained within a local authority district
bookies_in_uk <- st_join(uk_bookies, uk_map, st_within) |>
  filter(!is.na(area_name))

# Save as .rds
saveRDS(bookies_in_uk, "betting-shops/data/bookies_in_uk.rds")




