Betting Shops Analysis ReadMe
================

# Overview

This blog looks at where betting shops are located around the UK to
calculate the number of bookmakers per million people by local
authority. Comparing this data to the Index of Multiple Deprivation
shows how more deprived areas see a higher number of bookies per person.

`scripts/0-create-data.R`

# Data on bookmakers

The data on the locations of bookmakers comes from Open Street Map.
Using the `osmdata` r package, I collected the locations of each
bookmaker which Open Street Map has data for.

``` r
library(osmdata)
library(sf)
# Get all points for england and wales
bookmakers <- opq("england") |> # includes data for wales
  add_osm_feature(key = "shop", value = "bookmaker") |>
  osmdata_sf()

# Get all points for scotland
bookmakers_scotland <- opq("scotland") |>
  add_osm_feature("shop", "bookmaker") |>
  osmdata_sf()
```

Once this data was collected, I realised there was a problem of
massively overestimating the number of bookies. This was caused by the
point data representing the 4 corners of the shop building. So instead
of having one point per bookmaker, some shops had 4 or more. This is
solved by finding all of these points and replacing them with on point
in the center of the building. The data from england, wales and scotland
is then combined into one dataset.

``` r
# function to remove duplicate points for the same bookmaker
remove_dups <- function(x) {
  x$osm_points |>
    st_join(x$osm_polygons |> mutate(building_poly = "yes"), st_intersects) |>
    filter(is.na(building_poly)) |>
    bind_rows(st_centroid(x$osm_polygons))
}

# combine data for Scotland, England and Wales
bookies <- remove_dups(bookmakers_scotland) |>
  bind_rows(remove_dups(bookmakers)) |>
  distinct() |>
  select(name, addr.postcode) |>
  st_transform(27700)
```

For this data to be meaningful for analysis, we need to know which area
each of these points is in. I do this by sending the latitude and
longitude of each point to the postcodes.io api, which returns
information on the local authority, lower super output area, region and
country.

``` r
# get area data from postcodes.io
get_area_data <- function(x) {
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

# Fetch the data and filter out any points which aren't in a lower super output area
bookies <- bookies |>
  get_area_data() |>
  filter(!is.na(lsoa))
```

This leaves us with a point coordinate for each bookmaker in the UK
along with variables for what area it is located.

# Merging with data on deprivation

For data on deprivation I used the English Index of Deprivation from the
Ministry of Housing Communities and Local Government (now the Department
for Leveling Up Housing and Communities). They provide a dataset of the
average deprivation score by local authority. I also use population
estimates from the ONS to calculate the number of bookies per person

``` r
library(dplyr)
library(data.table)

# Get data for deprivation
deprivation_la <- readxl::read_xlsx("betting-shops/data/deprivation-lad.xlsx",
                                 sheet = 2) |>
  select(area_code = `Local Authority District code (2019)`,
         deprivation = `IMD - Average score`,
         imd_rank = `IMD - Rank of average score`) |>
  as.data.table()

uk_pop <- readxl::read_xls("betting-shops/data/uk-population-estimates-mid-2020.xls", sheet = 7, range = "A8:D426") |>
  select(area_code = Code, population = `All ages`) |>
  as.data.table()
```

Using the `data.table` package I merge these datasets together and
calculate the number of bookies per million people by local authority.

``` r
la_n_bookies <- bookies[, .(n_bookies = .N), by = .(region, area_name, area_code)][
  as.data.table(uk_pop), on = "area_code", nomatch = 0
][
  deprivation_la, on = "area_code"
]
la_n_bookies[, bookies_per_million := round(n_bookies / population * 1e6, 3)]
la_n_bookies[, region_name := case_when(region %in% c("North East", "North West", "Yorkshire and The Humber") ~ "North",
                                        region %in% c("East Midlands", "West Midlands", "East of England") ~ "Midlands and East",
                                        region %in% c("South West", "South East") ~ "South",
                                        TRUE ~ region)]
```

This data is available in `public/betting-shops-deprivation.csv`. It is
subject to the [Open Street Map
license](https://www.openstreetmap.org/copyright).