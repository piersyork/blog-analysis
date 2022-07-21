

lsoa_bookies <- as.data.table(bookies)[, .(n_bookies = .N), by = lsoa][as.data.table(lsoa_map)[, .(lsoa = LSOA11CD, lsoa_name = LSOA11NM, geometry)], on = "lsoa"]

lsoa_bookies[is.na(n_bookies), n_bookies := 0]

lsoa_bookies[, .(n_bookies = sum(n_bookies)), by = area_name][order(-n_bookies)]

areas <- readr::read_csv("~/Downloads/Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_(December_2020)_Lookup_in_England_and_Wales.csv") |>
  select(lsoa = LSOA11CD, area_code = LAD20CD, area_name = LAD20NM, region = RGN20NM) |>
  distinct() |>
  as.data.table()

areas[
  lsoa_bookies, on = "lsoa"
][
  , .(n_bookies = sum(n_bookies)), by = area_code
][
  as.data.table(uk_map), on = "area_code"
] |>
  st_as_sf() |>
  qtm(fill = "n_bookies")


lsoa_bookies[order(-n_bookies), head(.SD)]

tmap_mode("view")
lsoa_bookies |>
  st_as_sf() |>
  qtm(fill = "n_bookies", borders = NULL)
