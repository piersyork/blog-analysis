library(dplyr)
library(sf)
library(tmap)
library(ggplot2)

options(box.path = getwd())
box::use(functions/theme[theme_piers])

## 1. Load data produced by 0-create-data.R ##
uk_map <- readRDS("betting-shops/data/uk_local_authority_map.rds") |>
  select(area_code, area_name, geometry)
bookies_in_uk <- readRDS("betting-shops/data/bookies_in_uk.rds") |>
  select(area_code, area_name, geometry)


uk_pop <- readxl::read_xls("betting-shops/data/uk-population-estimates-mid-2020.xls", sheet = 7, range = "A8:D426") |>
  select(area_code = Code, population = `All ages`)

income <- readxl::read_xls("~/Downloads/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls", sheet = 7, skip = 4) |>
  janitor::clean_names() |>
  group_by(local_authority_code) |>
  summarise(income = mean(net_annual_income_after_housing_costs)) |>
  rename(area_code = local_authority_code)

regions <- readxl::read_xls("~/Downloads/incomeestimatesforsmallareasdatasetfinancialyearending20181.xls", sheet = 7, skip = 4) |>
  janitor::clean_names() |>
  select(area_code = local_authority_code, region_name) |>
  distinct() |>
  mutate(region_name = case_when(region_name %in% c("North East", "Yorkshire", "North West") ~ "North",
                                 region_name %in% c("East Midla", "West Midla", "East") ~ "Midlands & East",
                                 region_name %in% c("South East", "South West") ~ "South",
                                 TRUE ~ region_name))

regions$region_name |> unique()

deprivation <- readxl::read_xlsx("~/Downloads/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx",
                                 sheet = 2) |>
  select(area_code = `Local Authority District code (2019)`,
         deprivation = `IMD - Average score`,
         imd_rank = `IMD - Rank of average score`)


##



bookies_by_la <- bookies_in_uk |>
  as_tibble() |>
  count(area_name, area_code, name = "n_bookies") |>
  left_join(uk_pop, by = "area_code") |>
  left_join(deprivation, by = "area_code") |>
  left_join(regions, by = "area_code")


bookies_by_la_map <- bookies_by_la |>
  right_join(uk_map) |>
  mutate(n_bookies = ifelse(is.na(n_bookies), 0, n_bookies),
         bookies_per_100k = ifelse(n_bookies == 0, 0.00, n_bookies / population * 1e5),
         people_per_bookies = ifelse(n_bookies == 0, 0.00, population / n_bookies)) |>
  st_as_sf()


bookies_by_la_map |>
  # filter(grepl("^E09", area_code)) |>
  ggplot(aes(fill = bookies_per_100k)) +
  geom_sf(size = 0.05) +
  theme_void() +
  scale_fill_distiller(palette = "Reds", direction = 1, trans = "log10", na.value = "#FEE0D2")

tmap_mode("view")
plot1 <- (
  bookies_by_la_map |>
    tm_shape() +
    tm_polygons(col = "bookies_per_100k", style = "fisher", legend.hist = TRUE, text = "bookies_per_100k") +
    tm_layout(legend.outside = TRUE)
) |>
  tmap_leaflet()

plot1 |>
  leaflet::removeTiles(1)

tm_dots()

tmap_mode("plot")
qtm(bookies_in_uk, dots.size = 0.002, dots.alpha = 0.5)


bookies_by_la_map |>
  as_tibble() |>
  select(area_name, n_bookies, population, bookies_per_100k, people_per_bookies) |>
  arrange(desc(bookies_per_100k))

# saveRDS(bookies_in_uk, "~/Documents/GitHub/piersyork.github.io/blog/_data/bookies-location.rds")

bookies_by_la_map |>
  as_tibble() |>
  select(area_name, deprivation, bookies_per_100k, population, region_name) |>
  na.omit() |>
  mutate(label = ifelse(area_name %in% c("City of London"), area_name, NA)) |>
  ggplot(aes(deprivation, bookies_per_100k, size = population, colour = region_name)) +
  geom_point(alpha = 0.3) +
  ggrepel::geom_text_repel(aes(label = label), show.legend = FALSE,
                           size = 4, point.padding = 0.8, box.padding = 2, hjust = 1,
                           arrow = arrow(angle = 30, length = unit(0.1, "inches"))) +
  # geom_text(aes(label = "City of London", y = 120, x = 15), inherit.aes = FALSE) +
  scale_y_log10(limits = c(NA, 1000)) +
  scale_size_area(max_size = 10, labels = scales::label_number(big.mark = ", ")) +
  owidR::scale_colour_owid() +
  geom_smooth(aes(deprivation, bookies_per_100k), method = "lm", colour = "#EC0175", inherit.aes = FALSE) +
  theme_piers() +
  labs(title = "Areas with higher deprivation see a higher number of bookies per 100k people",
       x = "Index of Deprivation", y = "Bookies per 100k people", colour = "Region", size = "Population")



bookies_by_la_map |>
  as_tibble() |>
  filter(area_name != "City of London") |>
  lm(n_bookies ~ deprivation, data = _) |>
  summary()




bookies_by_la_map |>
  as_tibble() |>
  group_by(region_name) |>
  summarise(bookies_per_100k = mean(bookies_per_100k))

bookies_by_la_map |>
  as_tibble() |>
  skimr::skim()

bookies_in_uk$OBJECTID |>
  as.character() |>
  skimr::skim()








