library(dplyr)
library(data.table)
library(ggplot2)

options(box.path = getwd())
box::use(functions/theme[...])


#############################################################################################################################
# Import data

## Get data for deprivation
deprivation_lsoa <- readxl::read_xlsx("betting-shops/data/deprivation-lsoa.xlsx", sheet = 2) |>
  select(lsoa = `LSOA code (2011)`, lsoa_name = `LSOA name (2011)`,
         area_code = `Local Authority District code (2019)`, area_name = `Local Authority District name (2019)`,
         imd_rank = `Index of Multiple Deprivation (IMD) Rank`, imd_decile = `Index of Multiple Deprivation (IMD) Decile`) |>
  as.data.table()

deprivation_la <- readxl::read_xlsx("betting-shops/data/deprivation-lad.xlsx",
                                 sheet = 2) |>
  select(area_code = `Local Authority District code (2019)`,
         deprivation = `IMD - Average score`,
         imd_rank = `IMD - Rank of average score`) |>
  as.data.table()

## Get data for population
uk_pop <- readxl::read_xls("betting-shops/data/uk-population-estimates-mid-2020.xls", sheet = 7, range = "A8:D426") |>
  select(area_code = Code, population = `All ages`) |>
  as.data.table()

## Import bookies data
bookies <- readRDS("betting-shops/data/bookies_in_uk.rds") |>
  as.data.table()

bookies |>
  sf::st_as_sf()


la_n_bookies <- bookies[, .(n_bookies = .N), by = .(region, area_name, area_code)][
  as.data.table(uk_pop), on = "area_code", nomatch = 0
][
  deprivation_la, on = "area_code"
][
  # as.data.table(regions), on = "area_code", nomatch = 0
]
la_n_bookies[, bookies_per_million := round(n_bookies / population * 1e6, 3)]
la_n_bookies[, region_name := case_when(region %in% c("North East", "North West", "Yorkshire and The Humber") ~ "North",
                                        region %in% c("East Midlands", "West Midlands", "East of England") ~ "Midlands and East",
                                        region %in% c("South West", "South East") ~ "South",
                                        TRUE ~ region)]


#############################################################################################################################
## Public data: number of bookies by local authority district plus deprivation score
readr::write_csv(la_n_bookies, "betting-shops/public/betting-shops-deprivation.csv")
#############################################################################################################################

## Create chart comparing deprivation to bookies per mil
la_n_bookies[!is.na(region_name)] |>
  ggplot(aes(deprivation, bookies_per_million, size = population, colour = region_name, fill = region_name)) +
  geom_point(shape = 21) +
  annotate("text", label = "City of London", y = 1200, x = 25.5, colour = "#393A76") +
  annotate("curve", x = 21.5, y = 1200, xend = 15.1, yend = 2500,
           curvature = -0.25, arrow = arrow(length = unit(0.5, "cm")), colour = "#393A76") +
  scale_y_log10() +
  scale_size_area(max_size = 6, labels = scales::label_number(big.mark = ",")) +
  scale_colour_piers() +
  scale_fill_piers(alpha = 0.5, guide = "none") +
  geom_smooth(aes(deprivation, bookies_per_million), method = "lm", colour = "#044a6b", inherit.aes = FALSE) +
  theme_piers() +
  labs(title = "Local authorities with higher deprivation see a higher \nnumber of bookies per person",
       subtitle = "Bookies per million people vs Index of Deprivation",
       x = "Index of Multiple Deprivation", y = "Bookies per million people", colour = "Region", size = "Population",
       caption = "Source: Open Street Maps; Ministry of Housing, Communties and Local Government") +
  guides(colour = guide_legend(override.aes = list(size = 4, shape = 16, alpha = 0.5))) +
  theme(plot.caption = element_text(hjust = 0, size = 10))

## n_bookies by imd decile
lsoa_bookies <- bookies[
  , .(n_bookies = .N), by = lsoa
][
  deprivation_lsoa, on = "lsoa"
]
lsoa_bookies[is.na(n_bookies), n_bookies := 0]

lsoa_bookies[order(imd_decile), .(n_bookies = sum(n_bookies)), by = imd_decile] |>
  ggplot(aes(factor(imd_decile), n_bookies)) +
  geom_col(fill = "#BE0E34") +
  theme_piers() +
  labs(x = "Decile of deprivation score (the lower the more deprived)", y = "Number of bookies")

# Share of bookies in 25% most deprived lower super output areas
lsoa_bookies[order(imd_rank)][1:(nrow(lsoa_bookies)/4), sum(n_bookies) / sum(lsoa_bookies$n_bookies)]




