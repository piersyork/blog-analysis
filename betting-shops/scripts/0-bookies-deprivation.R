library(dplyr)
library(ggplot2)

options(box.path = getwd())
box::use(functions/theme[...])

get_area_data <- function(x) {
  postcode <- x$postcode

  lsoas <- vector(length = length(postcode))
  lsoa_names <- vector(length = length(postcode))
  lads <- vector(length = length(postcode))
  lad_names <- vector(length = length(postcode))
  country <- vector(length = length(postcode))
  region <- vector(length = length(postcode))

  pb <- txtProgressBar(style = 3)
  pb_add <- seq(1/length(postcode), 1, 1/length(postcode)) # amount to add each loop

  for (i in 1:length(postcode)) {
    api_return <- tryCatch(
      jsonlite::fromJSON(glue::glue("https://api.postcodes.io/postcodes/{postcode[i]}"))$result,
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
      region[i] <- if (is.null(api_return$region)) NA else api_return$region
    }
    setTxtProgressBar(pb, pb_add[i])
    # print(i)
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

betting_shops <- readxl::read_xlsx("betting-shops/data/betting-shops.xlsx") |>
  janitor::clean_names() |>
  get_area_data() |>
  select(-account_number, -premises_activity)

#### Save bookies-deprivation data to /public ####################################################################
write.csv(betting_shops, "betting-shops/public/betting-shop-locations.csv")
##################################################################################################################


deprivation_la <- readxl::read_xlsx("betting-shops/data/deprivation-lad.xlsx",
                                    sheet = 2) |>
  select(area_code = `Local Authority District code (2019)`,
         deprivation = `IMD - Average score`,
         imd_rank = `IMD - Rank of average score`)

## Get data for population
uk_pop <- readxl::read_xls("betting-shops/data/uk-population-estimates-mid-2020.xls", sheet = 7, range = "A8:D426") |>
  select(area_code = Code, population = `All ages`)

bookies <- betting_shops |>
  mutate(region_name = case_when(region %in% c("North East", "North West", "Yorkshire and The Humber") ~ "North",
                                 region %in% c("East Midlands", "West Midlands", "East of England") ~ "Midlands and East",
                                 region %in% c("South West", "South East") ~ "South",
                                 TRUE ~ region)) |>
  group_by(area_name, area_code, region_name, region) |>
  count(name = "n_bookies") |>
  inner_join(uk_pop, by = "area_code") |>
  right_join(deprivation_la, by = "area_code") |>
  mutate(bookies_per_million = n_bookies / population * 1e6) |>
  arrange((bookies_per_million))


#### Save bookies-deprivation data to /public ####################################################################
write.csv(bookies, "betting-shops/public/betting-shops-deprivation.csv")
##################################################################################################################

# to-do: get area data for local authorities where there are 0 bookies, set NAs to 0.

## Create chart comparing deprivation to bookies per mil
bookies |>
  filter(!is.na(region_name)) |>
  ggplot(aes(deprivation, bookies_per_million, size = population, colour = region_name, fill = region_name)) +
  geom_point(shape = 21) +
  annotate("text", label = "City of London", y = 1000, x = 24.2, colour = "#393A76") +
  annotate("curve", x = 20.5, y = 1000, xend = 15.1, yend = 1900,
           curvature = -0.25, arrow = arrow(length = unit(0.5, "cm")), colour = "#393A76") +
  scale_y_log10() +
  scale_size_area(max_size = 5, labels = scales::label_number(big.mark = ",")) +
  scale_colour_piers() +
  scale_fill_piers(alpha = 0.5, guide = "none") +
  geom_smooth(aes(deprivation, bookies_per_million), method = "lm", colour = "#044a6b", inherit.aes = FALSE) +
  theme_piers() +
  labs(title = "Local authorities with higher deprivation see a higher \nnumber of bookies per person",
       subtitle = "Bookies per million people vs Index of Deprivation",
       x = "Index of Multiple Deprivation", y = "Bookies per million people", colour = "Region", size = "Population",
       caption = "Source: Analysis of Gambling Commission FoI; Ministry of Housing, Communties and Local Government") +
  guides(colour = guide_legend(override.aes = list(size = 4, shape = 16, alpha = 0.5))) +
  theme(plot.caption = element_text(hjust = 0, size = 10))

bookies |>
  filter(!is.na(region_name), area_name != "City of London") |>
  ggplot(aes(deprivation, bookies_per_million, colour = region_name, fill = region_name, text = area_name)) +
  geom_point(shape = 21, size = 2.5) +
  scale_colour_piers() +
  scale_fill_piers(alpha = 0.5, guide = "none") +
  geom_smooth(aes(deprivation, bookies_per_million), method = "lm", colour = "#044a6b", inherit.aes = FALSE) +
  theme_piers() +
  labs(title = "Local authorities with higher deprivation see a higher \nnumber of bookies per person",
       subtitle = "Bookies per million people vs Index of Deprivation",
       x = "Index of Multiple Deprivation", y = "Bookies per million people", colour = "Region", size = "Population",
       caption = "Note: Excludes City of London
Source: Analysis of Gambling Commission FoI; Ministry of Housing, Communties and Local Government") +
  guides(colour = guide_legend(override.aes = list(size = 4, shape = 16, alpha = 0.5))) +
  theme(plot.caption = element_text(hjust = 0, size = 10))

bookies |>
  filter(!is.na(region)) |>
  group_by(region) |>
  summarise(n_bookies = sum(n_bookies), population = sum(population)) |>
  mutate(bookies_per_million = n_bookies / population * 1e6) |>
  ggplot(aes(bookies_per_million, forcats::fct_reorder(region, bookies_per_million))) +
  geom_col() +
  theme_piers()


brands <- betting_shops |>
  group_by(account_name) |>
  count(name = "n_bookies") |>
  arrange(desc(n_bookies)) |>
  mutate(account_name = case_when(account_name == "Ladbrokes Betting & Gaming Limited" ~ "Ladbrokes",
                                  account_name == "Done Brothers (Cash Betting) Limited" ~ "Betfred", #  (Done Brothers)
                                  account_name == "William Hill Organization Limited" ~ "William Hill",
                                  account_name == "Power Leisure Bookmakers Limited" ~ "Paddy Power",
                                  TRUE ~ "Other")) |>
  summarise(n_bookies = sum(n_bookies)) |>
  arrange(n_bookies)

brands |>
  ggplot(aes(n_bookies, forcats::fct_relevel(account_name, c("Other", "Paddy Power", "William Hill", "Betfred", "Ladbrokes")))) +
  geom_col(fill = "#BE0E34", width = 0.8) +
  scale_x_continuous(n.breaks = 6) +
  theme_piers() +
  theme(axis.title = element_blank(), panel.grid.major.y = element_blank()) +
  labs(title = "40% of UK betting shops are owned by Ladbrokes",
       subtitle = "Number of bettings shops by bookmaker")

brands |>
  ggplot(aes(n_bookies, "", fill = forcats::fct_relevel(account_name, c("Ladbrokes", "Betfred", "William Hill", "Paddy Power", "Other")))) +
  geom_col() +
  theme_piers() +
  theme(legend.title = element_blank()) +
  scale_fill_piers()

brands$n_bookies[4] / sum(brands$n_bookies)


deprivation_lsoa <- readxl::read_xlsx("betting-shops/data/deprivation-lsoa.xlsx", sheet = 2) |>
  select(lsoa = `LSOA code (2011)`, lsoa_name = `LSOA name (2011)`,
         area_code = `Local Authority District code (2019)`, area_name = `Local Authority District name (2019)`,
         imd_rank = `Index of Multiple Deprivation (IMD) Rank`, imd_decile = `Index of Multiple Deprivation (IMD) Decile`)

decile_bookies <- deprivation_lsoa |>
  left_join(count(betting_shops, lsoa, name = "n_bookies"), by = "lsoa") |>
  # arrange(desc(n_bookies)) |>
  mutate(n_bookies = ifelse(is.na(n_bookies), 0, n_bookies)) |>
  group_by(imd_decile) |>
  summarise(n_bookies = sum(n_bookies))
decile_bookies |>
  ggplot(aes(factor(imd_decile), n_bookies)) +
  geom_col()


decile_bookies |>
  mutate(imd_quintile = ifelse(imd_decile %% 2 == 0, imd_decile / 2, round((imd_decile / 2) + 0.1, 0))) |>
  group_by(imd_quintile) |>
  summarise(n_bookies = sum(n_bookies)) |>
  ggplot(aes(factor(imd_quintile), n_bookies)) +
  geom_col()


decile_bookies |>
  mutate(prop = n_bookies / sum(n_bookies) * 100,
         roll_prop = cumsum(prop))







