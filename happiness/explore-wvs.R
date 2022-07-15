library(data.table)
library(dplyr)
library(haven)
library(ggplot2)

wvs <- readRDS("EVS_WVS_Joint_rds_v3_0.rds") |>
  as.data.table()

owidR::owid_search("gdp per capita")

gdp <- owidR::owid("real-gdp-per-capita-PennWT", rename = "gdp_per_cap") |>
  filter(year == max(year)) |>
  as.data.table()

wvs |>
  select(cntry_AN, happiness = A008)


happiness <- wvs[
  as.numeric(A008) > 0,
  .(code = cntry_AN, happiness = ifelse(as_factor(A008) %in% c("Very happy", "Quite happy"), 1, 0))
][
  ,
  .(entity = countrycode::countrycode(code, "iso2c", "country.name"),
    avg_happiness = mean(happiness)),
  by = code
][
  order(-avg_happiness),
  .(code = countrycode::countrycode(code, "iso2c", "iso3c"), entity, avg_happiness)
][
  gdp,
  on = "code",
  nomatch = 0
]

happiness |>
  ggplot(aes(gdp_per_cap, avg_happiness)) +
  geom_point() +
  geom_smooth(method = lm)

















