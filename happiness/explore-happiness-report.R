options(box.path = getwd())
box::use(functions/theme[...])

library(ggdist)
library(ggplot2)
library(dplyr)
library(data.table)

whr <- readxl::read_xls("happiness/raw-data/DataForTable2.1.xls") |>
  janitor::clean_names() |>
  as.data.table()

satis <- readxl::read_xls("happiness/raw-data/Appendix_2_Data_for_Figure_2.1.xls") |>
  janitor::clean_names() |>
  as.data.table() |>
  na.omit()

scores_2017 <- readxl::read_xlsx("happiness/raw-data/final-data-for-figures-in-chapter-2-whr-2017.xlsx", sheet = 2) |>
  janitor::clean_names() |>
  na.omit() |>
  as.data.table()

clean_happiness <- satis[
  country != "Kosovo",
  .(country,
    continent = countrycode::countrycode(country, "country.name", "continent"),
    happiness_score)
][
  order(happiness_score)
]

clean_2017 <- scores_2017[
  ,
  .(country,
    continent = countrycode::countrycode(country, "country.name", "continent"),
    happiness_score)
]

clean_happiness |>
  na.omit() |>
  ggplot(aes(happiness_score, forcats::fct_reorder(continent, happiness_score, mean), colour = continent)) +
  ggdist::geom_dots(binwidth = 0.07, show.legend = FALSE, shape = 19) +
  scale_x_continuous(breaks = c(2:8), limits = c(2, 8)) +
  scale_y_discrete(expand = c(0.05, 0.01)) +
  coord_cartesian(clip = "off") +
  scale_colour_piers() +
  theme_piers() +
  theme(axis.title.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "solid", colour = "grey"),
        axis.text.y = element_text(vjust = -0.05),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        plot.margin = margin(15, 30, 5, 5)) +
  labs(title = "Of the 25% least happy countries, almost 70% are \nin Africa",
       subtitle = "The distribution of happiness around the world shows a large \nhappiness inequality",
       x = "Happiness Score",
       caption = "Source: World Happiness Report 2022 \nHappiness is measured on a scale of one to ten, calculated using six factors from the Gallup World Survey.") +
  annotate("text", 3.6, 2.6, label = "Afghanistan", colour = "grey60") +
  annotate("curve", 3.1, 2.6, xend = 2.5, yend = 2.2, colour = "grey",
           curvature = 0.18, arrow = arrow(length = unit(0.4, "cm"))) +
  annotate("text", 7.1, 1.6, label = "Mauritius", colour = "grey60") +
  annotate("curve", 6.7, 1.6, xend = 6.15, yend = 1.2, colour = "grey",
           curvature = 0.18, arrow = arrow(length = unit(0.4, "cm"))) +
  annotate("text", 8.05, 4.6, label = "Finland", colour = "grey60") +
  annotate("curve", 6.7, 4.6, xend = 6.15, yend = 4.2, colour = "grey",
           curvature = 0.18, arrow = arrow(length = unit(0.4, "cm")))

clean_happiness[
  order(happiness_score),
  .(saddest = head(.SD$country, 1), happiest = tail(.SD$country, 1), n = .N),
  by = continent
] |>
  gt::gt()

clean_cont <- clean_2017[, .(score_17 = median(happiness_score)), by = .(continent)] |>
  inner_join(clean_happiness[!country %like% "\\*$", .(score_22 = median(happiness_score)), by = .(continent)]) |>
  na.omit()

clean_cont |>
  ggplot() +
  geom_segment(aes(x = score_17, xend = score_22, y = continent, yend = continent), colour = "grey75") +
  geom_point(aes(score_17, continent, colour = "2016"), size = 3) +
  geom_point(aes(score_22, continent, colour = "2019-21"), size = 3) +
  scale_x_continuous(breaks = 2:8, limits = c(2, 8)) +
  scale_colour_piers() +
  labs(title = "Despite covid most places have seen an increase in \nhappiness in the last five years",
       x = "Average Happiness Score") +
  theme_piers() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "top")

clean_both <- clean_2017[, .(country, score_17 = happiness_score)] |>
  inner_join(clean_happiness[!country %like% "\\*$", .(country, score_22 = happiness_score)])

# Percent of countries that have had happiness increase
clean_both[, .(country, change = score_22 - score_17, increased = score_22 - score_17 > 0)][, sum(increased)]

# Of the 25% least happy countries, 70% are in Africa
clean_happiness[
  ,
  .(country, happiness_score, continent, rank = row.names(.SD),
    decile = as.numeric(row.names(.SD)) / nrow(.SD))
][
  decile <= .25, .(mean(continent == "Africa"), nrow(.SD))
]

# 69% of the third least happy countries are in africa
clean_happiness[1:(round(nrow(clean_happiness))*0.25), mean(continent == "Africa", na.rm = TRUE) |> round(3)]













