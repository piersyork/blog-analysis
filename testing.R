library(dplyr)
library(ggplot2)

options(box.path = getwd())
box::use(functions/theme[...])

# Potential narratives:
# - As bookmakers begin to decline where will they remain
# - Bookmakers are profiting off of deprivation, there is little councils can do to stop them.
# -

readxl::read_xlsx("~/Downloads/Industry_Statistics_-_July_2022_Revision.xlsx", sheet = "6a", range = "A9:O21", .name_repair = "minimal") |>
  select(1:8) |>
  janitor::clean_names() |>
  mutate(year = as.numeric(stringr::word(reporting_period, 5, -1)), .before = total) |>
  select(-total, -percent_change, -reporting_period) |>
  tidyr::pivot_longer(dogs:other, names_to = "category", values_to = "yield") |>
  group_by(category) |>
  mutate(label = ifelse(year == max(year), stringr::str_to_title(category), NA)) |>
  ggplot(aes(year, yield, colour = category, label = label)) +
  geom_line() +
  ggrepel::geom_text_repel(hjust = 0, xlim = Inf, na.rm = TRUE, segment.colour = "grey") +
  coord_cartesian(clip = "off") +
  scale_colour_ons() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "£"), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  theme_piers() +
  theme(plot.margin = margin(11, 55, 5, 10),
        legend.position = "none",
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "While turnover from horse betting falls, profit from football \nbetting is steadily rising",
       subtitle = "Turnover (£ millions), years ending March",
       caption = "Source: Gambling Commission Industry Statistics, November 2021",
       x = "")

gambling_yield <- readxl::read_xlsx("~/Downloads/Industry_Statistics_-_July_2022_Revision.xlsx", sheet = "1", range = "A8:M21", .name_repair = "minimal") |>
  janitor::clean_names() |>
  select(-contains("total"), -percent_change) |>
  mutate(year = as.numeric(stringr::word(reporting_period, 5) |> stringr::str_remove("P|R")), .before = reporting_period,
         across(everything(), as.double)) |>
  select(-reporting_period) |>
  tidyr::pivot_longer(arcades_non_remote:the_national_lottery_remote_and_non_remote,
                      names_to = "category", values_to = "yield")

premises_plot <- readxl::read_xlsx("~/Downloads/Industry_Statistics_-_July_2022_Revision.xlsx",
                  sheet = "3", range = "A9:H22", .name_repair = "minimal",
                  col_types = c("date", rep("numeric", 7))) |>
  janitor::clean_names() |>
  mutate(year = lubridate::year(reporting_period)) |>
  ggplot(aes(year, betting)) +
  geom_line(colour = "#12436D") +
  geom_point(colour = "#12436D") +
  scale_y_continuous(limits = c(0, 10e3)) +
  scale_x_continuous(limits = c(2009, NA), breaks = seq(2010, 2020, 2)) +
  theme_piers() +
  theme(plot.margin = margin(11, 55, 5, 10),
        plot.title = element_text(size = 14),
        legend.position = "none",
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Even before covid, the number of betting \nshops has been steadily decreasing",
       subtitle = "Number of betting shops, years ending March",
       caption = "", x = "")



yield_plot <- gambling_yield |>
  filter(category %in% c("betting_non_remote", "betting_remote")) |>
  na.omit() |>
  group_by(category) |>
  ggplot(aes(year, yield, colour = category)) +
  geom_line() +
  geom_point(size = 1) +
  theme_piers() +
  scale_colour_ons() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "£"), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  annotate("text", x = 2016.8, y = 3060, label = "Non-remote betting", colour = "#393A76") +
  annotate("text", x = 2018.4, y = 1750, label = "Remote betting", colour = "#BE0E34") +
  theme(legend.position = "none",
        plot.title = element_text(size = 14),
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Profit from remote betting is steadily increasing as \nin-shop betting takes a steep fall",
       subtitle = "Gross gambling yield (£ millions), years ending March",
       caption = "Source: Gambling Commission Industry Statistics, July 2022",
       x = "")

title <- ggdraw() +
  draw_label(
    "The decline of in-preson betting",
    fontface = "bold",
    size = 20,
    hjust = 1.15,
    vjust = 0.2,
  )

grid_plots <- cowplot::plot_grid(yield_plot, premises_plot)
cowplot::plot_grid(title, grid_plots, nrow = 2, rel_heights = c(0.12, 1))


in_person <- readxl::read_xlsx("~/Downloads/FINAL_Survey-data-on-gambling-participation-April-2022_-_.xlsx",
                  sheet = "1e", range = "A30:C34", col_names = c("year", "mode", "participation")) |>
  mutate(year = as.numeric(stringr::word(year, 4)), mode = "In Person")

online <- readxl::read_xlsx("~/Downloads/FINAL_Survey-data-on-gambling-participation-April-2022_-_.xlsx",
                  sheet = "1c", range = "A27:C31", col_names = c("year", "mode", "participation")) |>
  mutate(year = as.numeric(stringr::word(year, 4)), mode = "Online")

bind_rows(in_person, online) |>
  ggplot(aes(year, participation, colour = mode)) +
  geom_line() +
  geom_point(size = 1) +
  annotate("text", x = 2020.6, y = 22, label = "In-person", colour = "#393A76", size = 4.2) +
  annotate("text", x = 2019.6, y = 14.7, label = "Online", colour = "#BE0E34", size = 4.2) +
  theme_piers() +
  scale_colour_ons() +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, suffix = "%"), limits = c(0, NA)) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(), plot.caption = element_text(vjust = -1)) +
  labs(title = "People are gambling in-person less since the covid \npandemic",
       subtitle = "Respondents who have gambled in the last 4 weeks; years are to the \nend of March",
       caption = "Source: Gambling Commission")

mar

bind_rows(in_person, online) |>
  ggplot(aes(year, participation, fill = mode)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, ) +
  theme_piers() +
  scale_fill_piers() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_blank())

bind_rows(in_person, online) |>
  group_by(mode) |>
  mutate(pct_change = (participation - lag(participation)) / lag(participation)) |>
  na.omit() |>
  ggplot(aes(factor(year), pct_change, fill = mode)) +
  geom_col(position = position_dodge(), width = 0.7) +
  scale_fill_piers() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(-0.4, 0.2)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  theme_piers() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major.x = element_blank())

uk_pop
gb_population <- readxl::read_xls("betting-shops/data/uk-population-estimates-mid-2020.xls", sheet = 7, range = "A8:D426") |>
  slice(2) |>
  pull(`All ages`)

(in_person$participation[5] / 100) * gb_population

gambling_yield |>
  filter(category == "betting_non_remote", year == 2020) |>
  pull(yield)




