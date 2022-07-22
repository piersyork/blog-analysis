library(dplyr)
library(ggplot2)

options(box.path = getwd())
box::use(functions/theme[...])

readxl::read_xlsx("~/Downloads/Industry_Statistics_November_2021.xlsx", sheet = "6a", range = "A9:O21", .name_repair = "minimal") |>
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
  scale_colour_piers() +
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

gambling_yield <- readxl::read_xlsx("~/Downloads/Industry_Statistics_November_2021.xlsx", sheet = "1", range = "A8:M21", .name_repair = "minimal") |>
  janitor::clean_names() |>
  select(-contains("total"), -percent_change) |>
  mutate(year = as.numeric(stringr::word(reporting_period, 5) |> stringr::str_remove("P")), .before = reporting_period,
         across(everything(), as.double)) |>
  select(-reporting_period) |>
  tidyr::pivot_longer(arcades_non_remote:the_national_lottery_remote_and_non_remote,
                      names_to = "category", values_to = "yield")

gambling_yield |>
  filter(category %in% c("betting_non_remote", "betting_remote")) |>
  na.omit() |>
  group_by(category) |>
  mutate(label = ifelse(year == max(year), stringr::str_to_title(stringr::str_replace_all(category, "_", " ")), NA)) |>
  ggplot(aes(year, yield, colour = category, label = label)) +
  geom_line() +
  ggrepel::geom_text_repel(hjust = 0, xlim = Inf, na.rm = TRUE, segment.colour = "grey") +
  coord_cartesian(clip = "off") +
  theme_piers() +
  scale_colour_piers() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "£"), limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  theme(plot.margin = margin(11, 100, 5, 10),
        legend.position = "none",
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank()) +
  labs(title = "Yield from remote betting soars as in-shop \nbetting begins its descent",
       subtitle = "Gross gambling yield (£ millions), years ending March",
       caption = "Source: Gambling Commission Industry Statistics, November 2021",
       x = "")











