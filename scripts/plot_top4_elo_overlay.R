#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(stringr)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd

input_rds <- file.path(project_root, "data", "raw", "clubelo_eng.rds")
output_png <- file.path(project_root, "docs", "figures", "arsenal-liverpool-man-city-man-united-elo-overlay.png")

if (!file.exists(input_rds)) {
  stop(sprintf("Missing input data: %s", input_rds))
}

bg <- "white"
text_col <- "#3C3C3C"
grid_col <- "grey92"
axis_col <- "grey30"

club_cols <- c(
  "Arsenal" = "#d7191c",
  "Liverpool" = "#8c1d40",
  "Man City" = "#2b8cbe",
  "Man United" = "#111111"
)

clubs <- c("Arsenal", "Liverpool", "Man City", "Man United")

df <- readRDS(input_rds) %>%
  filter(Club %in% clubs, From >= as.Date("2017-01-01")) %>%
  arrange(Club, From)

approx_elo <- function(club, date) {
  club_df <- df %>% filter(Club == club)
  approx(
    x = as.numeric(club_df$From),
    y = club_df$Elo,
    xout = as.numeric(date),
    rule = 2
  )$y
}

events <- tibble::tribble(
  ~Club, ~date, ~label,
  "Man City",   as.Date("2018-04-15"), "2017-18 title won",
  "Liverpool",  as.Date("2019-06-01"), "Champions\nLeague won",
  "Arsenal",    as.Date("2019-12-20"), "Arteta\nappointed",
  "Man United", as.Date("2021-05-10"), "Ole-era local peak",
  "Man City",   as.Date("2023-06-10"), "Treble\ncompleted",
  "Man United", as.Date("2024-05-25"), "FA Cup won\nvs City",
  "Liverpool",  as.Date("2024-05-19"), "Klopp farewell",
  "Arsenal",    as.Date("2026-03-05"), "Current peak"
) %>%
  rowwise() %>%
  mutate(Elo = approx_elo(Club, date)) %>%
  ungroup() %>%
  mutate(
    x_nudge = c(-100, -150, 110, -130, 145, 135, -120, 100),
    y_nudge = c(28, 40, -36, 26, 20, 20, -42, 24),
    x_label = date + x_nudge,
    y_label = Elo + y_nudge,
    label = str_wrap(label, 18)
  )

end_labels <- df %>%
  group_by(Club) %>%
  slice_max(order_by = From, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    x_label = From + c(70, 70, 70, 70),
    y_label = Elo + c(18, -4, -18, 0)
  )

season_lines <- tibble(
  season_start = as.Date(c(
    "2017-08-01", "2018-08-01", "2019-08-01", "2020-08-01", "2021-08-01",
    "2022-08-01", "2023-08-01", "2024-08-01", "2025-08-01"
  ))
)

p <- ggplot(df, aes(x = From, y = Elo, colour = Club)) +
  geom_hline(yintercept = seq(1750, 2100, by = 50), colour = grid_col, linewidth = 0.28) +
  geom_vline(
    data = season_lines,
    aes(xintercept = season_start),
    inherit.aes = FALSE,
    colour = grid_col,
    linewidth = 0.45,
    linetype = "dashed"
  ) +
  geom_line(linewidth = 0.95) +
  geom_point(
    data = events,
    aes(x = date, y = Elo, colour = Club),
    size = 2.4,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = events,
    aes(x = date, xend = x_label, y = Elo, yend = y_label, colour = Club),
    inherit.aes = FALSE,
    linewidth = 0.32,
    alpha = 0.75
  ) +
  geom_label(
    data = events,
    aes(x = x_label, y = y_label, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0.92),
    colour = text_col,
    fontface = "bold",
    size = 3.1,
    lineheight = 0.95,
    linewidth = 0
  ) +
  geom_text(
    data = end_labels,
    aes(x = x_label, y = y_label, label = Club, colour = Club),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4.2
  ) +
  scale_colour_manual(values = club_cols) +
  scale_x_date(
    limits = c(as.Date("2017-01-01"), as.Date("2026-12-31")),
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.07))
  ) +
  scale_y_continuous(
    limits = c(1720, 2110),
    breaks = seq(1750, 2100, by = 50),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "Adding Manchester United to the Elite ClubElo Overlay",
    subtitle = "Liverpool's Klopp spike, City's dynasty, Arsenal's climb, and United's shorter-lived surges all show up clearly.",
    x = NULL,
    y = "ClubElo rating",
    caption = "Source: ClubElo data refreshed in this repo. Event annotations use official club / competition sources."
  ) +
  theme_minimal(base_family = "Helvetica", base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg, colour = bg),
    panel.background = element_rect(fill = bg, colour = bg),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.28),
    axis.line.x = element_line(colour = axis_col, linewidth = 0.25),
    axis.line.y = element_line(colour = axis_col, linewidth = 0.25),
    axis.text = element_text(face = "bold", colour = text_col, size = 11),
    axis.title = element_text(face = "bold", colour = text_col, size = 14),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", colour = text_col, size = 22),
    plot.subtitle = element_text(colour = "#666666", size = 13),
    plot.caption = element_text(colour = "#666666", size = 10),
    plot.margin = margin(16, 26, 12, 14)
  )

ggsave(output_png, plot = p, width = 15.4, height = 8.8, dpi = 220, bg = bg)

message(sprintf("Saved %s", output_png))

# Public event sources used for annotation:
# Arsenal - Arteta appointed (20 Dec 2019): https://www.arsenal.com/news/mikel-arteta-joining-our-new-head-coach
# Liverpool - Champions League won (1 Jun 2019): https://www.uefa.com/uefachampionsleague/history/h2h/7889/1652/
# Liverpool - Klopp final Anfield farewell (19 May 2024): https://www.liverpoolfc.com/news/anfield-prepares-emotional-farewell-jurgen-klopp
# Man City - 2017/18 title confirmed (15 Apr 2018): https://www.premierleague.com/en/news/666121/manchester-city-confirmed-as-champions
# Man City - Treble completed (10 Jun 2023): https://www.uefa.com/uefachampionsleague/news/0282-1839b24603ef-36e94e63621d-1000--highlights-report-man-city-win-champions-league/
# Man United - Solskjaer period peak (10 May 2021) inferred from ClubElo series
# Man United - FA Cup win vs Man City (25 May 2024): https://www.manutd.com/en/news/detail/2024-fa-cup-final-kick-off-time-confirmed-for-man-utd-v-man-city-on-may-25
