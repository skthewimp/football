#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(stringr)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd

input_rds <- file.path(project_root, "data", "raw", "clubelo_eng.rds")
output_png <- file.path(project_root, "docs", "figures", "arsenal-liverpool-man-city-elo-overlay.png")

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
  "Man City" = "#2b8cbe"
)

clubs <- c("Arsenal", "Liverpool", "Man City")

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
  "Man City",  as.Date("2018-04-15"), "2017-18 title won\n(Centurions year)",
  "Liverpool", as.Date("2019-06-01"), "Champions League won",
  "Arsenal",   as.Date("2019-12-20"), "Arteta appointed",
  "Liverpool", as.Date("2020-06-25"), "Premier League won",
  "Arsenal",   as.Date("2023-08-06"), "Community Shield won\nvs Man City",
  "Man City",  as.Date("2023-06-10"), "Treble completed",
  "Liverpool", as.Date("2024-05-19"), "Klopp farewell",
  "Man City",  as.Date("2024-05-19"), "Fourth straight\nleague title"
) %>%
  rowwise() %>%
  mutate(Elo = approx_elo(Club, date)) %>%
  ungroup() %>%
  mutate(
    label = str_wrap(label, 20),
    x_nudge = c(-110, -150, 110, 100, -140, 150, -130, 120),
    y_nudge = c(35, 45, -35, -40, 48, 18, -45, -35),
    x_label = date + x_nudge,
    y_label = Elo + y_nudge
  )

end_labels <- df %>%
  group_by(Club) %>%
  slice_max(order_by = From, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    label = case_when(
      Club == "Arsenal" ~ "Arsenal",
      Club == "Liverpool" ~ "Liverpool",
      TRUE ~ "Man City"
    ),
    x_label = From + c(70, 70, 70),
    y_label = Elo + c(18, 0, -18)
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
  geom_line(linewidth = 1) +
  geom_point(
    data = events,
    aes(x = date, y = Elo, colour = Club),
    size = 2.6,
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = events,
    aes(x = date, xend = x_label, y = Elo, yend = y_label, colour = Club),
    inherit.aes = FALSE,
    linewidth = 0.35,
    alpha = 0.8
  ) +
  geom_label(
    data = events,
    aes(x = x_label, y = y_label, label = label),
    inherit.aes = FALSE,
    fill = alpha("white", 0.92),
    colour = text_col,
    fontface = "bold",
    size = 3.35,
    lineheight = 0.95,
    linewidth = 0
  ) +
  geom_text(
    data = end_labels,
    aes(x = x_label, y = y_label, label = label, colour = Club),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 4.4
  ) +
  scale_colour_manual(values = club_cols) +
  scale_x_date(
    limits = c(as.Date("2017-01-01"), as.Date("2026-12-31")),
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.06))
  ) +
  scale_y_continuous(
    limits = c(1720, 2110),
    breaks = seq(1750, 2100, by = 50),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "How the Elite Shifted: ClubElo for Arsenal, Liverpool and Manchester City",
    subtitle = "From Liverpool's Klopp peak to City's dynasty to Arsenal's Arteta climb, the lines tell most of the story.",
    x = NULL,
    y = "ClubElo rating",
    caption = paste(
      "Source: ClubElo data refreshed in this repo.",
      "Event annotations use official club / competition sources."
    )
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
    plot.margin = margin(16, 24, 12, 14)
  )

ggsave(output_png, plot = p, width = 15, height = 8.8, dpi = 220, bg = bg)

message(sprintf("Saved %s", output_png))

# Public event sources used for annotation:
# Arsenal - Arteta appointed (20 Dec 2019): https://www.arsenal.com/news/mikel-arteta-joining-our-new-head-coach
# Arsenal - Community Shield win vs Man City (6 Aug 2023): https://www.arsenal.com/fa-community-shield-match-report-trossard-penalty-shootout
# Liverpool - Champions League won (1 Jun 2019): https://www.uefa.com/uefachampionsleague/history/h2h/7889/1652/
# Liverpool - Premier League title confirmed (25 Jun 2020): https://www.premierleague.com/en/news/1697247
# Liverpool - Klopp final Anfield farewell (19 May 2024): https://www.liverpoolfc.com/news/anfield-prepares-emotional-farewell-jurgen-klopp
# Man City - 2017/18 title confirmed (15 Apr 2018): https://www.premierleague.com/news/666121
# Man City - Treble completed / Champions League final (10 Jun 2023): https://www.uefa.com/uefachampionsleague/match/2037765--man-city-vs-inter/final/
# Man City - Fourth straight title (19 May 2024): https://www.premierleague.com/en/news/4019801
