#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(readr)
  library(scales)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd

season_to_plot <- "2025-26"
input_csv <- file.path(project_root, "data", "derived", "epl_points_progression_2020_21_to_2025_26.csv")
output_png <- file.path(project_root, "docs", "figures", "epl-2025-26-points-progression.png")

if (!file.exists(input_csv)) {
  stop(sprintf("Missing input data: %s. Run scripts/download_epl_results.R first.", input_csv))
}

dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)

points_progression <- read_csv(input_csv, show_col_types = FALSE, progress = FALSE) %>%
  filter(season == season_to_plot) %>%
  mutate(
    date = as.Date(date),
    downloaded_at = as.Date(downloaded_at)
  )

if (nrow(points_progression) == 0) {
  stop(sprintf("No rows found for season %s in %s.", season_to_plot, input_csv))
}

teams <- sort(unique(points_progression$team))
completed_count <- n_distinct(points_progression$match_id)
latest_match_date <- max(points_progression$date, na.rm = TRUE)
download_date <- max(points_progression$downloaded_at, na.rm = TRUE)
source_url <- points_progression %>%
  distinct(source_url) %>%
  pull(source_url) %>%
  paste(collapse = "; ")

team_match_counts <- points_progression %>%
  count(team, name = "matches_played") %>%
  arrange(desc(matches_played), team)

if (length(teams) != 20) {
  warning(sprintf("Expected 20 EPL teams, found %s: %s", length(teams), paste(teams, collapse = ", ")))
}

if (completed_count < 380) {
  message(sprintf(
    "Source has %s completed matches through %s; chart is season-to-date.",
    completed_count,
    latest_match_date
  ))
} else {
  message(sprintf("Source has all %s completed EPL matches through %s.", completed_count, latest_match_date))
}

uneven_counts <- team_match_counts %>%
  filter(matches_played != max(matches_played))

if (nrow(uneven_counts) > 0) {
  message("Teams with fewer completed matches than the current maximum:")
  message(paste(sprintf("%s (%s)", uneven_counts$team, uneven_counts$matches_played), collapse = ", "))
}

plot_df <- points_progression %>%
  select(team, match_number, date, cumulative_points) %>%
  bind_rows(
    tibble(
      team = teams,
      match_number = 0L,
      date = min(points_progression$date, na.rm = TRUE) - days(1),
      cumulative_points = 0L
    )
  )

end_labels <- points_progression %>%
  group_by(team) %>%
  slice_max(order_by = match_number, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(cumulative_points), desc(goal_difference), desc(goals_for_total), team) %>%
  mutate(
    label = sprintf("%s  %s", team, cumulative_points),
    x_label = latest_match_date + days(12)
  )

space_end_labels <- function(y, min_gap = 2.1) {
  spaced <- y
  for (i in seq_along(spaced)) {
    if (i == 1) next
    spaced[i] <- min(spaced[i], spaced[i - 1] - min_gap)
  }
  spaced
}

end_labels <- end_labels %>%
  mutate(y_label = space_end_labels(cumulative_points))

club_cols <- c(
  "Arsenal" = "#D71920",
  "Aston Villa" = "#7A003C",
  "Bournemouth" = "#DA291C",
  "Brentford" = "#E30613",
  "Brighton" = "#0057B8",
  "Burnley" = "#6C1D45",
  "Chelsea" = "#034694",
  "Crystal Palace" = "#1B458F",
  "Everton" = "#003399",
  "Fulham" = "#111111",
  "Leeds" = "#FFCD00",
  "Liverpool" = "#C8102E",
  "Man City" = "#6CABDD",
  "Man United" = "#DA291C",
  "Newcastle" = "#241F20",
  "Nott'm Forest" = "#DD0000",
  "Sunderland" = "#E03A3E",
  "Spurs" = "#132257",
  "Tottenham" = "#132257",
  "West Ham" = "#7A263A",
  "Wolves" = "#FDB913"
)

missing_colours <- setdiff(teams, names(club_cols))
if (length(missing_colours) > 0) {
  fallback_cols <- setNames(hue_pal()(length(missing_colours)), missing_colours)
  club_cols <- c(club_cols, fallback_cols)
}

bg <- "white"
text_col <- "#3C3C3C"
muted_col <- "#666666"
grid_col <- "grey92"
axis_col <- "grey30"

subtitle <- sprintf(
  "Cumulative points by date; %s matches through %s.",
  completed_count,
  format(latest_match_date, "%d %b %Y")
)

p <- ggplot(plot_df, aes(x = date, y = cumulative_points, colour = team)) +
  geom_hline(
    yintercept = seq(0, max(plot_df$cumulative_points, na.rm = TRUE), by = 10),
    colour = grid_col,
    linewidth = 0.28
  ) +
  geom_line(linewidth = 0.75, alpha = 0.9) +
  geom_point(
    data = points_progression,
    aes(x = date, y = cumulative_points, colour = team),
    size = 0.9,
    alpha = 0.75
  ) +
  geom_segment(
    data = end_labels,
    aes(
      x = date,
      xend = x_label - days(2),
      y = cumulative_points,
      yend = y_label,
      colour = team
    ),
    inherit.aes = FALSE,
    linewidth = 0.25,
    alpha = 0.55
  ) +
  geom_text(
    data = end_labels,
    aes(x = x_label, y = y_label, label = label, colour = team),
    inherit.aes = FALSE,
    hjust = 0,
    fontface = "bold",
    size = 3.0
  ) +
  scale_colour_manual(values = club_cols[teams]) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = c(
      min(plot_df$date, na.rm = TRUE),
      max(plot_df$date, na.rm = TRUE) + days(55)
    ),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0, max(plot_df$cumulative_points, na.rm = TRUE) + 5, by = 10),
    limits = c(0, max(plot_df$cumulative_points, na.rm = TRUE) + 4),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(
    title = "Premier League 2025-26 points race",
    subtitle = subtitle,
    x = NULL,
    y = "Points",
    caption = sprintf("Source: Football-Data.co.uk E0.csv (%s), downloaded %s.", source_url, download_date)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Helvetica", base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg, colour = bg),
    panel.background = element_rect(fill = bg, colour = bg),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.28),
    axis.line.x = element_line(colour = axis_col, linewidth = 0.25),
    axis.line.y = element_line(colour = axis_col, linewidth = 0.25),
    axis.text = element_text(face = "bold", colour = text_col, size = 10),
    axis.title = element_text(face = "bold", colour = text_col, size = 12),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.title = element_text(face = "bold", colour = text_col, size = 22),
    plot.subtitle = element_text(colour = muted_col, size = 12),
    plot.caption = element_text(colour = muted_col, size = 9),
    plot.margin = margin(16, 120, 12, 14)
  )

ggsave(output_png, plot = p, width = 15.5, height = 8.8, dpi = 220, bg = bg)

message(sprintf("Saved chart to %s", output_png))
