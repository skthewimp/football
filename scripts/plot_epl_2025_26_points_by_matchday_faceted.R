#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd

season_to_plot <- "2025-26"
input_csv <- file.path(project_root, "data", "derived", "epl_points_progression_2020_21_to_2025_26.csv")
output_png <- file.path(project_root, "docs", "figures", "epl-2025-26-points-by-matchday-faceted.png")

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

completed_count <- n_distinct(points_progression$match_id)
latest_match_date <- max(points_progression$date, na.rm = TRUE)
max_match_number <- max(points_progression$match_number, na.rm = TRUE)
download_date <- max(points_progression$downloaded_at, na.rm = TRUE)
source_url <- points_progression %>%
  distinct(source_url) %>%
  pull(source_url) %>%
  paste(collapse = "; ")

team_order <- points_progression %>%
  group_by(team) %>%
  slice_max(order_by = match_number, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(cumulative_points), desc(goal_difference), desc(goals_for_total), team) %>%
  pull(team)

team_stories <- c(
  "Arsenal" = "Two five-win bursts, four wins to close",
  "Man City" = "Six-win midseason run, late chase",
  "Man United" = "Four-win spring burst lifts them to third",
  "Aston Villa" = "Eight straight wins drive the season",
  "Liverpool" = "Five straight wins, then four losses",
  "Bournemouth" = "Five-match draw spell before late wins",
  "Brighton" = "Late three-win run pushes top half",
  "Chelsea" = "Four straight wins, then six losses",
  "Brentford" = "Five straight draws stall late push",
  "Sunderland" = "Four straight draws, then steady finish",
  "Newcastle" = "Four straight losses blunt the run-in",
  "Everton" = "Stop-start, never more than two wins",
  "Fulham" = "Three-win run offsets early slide",
  "Leeds" = "Slow middle, strong 16-point finish",
  "Crystal Palace" = "Bright early spell fades late",
  "Nott'm Forest" = "Three straight late wins rescue finish",
  "Tottenham" = "Five-loss slide defines the run-in",
  "West Ham" = "Three straight losses to close",
  "Burnley" = "Seven-loss autumn run, five-loss finish",
  "Wolves" = "Eleven straight losses after early slump"
)

facet_levels <- sprintf("%s\n%s", team_order, team_stories[team_order])

plot_df <- points_progression %>%
  select(team, match_number, cumulative_points) %>%
  bind_rows(
    tibble(
      team = unique(points_progression$team),
      match_number = 0L,
      cumulative_points = 0L
    )
  ) %>%
  mutate(
    facet_label = sprintf("%s\n%s", team, team_stories[team]),
    facet_label = factor(facet_label, levels = facet_levels)
  )

end_points <- points_progression %>%
  group_by(team) %>%
  slice_max(order_by = match_number, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    facet_label = sprintf("%s\n%s", team, team_stories[team]),
    facet_label = factor(facet_label, levels = facet_levels),
    label = cumulative_points
  )

if (completed_count < 380) {
  message(sprintf(
    "Source has %s completed matches through %s; chart is season-to-date.",
    completed_count,
    latest_match_date
  ))
} else {
  message(sprintf("Source has all %s completed EPL matches through %s.", completed_count, latest_match_date))
}

bg <- "white"
text_col <- "#3C3C3C"
muted_col <- "#666666"
grid_col <- "grey92"
axis_col <- "grey30"
line_col <- "#2B6CB0"
point_col <- "#1F4E79"

subtitle <- sprintf(
  "One panel per club; cumulative points after each club's match. %s matches through %s.",
  completed_count,
  format(latest_match_date, "%d %b %Y")
)

p <- ggplot(plot_df, aes(x = match_number, y = cumulative_points)) +
  geom_hline(
    yintercept = seq(0, max(plot_df$cumulative_points, na.rm = TRUE), by = 10),
    colour = grid_col,
    linewidth = 0.24
  ) +
  geom_line(colour = line_col, linewidth = 0.65) +
  geom_point(colour = point_col, size = 0.7, alpha = 0.75) +
  geom_text(
    data = end_points,
    aes(x = match_number + 0.8, y = cumulative_points, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    colour = text_col,
    fontface = "bold",
    size = 2.7
  ) +
  facet_wrap(~facet_label, ncol = 5) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 30, max_match_number),
    limits = c(0, max_match_number + 3),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0, max(plot_df$cumulative_points, na.rm = TRUE) + 5, by = 20),
    limits = c(0, max(plot_df$cumulative_points, na.rm = TRUE) + 4),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  labs(
    title = "Premier League 2025-26 points race by matchday",
    subtitle = subtitle,
    x = "Matches played",
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
    panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.24),
    axis.line.x = element_line(colour = axis_col, linewidth = 0.22),
    axis.line.y = element_line(colour = axis_col, linewidth = 0.22),
    axis.text = element_text(face = "bold", colour = text_col, size = 8),
    axis.title = element_text(face = "bold", colour = text_col, size = 11),
    axis.ticks = element_blank(),
    strip.text = element_text(face = "bold", colour = text_col, size = 8.5, lineheight = 0.95),
    panel.spacing = unit(1.05, "lines"),
    legend.position = "none",
    plot.title = element_text(face = "bold", colour = text_col, size = 22),
    plot.subtitle = element_text(colour = muted_col, size = 12),
    plot.caption = element_text(colour = muted_col, size = 9),
    plot.margin = margin(16, 24, 12, 14)
  )

ggsave(output_png, plot = p, width = 15.5, height = 10.8, dpi = 220, bg = bg)

message(sprintf("Saved chart to %s", output_png))
