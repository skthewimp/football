#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(tidyr)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd

seasons <- tibble::tribble(
  ~season,   ~football_data_code,
  "2020-21", "2021",
  "2021-22", "2122",
  "2022-23", "2223",
  "2023-24", "2324",
  "2024-25", "2425",
  "2025-26", "2526"
)

raw_dir <- file.path(project_root, "data", "raw")
derived_dir <- file.path(project_root, "data", "derived")
combined_raw_csv <- file.path(raw_dir, "epl_results_2020_21_to_2025_26.csv")
combined_derived_csv <- file.path(derived_dir, "epl_points_progression_2020_21_to_2025_26.csv")
current_season_derived_csv <- file.path(derived_dir, "epl_2025_26_points_progression.csv")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

parse_match_date <- function(x) {
  parsed <- suppressWarnings(dmy(x))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(dmy(x, truncated = 2))
  }
  parsed
}

parse_match_time <- function(x) {
  if (is.null(x)) {
    return(rep(NA_real_, 0))
  }

  parsed <- suppressWarnings(hm(x))
  hour(parsed) * 60 + minute(parsed)
}

download_season <- function(season, football_data_code) {
  source_url <- sprintf("https://www.football-data.co.uk/mmz4281/%s/E0.csv", football_data_code)
  raw_csv <- file.path(raw_dir, sprintf("epl_results_%s.csv", gsub("-", "_", season)))

  message(sprintf("Downloading %s from %s", season, source_url))
  download.file(source_url, raw_csv, mode = "wb", quiet = TRUE)

  read_csv(raw_csv, show_col_types = FALSE, progress = FALSE) %>%
    mutate(
      season = season,
      source_url = source_url,
      downloaded_at = Sys.Date(),
      source_row = row_number(),
      .before = 1
    )
}

raw_results <- purrr::map2_dfr(
  seasons$season,
  seasons$football_data_code,
  download_season
)

write_csv(raw_results, combined_raw_csv)

required_cols <- c("season", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR")
missing_cols <- setdiff(required_cols, names(raw_results))

if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns in source CSVs: %s", paste(missing_cols, collapse = ", ")))
}

match_results <- raw_results %>%
  mutate(
    match_date = parse_match_date(Date),
    match_time_minutes = if ("Time" %in% names(.)) parse_match_time(Time) else NA_real_,
    home_goals = suppressWarnings(as.integer(FTHG)),
    away_goals = suppressWarnings(as.integer(FTAG)),
    source_result = as.character(FTR)
  )

bad_dates <- match_results %>%
  filter(!is.na(Date), is.na(match_date))

if (nrow(bad_dates) > 0) {
  stop(sprintf("Could not parse %s match dates from source CSVs.", nrow(bad_dates)))
}

completed_matches <- match_results %>%
  filter(
    !is.na(match_date),
    !is.na(home_goals),
    !is.na(away_goals),
    source_result %in% c("H", "D", "A")
  ) %>%
  arrange(season, match_date, match_time_minutes, source_row)

if (nrow(completed_matches) == 0) {
  stop("No completed EPL matches found in the downloaded source CSVs.")
}

season_team_counts <- completed_matches %>%
  group_by(season) %>%
  summarise(
    completed_matches = n(),
    teams = n_distinct(c(HomeTeam, AwayTeam)),
    latest_match_date = max(match_date),
    .groups = "drop"
  )

bad_team_counts <- season_team_counts %>%
  filter(teams != 20)

if (nrow(bad_team_counts) > 0) {
  warning(sprintf(
    "Expected 20 teams per season; mismatches: %s",
    paste(sprintf("%s=%s", bad_team_counts$season, bad_team_counts$teams), collapse = ", ")
  ))
}

points_progression <- bind_rows(
  completed_matches %>%
    transmute(
      season,
      match_id = source_row,
      date = match_date,
      team = HomeTeam,
      opponent = AwayTeam,
      venue = "H",
      goals_for = home_goals,
      goals_against = away_goals,
      result = case_when(
        source_result == "H" ~ "W",
        source_result == "D" ~ "D",
        TRUE ~ "L"
      ),
      points = case_when(
        source_result == "H" ~ 3L,
        source_result == "D" ~ 1L,
        TRUE ~ 0L
      ),
      source_url,
      downloaded_at
    ),
  completed_matches %>%
    transmute(
      season,
      match_id = source_row,
      date = match_date,
      team = AwayTeam,
      opponent = HomeTeam,
      venue = "A",
      goals_for = away_goals,
      goals_against = home_goals,
      result = case_when(
        source_result == "A" ~ "W",
        source_result == "D" ~ "D",
        TRUE ~ "L"
      ),
      points = case_when(
        source_result == "A" ~ 3L,
        source_result == "D" ~ 1L,
        TRUE ~ 0L
      ),
      source_url,
      downloaded_at
    )
) %>%
  arrange(season, team, date, match_id) %>%
  group_by(season, team) %>%
  mutate(
    match_number = row_number(),
    cumulative_points = cumsum(points),
    goal_difference = cumsum(goals_for - goals_against),
    goals_for_total = cumsum(goals_for),
    goals_against_total = cumsum(goals_against)
  ) %>%
  ungroup() %>%
  select(
    season, team, match_number, date, opponent, venue, goals_for, goals_against, result, points,
    cumulative_points, goal_difference, goals_for_total, goals_against_total,
    match_id, source_url, downloaded_at
  )

write_csv(points_progression, combined_derived_csv)
write_csv(points_progression %>% filter(season == "2025-26"), current_season_derived_csv)

message("Season coverage:")
season_team_counts %>%
  mutate(
    status = if_else(completed_matches == 380, "complete", "season-to-date"),
    summary = sprintf(
      "%s: %s matches, %s teams, latest %s (%s)",
      season,
      completed_matches,
      teams,
      latest_match_date,
      status
    )
  ) %>%
  pull(summary) %>%
  paste(collapse = "\n") %>%
  message()

message(sprintf("Saved combined raw results to %s", combined_raw_csv))
message(sprintf("Saved combined points progression to %s", combined_derived_csv))
message(sprintf("Saved 2025-26 points progression to %s", current_season_derived_csv))
