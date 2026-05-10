#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(readr)
  library(rvest)
  library(stringr)
  library(tidyr)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd
input_rds <- file.path(project_root, "data", "raw", "clubelo_eng.rds")
output_rds <- file.path(project_root, "data", "derived", "manager_elo_eng.rds")
output_csv <- file.path(project_root, "data", "derived", "manager_elo_eng_latest.csv")

clean_text <- function(x) {
  x %>%
    str_replace_all("\\[[^]]+\\]", "") %>%
    str_replace_all("\\u00a0", " ") %>%
    str_squish()
}

clean_date <- function(x) {
  parsed <- clean_text(x)
  parsed[parsed %in% c("", "Incumbent", "Present", "present")] <- NA_character_
  suppressWarnings(as.Date(lubridate::dmy(parsed)))
}

extract_manager_table <- function(url, club_col_candidates) {
  html <- read_html(url)
  tables <- html_table(html, fill = TRUE)

  table_index <- which(
    map_lgl(
      tables,
      ~ {
        cols <- names(.x)
        any(cols %in% club_col_candidates) && "Name" %in% cols && "From" %in% cols
      }
    )
  )[1]

  if (is.na(table_index)) {
    stop(sprintf("Could not find manager table at %s", url))
  }

  table <- as_tibble(tables[[table_index]])
  club_col <- intersect(names(table), club_col_candidates)[1]

  table %>%
    rename(Club = all_of(club_col)) %>%
    mutate(across(everything(), clean_text)) %>%
    mutate(
      From = clean_date(From),
      To = if ("Until" %in% names(.)) clean_date(Until) else as.Date(NA)
    ) %>%
    select(any_of(c("Name", "Nat.", "Club", "From", "To")))
}

standardize_club_name <- function(club) {
  case_when(
    club == "Manchester City" ~ "Man City",
    club == "Manchester United" ~ "Man United",
    club == "Queens Park Rangers" ~ "QPR",
    club == "Nottingham Forest" ~ "Forest",
    club == "Sheffield Wednesday" ~ "Sheffield Weds",
    club == "Tottenham Hotspur" ~ "Tottenham",
    club == "West Ham United" ~ "West Ham",
    club == "West Bromwich Albion" ~ "West Brom",
    club == "Wolverhampton Wanderers" ~ "Wolves",
    TRUE ~ club
  )
}

if (!file.exists(input_rds)) {
  stop(sprintf("Missing input data: %s. Run scripts/refresh_club_elo.R first.", input_rds))
}

allelo <- readRDS(input_rds)

prem_mgrs <- extract_manager_table(
  "https://en.wikipedia.org/wiki/List_of_Premier_League_managers",
  c("Club", "Premier League club")
)

champ_mgrs <- extract_manager_table(
  "https://en.wikipedia.org/wiki/List_of_EFL_Championship_managers",
  c("Club", "Championship club", "EFL Championship club")
)

mgrs <- bind_rows(prem_mgrs, champ_mgrs) %>%
  mutate(
    Club = standardize_club_name(Club),
    To = coalesce(To, Sys.Date())
  ) %>%
  filter(!is.na(From), !is.na(Club), !is.na(Name)) %>%
  arrange(Club, From, To) %>%
  group_by(Name, Club) %>%
  mutate(
    To = coalesce(To, lead(From, 1), Sys.Date()),
    From = coalesce(From, lag(To, 1))
  ) %>%
  ungroup() %>%
  distinct()

mgrelo <- mgrs %>%
  arrange(Club, From) %>%
  group_by(Club) %>%
  mutate(
    Index = row_number(),
    Regime = as.numeric(To - From, units = "days")
  ) %>%
  ungroup() %>%
  filter(To >= as.Date("1937-01-01"), Regime >= 30) %>%
  mutate(
    FirstWord = word(Club, 1),
    InElo = Club %in% (allelo %>% distinct(Club) %>% pull(Club)),
    Club = if_else(InElo, Club, FirstWord),
    Parity = Index %% 2,
    LastName = word(Name, -1)
  ) %>%
  select(Club, From, To, Index, Parity, Name) %>%
  inner_join(
    allelo %>% select(Club, Level, Elo, From1 = From, To1 = To),
    by = "Club",
    relationship = "many-to-many"
  ) %>%
  filter(From1 >= From, From1 <= To) %>%
  bind_rows(
    allelo %>%
      filter(Club %in% .$Club) %>%
      select(Club, Level, Elo, From1 = From, To1 = To) %>%
      anti_join(
        select(., Club, From1, To1),
        by = c("Club", "From1", "To1")
      )
  ) %>%
  group_by(Club, Name, From) %>%
  mutate(
    MaxElo = max(Elo, na.rm = TRUE),
    MinElo = min(Elo, na.rm = TRUE),
    AvgElo = if_else(Parity == 1, MaxElo - 10, MinElo + 10)
  ) %>%
  group_by(Club) %>%
  arrange(From1, .by_group = TRUE) %>%
  mutate(
    Relegation = Level == lag(Level, 1) + 1,
    Promotion = Level == lag(Level, 1) - 1
  ) %>%
  ungroup() %>%
  arrange(Club, From1) %>%
  distinct(Club, From1, .keep_all = TRUE) %>%
  group_by(Club) %>%
  mutate(
    Name = coalesce(Name, ""),
    prevName = coalesce(lag(Name, 1), ""),
    change = Name != prevName,
    Index = cumsum(change)
  ) %>%
  group_by(Club, Index) %>%
  mutate(
    From = min(From1, na.rm = TRUE),
    To = max(To1, na.rm = TRUE),
    AvgElo = mean(Elo, na.rm = TRUE),
    Parity = Index %% 2,
    MaxElo = max(Elo, na.rm = TRUE),
    MinElo = min(Elo, na.rm = TRUE),
    AvgElo = if_else(Parity == 1, MaxElo - 10, MinElo + 10)
  ) %>%
  ungroup()

saveRDS(mgrelo, output_rds)

latest_manager_snapshot <- mgrelo %>%
  group_by(Club) %>%
  filter(From1 == max(From1, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Club, Name, Level, Elo, From1, To1, Relegation, Promotion)

write_csv(latest_manager_snapshot, output_csv)

message(sprintf("Saved %s rows to %s", nrow(mgrelo), output_rds))
