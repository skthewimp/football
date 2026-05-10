#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(curl)
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd
legacy_seed_path <- file.path(project_root, "legacy", "misc", "football_elo_epl.RData")
output_rds <- file.path(project_root, "data", "raw", "clubelo_eng.rds")
output_csv <- file.path(project_root, "data", "derived", "clubelo_eng_latest.csv")

clubelo_read_csv <- function(path_fragment) {
  base_urls <- c("http://api.clubelo.com", "https://api.clubelo.com")
  last_error <- NULL

  for (base_url in base_urls) {
    url <- sprintf("%s/%s", base_url, path_fragment)
    handle <- new_handle(connecttimeout = 5, timeout = 20, followlocation = TRUE)

    for (attempt in seq_len(3)) {
      result <- tryCatch(
        {
          response <- curl_fetch_memory(url, handle = handle)
          payload <- rawToChar(response$content)
          readr::read_csv(I(payload), show_col_types = FALSE, progress = FALSE)
        },
        error = function(err) err
      )

      if (!inherits(result, "error")) {
        return(result)
      }

      last_error <- result
      Sys.sleep(attempt)
    }
  }

  stop(sprintf("Failed to download ClubElo data for '%s': %s", path_fragment, conditionMessage(last_error)))
}

clubelo_slug <- function(club) {
  club %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]", "")
}

load_seed_clubs <- function() {
  seed_env <- new.env(parent = emptyenv())
  load(legacy_seed_path, envir = seed_env)

  seed_env$allelo %>%
    filter(Country == "ENG") %>%
    distinct(Club) %>%
    pull(Club)
}

fetch_current_eng_clubs <- function() {
  today_snapshot <- clubelo_read_csv(as.character(Sys.Date()))

  today_snapshot %>%
    filter(Country == "ENG") %>%
    distinct(Club) %>%
    pull(Club)
}

fetch_club_history <- function(club) {
  slug <- clubelo_slug(club)
  message(sprintf("Fetching %s (%s)", club, slug))

  clubelo_read_csv(slug) %>%
    mutate(
      From = as.Date(From),
      To = as.Date(To),
      Level = as.numeric(Level),
      Elo = as.numeric(Elo)
    )
}

seed_clubs <- if (file.exists(legacy_seed_path)) load_seed_clubs() else character()
current_clubs <- fetch_current_eng_clubs()

club_lookup <- tibble(Club = union(seed_clubs, current_clubs)) %>%
  arrange(Club)

allelo <- map_dfr(club_lookup$Club, fetch_club_history) %>%
  filter(Country == "ENG") %>%
  arrange(Club, From, To)

saveRDS(allelo, output_rds)

latest_snapshot <- allelo %>%
  group_by(Club) %>%
  filter(From == max(From, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Rank = suppressWarnings(as.numeric(Rank))) %>%
  arrange(Rank, desc(Elo), Club)

write_csv(latest_snapshot, output_csv)

message(sprintf("Saved %s rows to %s", nrow(allelo), output_rds))
