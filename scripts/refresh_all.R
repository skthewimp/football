#!/usr/bin/env Rscript

current_wd <- normalizePath(getwd(), mustWork = TRUE)
scripts_dir <- if (basename(current_wd) == "scripts") current_wd else file.path(current_wd, "scripts")
source(file.path(scripts_dir, "refresh_club_elo.R"), chdir = TRUE)
source(file.path(scripts_dir, "build_manager_elo.R"), chdir = TRUE)
