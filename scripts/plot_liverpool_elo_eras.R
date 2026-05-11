#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(scales)
})

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "scripts") normalizePath("..", mustWork = TRUE) else current_wd

input_rds <- file.path(project_root, "data", "raw", "clubelo_eng.rds")
output_png <- file.path(project_root, "docs", "figures", "liverpool-elo-klopp-slot.png")

if (!file.exists(input_rds)) {
  stop(sprintf("Missing input data: %s", input_rds))
}

bg <- "white"
primary <- "#5f3946"
secondary <- "#888888"
divider <- "#d9d9d9"
text_col <- "#3C3C3C"
highlight <- "firebrick3"
slot_col <- "blue3"

lfc <- readRDS(input_rds) %>%
  filter(Club == "Liverpool") %>%
  arrange(From)

plot_start <- as.Date("2015-10-01")
plot_end <- as.Date("2026-05-31")

lfc_plot <- lfc %>%
  filter(From >= plot_start, From <= plot_end)

regimes <- tibble(
  era = c("Klopp", "Slot"),
  x = as.Date(c("2019-11-01", "2025-03-01")),
  y = c(2112, 2112)
)

season_lines <- tibble(
  season_start = as.Date(c(
    "2015-08-01", "2016-08-01", "2017-08-01", "2018-08-01", "2019-08-01",
    "2020-08-01", "2021-08-01", "2022-08-01", "2023-08-01", "2024-08-01", "2025-08-01"
  ))
) %>%
  filter(season_start >= plot_start, season_start <= plot_end)

events <- tribble(
  ~date, ~label, ~type,
  as.Date("2015-10-08"), "Klopp appointed", "neutral",
  as.Date("2019-06-01"), "Champions League won", "up",
  as.Date("2020-02-16"), "ClubElo peak: 2091", "up",
  as.Date("2020-06-25"), "Premier League won", "up",
  as.Date("2021-02-20"), "Injury-hit low", "down",
  as.Date("2022-05-28"), "CL final loss,\nstill near elite level", "down",
  as.Date("2024-05-19"), "Klopp farewell", "neutral",
  as.Date("2024-06-01"), "Slot starts", "neutral",
  as.Date("2025-03-16"), "League Cup final loss", "down",
  as.Date("2025-04-27"), "Premier League won", "up",
  as.Date("2026-05-10"), "Back to 1925 Elo\nand top-four fight", "down"
) %>%
  left_join(lfc %>% select(date = From, Elo), by = "date") %>%
  mutate(
    y = coalesce(Elo, approx(x = as.numeric(lfc$From), y = lfc$Elo, xout = as.numeric(date), rule = 2)$y),
    y_text = c(
      1766, 1968, 2107, 2056, 1898, 2056, 1828, 1848, 1974, 2042, 1868
    ),
    x_text = date + c(
      120, -240, 40, 60, -120, 210, -260, 120, 110, 120, -230
    ),
    colour = case_when(
      type == "up" ~ highlight,
      type == "down" ~ slot_col,
      TRUE ~ secondary
    )
  )

period_labels <- tribble(
  ~x, ~y, ~label, ~colour,
  as.Date("2016-09-01"), 1740, "Reset", text_col,
  as.Date("2018-11-01"), 1928, "Rise into\nEuropean force", text_col,
  as.Date("2019-11-01"), 2068, "Peak Klopp", text_col,
  as.Date("2021-07-01"), 1886, "Comedown\nand rebuild", text_col,
  as.Date("2022-05-10"), 2042, "Still elite", text_col,
  as.Date("2024-12-20"), 2032, "Slot's fast climb", text_col,
  as.Date("2025-12-05"), 1948, "Title won,\nnot yet sustained", text_col
)

p <- ggplot(lfc_plot, aes(x = From, y = Elo)) +
  geom_hline(yintercept = seq(1700, 2100, by = 100), colour = "grey92", linewidth = 0.35) +
  geom_vline(
    data = season_lines,
    aes(xintercept = season_start),
    inherit.aes = FALSE,
    colour = divider,
    linewidth = 0.6,
    linetype = "dashed",
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = as.Date("2024-06-01"),
    colour = secondary,
    linewidth = 0.5
  ) +
  geom_line(colour = primary, linewidth = 0.9) +
  geom_point(
    data = events,
    aes(x = date, y = y, colour = type),
    size = 2.6,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c(up = highlight, down = slot_col, neutral = secondary)
  ) +
  geom_segment(
    data = events,
    aes(x = date, xend = x_text, y = y, yend = y_text),
    colour = secondary,
    linewidth = 0.45
  ) +
  geom_label(
    data = events,
    aes(x = x_text, y = y_text, label = label),
    fill = alpha(bg, 0.92),
    colour = text_col,
    linewidth = 0,
    size = 3.7,
    lineheight = 0.95,
    family = "Helvetica"
  ) +
  geom_text(
    data = season_lines,
    aes(x = season_start, y = 1691, label = paste0(substr(format(season_start, "%Y"), 3, 4), "-", substr(format(season_start + 365, "%Y"), 3, 4))),
    inherit.aes = FALSE,
    colour = secondary,
    size = 3.1,
    angle = 90,
    vjust = 0.4,
    hjust = 1
  ) +
  geom_text(
    data = regimes,
    aes(x = x, y = y, label = era),
    inherit.aes = FALSE,
    fontface = "bold",
    colour = text_col,
    size = 5
  ) +
  geom_text(
    data = period_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    colour = period_labels$colour,
    size = 5.1,
    lineheight = 0.95,
    fontface = "bold"
  ) +
  scale_x_date(
    limits = c(plot_start, plot_end),
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    limits = c(1688, 2125),
    breaks = seq(1700, 2100, by = 100),
    labels = label_number(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(
    title = "Liverpool's ClubElo Through the Klopp and Slot Eras",
    subtitle = "Klopp built the machine. Slot lifted Liverpool back to No. 1, but the second-season fade is visible.",
    x = NULL,
    y = "ClubElo rating",
    caption = "Source: ClubElo data refreshed in this repo; annotations use public match and appointment dates."
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.background = element_rect(fill = bg, colour = bg),
    panel.background = element_rect(fill = bg, colour = bg),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.35),
    axis.line.x = element_line(colour = text_col, linewidth = 0.25),
    axis.line.y = element_line(colour = text_col, linewidth = 0.25),
    axis.text = element_text(colour = text_col, size = 12),
    axis.title = element_text(colour = text_col, face = "bold"),
    axis.title.y = element_text(size = 15),
    axis.ticks = element_blank(),
    plot.title = element_text(colour = text_col, face = "bold", size = 24),
    plot.subtitle = element_text(colour = secondary, size = 14),
    plot.caption = element_text(colour = secondary, size = 11),
    plot.margin = margin(18, 22, 14, 14)
  )

ggsave(output_png, plot = p, width = 15.5, height = 9.5, dpi = 220, bg = bg)

message(sprintf("Saved %s", output_png))
