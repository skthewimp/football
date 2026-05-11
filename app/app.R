library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)

current_wd <- normalizePath(getwd(), mustWork = TRUE)
project_root <- if (basename(current_wd) == "app") normalizePath("..", mustWork = TRUE) else current_wd
data_candidates <- c(
  file.path(project_root, "manager_elo_eng.rds"),
  file.path(project_root, "app", "manager_elo_eng.rds"),
  file.path(project_root, "data", "derived", "manager_elo_eng.rds")
)
data_path <- data_candidates[file.exists(data_candidates)][1]

if (is.na(data_path) || !file.exists(data_path)) {
  stop("Missing manager_elo_eng.rds. Run scripts/refresh_all.R and copy the refreshed file into app/ before deployment.")
}

mgrelo <- readRDS(data_path)

ui <- fluidPage(
  titlePanel("English Club Elo Ratings by Manager"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "club",
        "Club",
        choices = mgrelo %>% distinct(Club) %>% arrange(Club) %>% pull(Club)
      ),
      dateInput(
        "startDate",
        "Start Date",
        value = "1992-01-01",
        min = min(mgrelo$From1, na.rm = TRUE),
        max = Sys.Date()
      )
    ),
    mainPanel(
      plotOutput("clubPlot", height = "700px")
    )
  )
)

server <- function(input, output) {
  output$clubPlot <- renderPlot({
    req(input$club, input$startDate)

    mgrelo %>%
      filter(Club == input$club, From1 >= input$startDate, From1 <= Sys.Date()) %>%
      mutate(
        From = pmax(From, input$startDate),
        Mid = From + as.numeric(To - From, units = "days") / 2,
        Name = coalesce(Name, ""),
        Index = if_else(is.na(Index), 0, Index)
      ) %>%
      ggplot() +
      geom_rect(
        data = . %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, Index),
        aes(xmin = From, xmax = To, ymin = MinElo, ymax = MaxElo, fill = factor(Index %% 9 + 1)),
        alpha = 0.45
      ) +
      geom_text(
        data = . %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, AvgElo),
        aes(x = Mid, y = AvgElo, label = Name),
        fontface = "bold",
        size = 3
      ) +
      geom_vline(
        data = . %>% filter(Relegation),
        aes(xintercept = From1),
        colour = "red"
      ) +
      geom_vline(
        data = . %>% filter(Promotion),
        aes(xintercept = From1),
        colour = "darkgreen"
      ) +
      geom_line(aes(x = From1, y = Elo), linewidth = 0.8) +
      theme_bw() +
      scale_fill_brewer(palette = "Set3") +
      scale_x_date("", date_breaks = "3 years", date_minor_breaks = "6 months", date_labels = "%Y") +
      labs(
        title = input$club,
        y = "Elo Rating"
      ) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_line(colour = "grey85")
      )
  })
}

shinyApp(ui = ui, server = server)
