#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
load('plmanagerelo.RData')

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Football Club Elo Ratings"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectizeInput('club', "Club", choices=mgrelo %>% distinct(Club) %>% pull(Club)), 
        #selectizeInput('manager', "Manager", choices=mgrelo %>% distinct(Name,Club) %>% count(Name) %>% filter(n>=2) %>% arrange(Name) %>% pull(Name)),
        dateInput('startDate', "Start Date", value = '1992-01-01', min = '1949-01-01', max=Sys.Date())
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("clubPlot")
         #plotOutput('managerPlot')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$clubPlot <- renderPlot({
    mgrelo %>%
      filter(Club==input$club & From1 >= input$startDate & From1 <= Sys.Date()) %>%
      mutate(From=pmax(From, input$startDate), Mid=From + as.numeric(To-From, units='days')/2, Name=coalesce(Name, ''), Index=ifelse(is.na(Index),0,Index)) %>%
      ggplot() + 
      geom_rect(
        data=. %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, Index), 
        aes(xmin=From, xmax=To, ymin=MinElo, ymax=MaxElo, fill=factor(Index%%9+1)), alpha=0.5) + 
      geom_text(
        data=. %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, AvgElo),
        aes(x=Mid, y=AvgElo, label=Name),
        fontface='bold',
        size=3
        ) + 
      geom_vline(
        data=. %>% filter(Relegation), 
        aes(xintercept=From1), 
        col='red'
      ) + 
      geom_vline(
        data=. %>% filter(Promotion), 
        aes(xintercept=From1), 
        col='green'
      ) + 
      geom_line(aes(x=From1, y=Elo), lwd=1) + theme_bw() + ylab("Elo Rating") +
      scale_fill_brewer(palette="Set3") + theme(legend.position = 'none', panel.grid.minor.x = element_line(colour='grey', linetype=1)) + ggtitle(input$club) + 
      scale_x_date('', date_breaks = '3 years', date_minor_breaks = '6 months', date_labels = '%Y') 
  })
  # output$managerPlot <- renderPlot({
  #   mgrelo %>%
  #     filter(Name==input$manager & From1 >= input$startDate) %>%
  #     mutate(From=pmax(From, input$startDate), Mid=From + as.numeric(To-From, units='days')/2) %>%
  #     ggplot() + 
  #     geom_rect(
  #       data=. %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, AvgElo, Index), 
  #       aes(xmin=From, xmax=To, ymin=MinElo, ymax=MaxElo, fill=factor(Index%%9+1)), alpha=0.5) + 
  #     geom_text(
  #       data=. %>% distinct(Name, Club, From, To, MinElo, MaxElo, Mid, AvgElo),
  #       aes(x=Mid, y=AvgElo, label=Club)) + 
  #     geom_vline(
  #       data=. %>% filter(Relegation), 
  #       aes(xintercept=From1), 
  #       col='red'
  #     ) + 
  #     geom_vline(
  #       data=. %>% filter(Promotion), 
  #       aes(xintercept=From1), 
  #       col='green'
  #     ) + 
  #     geom_line(aes(x=From1, y=Elo, group=Club)) + theme_bw() + xlab('') + ylab("Elo Rating") +
  #     scale_fill_brewer(palette="Set3") + theme(legend.position = 'none') + ggtitle(input$manager)
  #   
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

