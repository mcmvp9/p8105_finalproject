library(shiny)
library(flexdashboard)
library(tidyverse)
library(dplyr)
library(readr)
library(shiny)
library(shinyWidgets)
library(plotly)
library(viridis)
library(rsconnect)

world_cup = read_csv("./12_4_dataset.csv") %>%
  janitor::clean_names() %>%
  select(country,w, pld, d, rank, gf, ga) 
country = world_cup %>% distinct(country) %>% pull()
min_w = world_cup %>% distinct(w) %>% min()
min_pld = world_cup %>% distinct(pld) %>% min()
min_d = world_cup %>% distinct(d) %>% min()
min_gf = world_cup %>% distinct(gf) %>% min()
min_ga = world_cup %>% distinct(ga) %>% min()

max_w = world_cup %>% distinct(w) %>% max()
max_pld = world_cup %>% distinct(pld) %>% max()
max_d = world_cup %>% distinct(d) %>% max()
max_gf = world_cup %>% distinct(gf) %>% max()
max_ga = world_cup %>% distinct(ga) %>% max()
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Movie explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             selectInput(
               "country_choice", 
               label = h3("Select country"),
               choices = country),
             
             # sliderInput widget for pld
             sliderInput(
               "number_games_range", 
               label = h3("Choose number games range"), 
               min = 0, max = max_pld, value = c(113)),
             
             # sliderInput widget for d 
             sliderInput(
               "draw_games_range", 
               label = h3("Choose draw games range"), 
               min = min_d, max = max_d, value = c(22)),
             
             # sliderInput widget for gf
             sliderInput(
               "goals_for_range", 
               label = h3("Choose goals for range"), 
               min = min_gf, max = max_gf, value = c(236)),
             
             # sliderInput widget for ga 
             sliderInput(
               "goals_against_range", 
               label = h3("Choose goals against range"), 
               min = min_ga, max = max_ga, value = c(130)),
           )
    ),
    mainPanel(
            tableOutput("table")
          )
    
  )
)
#   fluidPage(
#   
#   # Application title
#   titlePanel("Predict the winning number of games for World Cup 2022"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     
#     
#     # selectInput widget
#     selectInput(
#       "country_choice", 
#       label = h3("Select country"),
#       choices = country),
#     sliderInput(
#       "number_games_range", 
#       label = h3("Choose number games range"), 
#       min = 0, max = max_pld, value = c(113)),
#     # Show a plot of the generated distribution
#     mainPanel(
#       tableOutput("table")
#     )
#   )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- renderTable({
    # generate bins based on input$bins from ui.R
    world_cup %>%
      filter(
        country == input$country_choice) %>%
      mutate(pld = input$number_games_range[1],
             prediction = -1.56 + 0.660*pld - 0.628*d + 0.0159*rank + 0.154*gf - 0.224*ga) %>% 
      select(-w) %>% 
      rename("Predicted Games Won"=prediction)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)