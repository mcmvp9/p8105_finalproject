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

# Import Dataset
world_cup = read_csv("./12_4_dataset.csv") %>%
   janitor::clean_names() %>%
         select(country,w, pld, d, rank, gf, ga) 

# 
country = world_cup %>% distinct(country) %>% pull()

#  min_w = world_cup %>% distinct(w) %>% min()
 # min_pld = world_cup %>% distinct(pld) %>% min()
 # min_d = world_cup %>% distinct(d) %>% min()
 # min_gf = world_cup %>% distinct(gf) %>% min()
 # min_ga = world_cup %>% distinct(ga) %>% min()

 # max_w = world_cup %>% distinct(w) %>% max()
 # max_pld = world_cup %>% distinct(pld) %>% max()
 # max_d = world_cup %>% distinct(d) %>% max()
 # max_gf = world_cup %>% distinct(gf) %>% max()
 # max_ga = world_cup %>% distinct(ga) %>% max()
  
# Define UI for a
ui <- fluidPage(
  titlePanel("2022 FIFA WORLD CUP PREDICTION TOOL"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput(
               "country_choice", 
               label = h3("Select Country"),
               choices = country),
             
             # sliderInput widget for pld
             sliderInput(
               "number_games_range", 
               label = h3("Choose Number of Matches Played"), 
               min = 0, max = 7, value = c(7)),
             
             # sliderInput widget for d 
             sliderInput(
               "draw_games_range", 
               label = h3("Choose draw games range"), 
               min = 0, max = 7, value = c(2)),
             
             # sliderInput widget for gf
             sliderInput(
               "goals_for_range", 
               label = h3("Choose goals for range"), 
               min = 0, max = 20, value = c(10)),
             
             # sliderInput widget for ga 
             sliderInput(
               "goals_against_range", 
               label = h3("Choose goals against range"), 
               min = 0, max = 15, value = c(0)),
           )
    ),
    
    mainPanel(
            tableOutput("table"), 
            plotOutput("plot"),
            span(textOutput("result"), style="font-size:26px", align="center"
            ),
            #textOutput("result"),
            imageOutput("home_img")
      
      
          )

  # ),
  # span(textOutput("result"), style="font-size:26px", align="center"
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
             d = input$draw_games_range[1],
             gf = input$goals_for_range[1],
             ga = input$goals_against_range[1],
          
  prediction = -1.56 + 0.660*pld - 0.628*d + 0.0159*rank + 0.154*gf - 0.224*ga) %>% 
    
      select(-w) %>% 
      
        rename( "Country" = country,
                "Matches Played" = pld,
                "Matches Drawn" = d,
                "FIFA Rankings 2022" = rank,
                "Goals Scored" = gf,
                "Goals Against" = ga,
               "Predicted Games Won" = prediction
               )
    
  })
  
  output$plot <- renderPlot({
    hist(world_cup$w, breaks = 12, xlab = "Number of Winning Games", main = "Histogram of number of Winning Games for all countries")
    abline(v=-1.56 + 0.660*input$number_games_range[1] - 0.628*input$draw_games_range[1] + 0.0159*world_cup$rank[country==input$country_choice] + 0.154*input$goals_for_range[1] - 0.224*input$goals_against_range[1], col="blue")
  })
  output$result <- renderText({
    paste0("The predicted matches won by ", input$country_choice  , "is : ", as.character(round(-1.56 + 0.660*input$number_games_range[1] - 0.628*input$draw_games_range[1] + 0.0159*world_cup$rank[country==input$country_choice] + 0.154*input$goals_for_range[1] - 0.224*input$goals_against_range[1],2)))
  })
  output$home_img <- renderImage({
    
    list(src = "qatar.png",
         width = 600,
         height = 330,
         style="display: block; margin-left: auto; margin-right: auto;")
    
  }, deleteFile = F)
}

# Run the application 
shinyApp(ui = ui, server = server)