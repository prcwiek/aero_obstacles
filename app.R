# Map of aeronautical obstacles
#
# https://www.ais.pansa.pl/en/publications/etod/
#
# file 252 dated 2022-06-16

library(dplyr)
library(DT)
library(leaflet)
library(readr)
library(shiny)
library(shinyjs)
library(stringr)

load("data/Obstacles_2022-06-16.rda")

do$latitude <- round(do$latitude, 6)
do$longitude <- round(do$longitude, 6)

# reference points of Polish borders
n_point <- 54.835778
s_point <- 49.0025
e_point <- 24.15
w_point <- 14.11

# obstacle list
obstacle_list <- sort(unique(do$Obstacle_type))

# Define UI for application that draws a histogram
ui <- fluidPage(

  
  theme = bslib::bs_theme(version = 3, bootswatch = "superhero"),
  
  shinyjs::useShinyjs(),
  
  br(),
  
  fluidRow(
    column(3),
    column(6, htmlOutput("about")),
    column(3)
  ),
  
  hr(),
  
  fluidRow(
    column(1),
    column(3, align = "bottom",
           selectInput("obstacle", "Obstacle type:",
                       choices = obstacle_list,
                       selected = c("Measuring mast"),
                       multiple = FALSE)),
    column(8, align = "center", leafletOutput("mastobstacles", height=1000, width = 1000)),
  ),
  
  hr(),
  
  fluidRow(
    column(1),
    column(10, 
           dataTableOutput("doobstacles")),
    column(1)
  )
)

server <- function(input, output, session) {

  dos <- reactiveValues(data = NULL)
  
  observeEvent(input$obstacle, {
    req(input$obstacle)
    dos$data <- do %>% filter(input$obstacle == Obstacle_type)

  })
  
  # dos$data <- reactive({
  #   req(input$obstacle)
  #   do %>% filter(input$obstacle == Obstacle_type)
  # })
  
  output$mastobstacles <- renderLeaflet({
    
    fpopup <- paste0("E ", round(dos$data$longitude, 6), "<br> N ", round(dos$data$latitude, 6), "<br>",
                     "Height: ", dos$data$Height, " ", dos$data$Height_Uom, "<br>",
                     "Location: ", dos$data$Location, "<br>",
                     "Obstacle type: ", dos$data$Obstacle_type)
    
    leaflet(dos$data) %>%
      fitBounds(lng1 = e_point, lat1 = n_point, lng2 = w_point, lat2 = s_point) %>%
      addCircleMarkers(radius = 4, color = "red", popup = ~fpopup) %>%
      addProviderTiles(providers$OpenStreetMap)
    
  })
  
  output$doobstacles <- renderDT({
    datatable(dos$data, style = "bootstrap4",
              options = list(paging = TRUE, pageLength = 25))
  }, server = FALSE)
  
  output$about <- renderText({
    paste0('<center><h3>Measurement masts in Poland</h3></center><br>
      <p><u>Information about the source of data:</u> file 252 dated 2022-06-16 from
      PANSA <a href="https://www.ais.pansa.pl/en/publications/etod/">
      https://www.ais.pansa.pl/en/publications/etod/</a><br>
      ')
  })
  
    
  # End application when a window or a tab is closed
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui = ui, server = server)
