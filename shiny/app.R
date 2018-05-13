#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinycssloaders)
library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(sf)

fin_muni <- sf::read_sf("../data/kuntajako_2017_maa_alueet.geojson") %>%
  dplyr::mutate(check = NAMEFIN == "Jyväskylä") %>% 
  dplyr::select(check)

check_colors <-c("TRUE" = "#A3A3A3", "FALSE" = "#595490")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Magpie!"
  ),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
  fluidRow(
    column(width = 3,
           box(width = NULL, status = "warning",
               uiOutput("routeSelect"),
               checkboxGroupInput("directions", "Show",
                                  choices = c(
                                    Northbound = 4,
                                    Southbound = 1,
                                    Eastbound = 2,
                                    Westbound = 3
                                  ),
                                  selected = c(1, 2, 3, 4)
               ),
               p(
                 class = "text-muted",
                 paste("Note: a route number can have several different trips, each",
                       "with a different path. Only the most commonly-used path will",
                       "be displayed on the map."
                 )
               ),
               actionButton("zoomButton", "Zoom to fit buses")
           ),
           box(width = NULL, status = "warning",
               selectInput("interval", "Refresh interval",
                           choices = c(
                             "30 seconds" = 30,
                             "1 minute" = 60,
                             "2 minutes" = 120,
                             "5 minutes" = 300,
                             "10 minutes" = 600
                           ),
                           selected = "60"
               ),
               uiOutput("timeSinceLastUpdate"),
               actionButton("refresh", "Refresh now"),
               p(class = "text-muted",
                 br(),
                 "Source data updates every 30 seconds."
               )
           )
      ),
      column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               withSpinner(leafletOutput("munimap", height = 800), type = 8)
           )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$munimap <- renderLeaflet({
    
    check_pal <- colorFactor(check_colors, names(check_colors))
    
    map <- leaflet(fin_muni) %>%
      addProviderTiles(providers$Hydda.Full) %>% 
      #addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png') %>%
      addPolygons(
        fillColor = ~check_pal(check),
        weight = 0.5,
        opacity = 0.8,
        color = "white",
        #dashArray = "3",
        fillOpacity = 0.8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

