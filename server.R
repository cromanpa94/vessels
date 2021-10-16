library(dplyr)
library(feather)
library(leaflet)
library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)
library(geosphere)
library(DT)
library(htmltools)
library(rgeos)
library(sf)
library(fasterize)
library(raster)
library(spData)

# read data
data <- feather::read_feather('ships.feather')
data <- data %>%dplyr::filter(is_parked == 0 )
source('functions.R')

server <- function(input, output, server) {
  
  output$vt_selected = renderUI({
    selectInput("vt_selected", label = h4("Vessel type"), 
                choices = c("", sort(unique(data$ship_type))), selected = NULL)
  })
  
  observeEvent(input$vt_selected,{
    output$vessels_selected = renderUI({
      vessels <- data %>%
        filter(ship_type == input$vt_selected) %>%
        distinct(SHIPNAME) %>% pull(SHIPNAME)
      
      selectInput(inputId = "vessels_selected", #name of input
                  label = h4("Vessel"), #label displayed in ui
                  choices = c("", sort(unique(vessels))), #calls list of available cities
                  selected = NULL)
    })
  })
  
  filteredData <- reactive({
    summarizeDistance(data = data, 
                      ShipType = input$vt_selected, 
                      VesselName = input$vessels_selected,
                      linear = input$method)
  })
  
  shiny::observeEvent(input$vessels_selected, {
    output$Map <- renderLeaflet({
      leaflet(filteredData()) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addProviderTiles('Esri.WorldStreetMap', group = 'Esri') %>%
        addProviderTiles('CartoDB.Positron', group = 'CartoDB') %>% 
        addMarkers(
          ~LON, ~LAT, label = ~htmlEscape( paste0('Observation: ',id, '; Time: ',DATETIME) ),
          labelOptions = labelOptions(noHide = T)
        ) %>%
        addLayersControl(
          baseGroups = c('OSM', 'Esri', 'CartoDB'),
          options = layersControlOptions(collapsed = TRUE)
        ) %>%
        addMiniMap(toggleDisplay = TRUE,
                   position = "bottomleft")
    })
  })
  

  observeEvent(req(input$vessels_selected != ''), {
    
    output$distance <-
      renderText({ 
      paste("In our data set, the ",input$vessels_selected,
            " (a",input$vt_selected ," vessel) ", 
            "traveled for a maximum of ", 
            round(filteredData()$DISTANCE[1],2), 
            "m. This trip happened between",
            filteredData()$DATETIME[1], " and ",
            filteredData()$DATETIME[1], ".")
    })
  })
  
  observeEvent(req(input$method != 'Yes'), {
  world <- st_read(system.file("shapes/world.gpkg", package="spData"))
  })
  
}
