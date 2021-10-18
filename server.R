

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
                      method = input$method)
  })
  
  
  output$Map <- renderLeaflet({
    leaflet() %>% addTiles()%>%
      addProviderTiles(providers$Esri.WorldPhysical)
  })
  
  shiny::observeEvent(req(input$vessels_selected != '' ), {
    tpoints <- filteredData()[[1]]
    allpoints <- filteredData()[[2]]

    labels_ag <- lapply(seq(nrow(tpoints)), function(i) {
      paste0(
        '<b>', 'Basic information','</b><br>',
        'Vessel: ', 
        tpoints[i, "SHIPNAME"][[1]],'</b><br>',
        'Type: ',
        tpoints[i, "ship_type"][[1]],
        '<br>', 'Date: ',
        tpoints[i, "DATETIME"][[1]]
      )
    })
    
    leafletProxy("Map", data = allpoints)%>%
      clearMarkers() %>%
      clearShapes() %>%
      addAwesomeMarkers(data = tpoints,
                   lng = ~ LON, lat = ~ LAT,
                   label = lapply(labels_ag, htmltools::HTML),
                   icon = makeAwesomeIcon(
                     icon = "ship",
                     markerColor = "darkblue",
                     library = "fa",
                     iconColor = "#FFFFFF"
                   )) %>%
        addMiniMap(toggleDisplay = TRUE,
                   position = "bottomleft") %>% 
    flyToBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))%>% 
      addPolylines(data = tpoints,
                   lng= ~ LON,
                   lat= ~ LAT,
                   color= 'red') %>% 
      addPolylines(data = allpoints,
                   lng= ~ LON,
                   lat= ~ LAT,
                   color= 'blue') 
    })

  

  observeEvent(req(input$vessels_selected != ''), {
    
    output$distance <-
      renderText({ 
      paste("In our data set, the ",input$vessels_selected,
            " (a",input$vt_selected ," vessel) ", 
            "traveled for a maximum of ", 
            round(filteredData()[[1]]$DISTANCE[1],2), 
            "m. This trip happened between",
            filteredData()[[1]]$DATETIME[1], " and ",
            filteredData()[[1]]$DATETIME[1], ".")
    })
  })
  
  observeEvent(req(input$method == 'Raster'), {
  world <- st_read(system.file("shapes/world.gpkg", package="spData"))
  })
  
}
