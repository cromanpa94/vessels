DatasetToFeather <- function(fileName = 'ships'){
  data <- as_tibble(fread(paste0(fileName, '.csv'), header = T))
  write_feather(data, paste0(fileName, '.feather'))
}

summarizeDistance <- function(data, ShipType = input$vt_selected, VesselName=input$vessels_selected){
  target_obs <- data %>%
    dplyr::filter(ship_type == ShipType & SHIPNAME == VesselName ) %>%
    dplyr::mutate(DATETIME_f = as.Date(DATETIME, format = "%m/%d/%Y")) %>%
    dplyr::arrange(DATETIME_f) %>% 
    dplyr::mutate(id = row_number())
  
  target_obs <- getDistance(data = target_obs, targetcolumns = c("LON", "LAT"))
  
  mostRecent <- target_obs %>%
    dplyr::slice(which.max(DISTANCE))  %>%
    dplyr::filter(row_number() == n()) 
  
  mostRecent <- rbind(mostRecent, target_obs[target_obs$id ==  (mostRecent$id + 1),])
  return(mostRecent)
}


getDistance <- function(data, targetcolumns = c("LON", "LAT")){
  data$DISTANCE <- sapply(seq_len(nrow(data)), function(x){ 
    distHaversine(data[x,targetcolumns], data[(x + 1),targetcolumns])
  })
  return(data)
}