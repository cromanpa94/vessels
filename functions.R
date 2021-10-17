DatasetToFeather <- function(fileName = 'ships'){
  data <- as_tibble(fread(paste0(fileName, '.csv'), header = T))
  write_feather(data, paste0(fileName, '.feather'))
}

summarizeDistance <- function(data, 
                              ShipType = input$vt_selected, 
                              VesselName=input$vessels_selected,
                              linear = input$method){
  target_obs <- data %>%
    dplyr::filter(ship_type == ShipType & SHIPNAME == VesselName ) %>%
    dplyr::arrange(DATETIME) %>% 
    dplyr::mutate(id = row_number())
  
  target_obs <- getDistance(data = target_obs, 
                            targetcolumns = c("LON", "LAT"), 
                            linear = linear)
  
  mostRecent <- target_obs %>%
    dplyr::slice(which.max(DISTANCE))  %>%
    dplyr::filter(row_number() == n()) 
  
  mostRecent <- rbind(mostRecent, target_obs[target_obs$id ==  (mostRecent$id + 1),])
  obs <- list(mostRecent=mostRecent, target_obs= target_obs)
  return(obs)
}


getDistance <- function(data, targetcolumns = c("LON", "LAT"), 
                        linear = "Yes"){
  if( linear == 'Yes' ){
    data$DISTANCE <- sapply(seq_len(nrow(data)), function(x){ 
      objDist <- distm(data[x,targetcolumns], data[(x + 1),targetcolumns])
    })}else{
      pts2<-st_as_sf(data[,c("LON", "LAT")], coords = c("LON", "LAT"),
                     crs = 'WGS84')
      r <- raster(extent(pts2), resolution=0.01)
      crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      rtas <- fasterize(summarize(world), r)
      
      data$DISTANCE <- sapply(1:nrow(data), function(x){
        rtas_pts <- rtas
        xy <- st_coordinates(pts2[c(x,x+1),])
        icell <- cellFromXY(rtas, xy)
        rtas_pts[icell[1]] <- 2
        tryCatch({
        d <- gridDistance(rtas_pts, origin = 2,omit = 1)
        d[icell[2]]
        }, error=function(e){0})
      })
    }
  return(data)
}