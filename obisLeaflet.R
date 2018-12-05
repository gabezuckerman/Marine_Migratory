library(robis)
library(mapview)
library(sf)
library(tidyverse)
library(ggmap)
library(leaflet)
library('rphylopic')
# load data
obis_batch <- function(list_of_species) {
  species_data <- list()
  for (i in 1:length(list_of_species)) {
    species_data[[i]] <- occurrence(list_of_species[[i]])
  }
  return(bind_rows(species_data))
}

loggerhead <- obis_batch("Caretta caretta")

#functions that help with keeping only instances in the Pacific Ocean
inPacific <- function(long, lat) {
  if (long <= 290 && long >= 280 && lat >= -80 && lat <= 9) {
    return(TRUE)
  }
  else if(long <= 280 && long >= 276 && lat >= -80 && lat <= 9) {
    return(TRUE)
  }
  else if(long <= 276 && long >= 270 && lat >= -80 && lat <= 14) {
    return(TRUE)
  }
  else if(long <= 270 && long >= 260 && lat >= -80 && lat <= 18) {
    return(TRUE)
  }
  else if(long <= 260 && long >= 145 && lat >= -80 && lat <= 66) {
    return(TRUE)
  }
  else if(long <= 145 && long >= 100 && lat >= 0 && lat <= 66) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

shift <- function(longitude) {
  if (longitude < 0) {
    return(longitude + 360)
  }
  else {
    return(longitude)
  }
}

pacificProcessing<- function(obisTable) {
  #reformats longitude
  obisTable$decimalLongitude <- as.numeric(map(obisTable$decimalLongitude, shift))
  #keeps rows in pacific determined by lon, lat
  pacific <- map2(obisTable$decimalLongitude, obisTable$decimalLatitude, ~inPacific(.x, .y))
  obisTable$inPO <- pacific
  obisTable <- filter(obisTable, inPO == TRUE)
  return(obisTable)
}




#instances where body of water is pacific
pacificLoggerhead <- pacificProcessing(loggerhead)


turtleIcon <- makeIcon("seaturtle.jpg", "seaturtle@2x.jpg", 18, 18)



m <- leaflet(data = pacificLoggerhead) %>% addTiles() %>%
  addMarkers(~decimalLongitude, ~decimalLatitude, 
             popup = ~as.character(species),
             label = ~as.numeric(individualCount), 
             icon = turtleIcon)  %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  setView(lng = 180, lat = 0, zoom = 1)


#creates a leaflet object from an obis table
pacificMap <- function(obisTable) {
  p <- pacificProcessing(obisTable)
  m <- leaflet(data = p) %>% addTiles() %>%
    addMarkers(~decimalLongitude, ~decimalLatitude, 
               popup = ~as.character(species),
               label = ~as.numeric(individualCount)) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    setView(lng = 180, lat = 0, zoom = 1)
  return(m)
}

pacificMap(loggerhead)

