library(robis)
library(mapview)
library(sf)
library(tidyverse)
library(ggmap)
library(leaflet)
# load data
obis_batch <- function(list_of_species) {
  species_data <- list()
  for (i in 1:length(list_of_species)) {
    species_data[[i]] <- occurrence(list_of_species[[i]])
  }
  return(bind_rows(species_data))
}

loggerhead <- obis_batch("Caretta caretta")


#instances where body of water is pacific
pacific <- loggerhead %>% filter(grepl("pacific", waterBody, ignore.case = T))
meanX <- mean(pacific$decimalLongitude)
meanY <- mean(pacific$decimalLatitude)


unique(pacific$collectionCode)


turtleIcon <- makeIcon("turtle.jpg", 18, 18)



m <- leaflet(data = pacific) %>% addTiles() %>%
  addMarkers(~decimalLongitude, ~decimalLatitude, 
             popup = ~as.character(species),
             label = ~as.numeric(individualCount), 
             icon = turtleIcon)  %>%
  addProviderTiles(providers$Esri.OceanBasemap)

setView(map = m, lng = -8.7, lat = 124.5, zoom = 1)


