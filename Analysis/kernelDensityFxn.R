rm(list = ls())


library(leaflet);
library(rgdal);
library(raster)
library(adehabitatHR);
library(dplyr);
library(RColorBrewer)


ATN_data <- read.csv("atnPacific_withUTMs.csv", header = TRUE, stringsAsFactors = FALSE)
# unique(ATN_data$species)

plot.kernelDensity <- function(species, individuals, conf){
  
  focalSp <- ATN_data[ATN_data$species == species , ]
  focalSp <- na.omit(focalSp)
  
  sequence <- unique(focalSp$serialNumber)[order(table(focalSp$serialNumber), decreasing = TRUE)][1:individuals]
  indiv <- subset(focalSp, focalSp$serialNumber %in% sequence)

  idsp <- data.frame(indiv$serialNumber)
  coordinates(idsp) <- cbind(indiv$UTM.east, indiv$UTM.north)
  
  kd <- kernelUD(idsp, h = "href", grid = 10000, same4all = TRUE, kern = "bivnorm" )
  homerange <- getverticeshr(kd, percent = conf)
    
  proj4string(homerange) <- CRS(paste0("+proj=utm +zone=", median(indiv$UTMzone)))
                                      
  sdf_poly <- spTransform(homerange, CRS("+proj=longlat +datum=WGS84")) 
  sdf_poly$id <- sequence
  pal <- brewer.pal(length(sequence), "Paired")
  
  leaflet(sdf_poly) %>% addTiles() %>% addPolygons(weight = 3, opacity = 0.5, color = pal , fillColor = pal)  %>%  addLegend('bottomleft', colors = pal, labels = sdf_poly$id,  title = c(unique(focalSp$commonName)) )
  
}

plot.kernelDensity("Blue Whale", 4, 90)
  
plot.kernelDensity("Leatherback Sea Turtle", 4, 90)

  
  
