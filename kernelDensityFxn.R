rm(list = ls())


library(leaflet);
library(rgdal);
library(raster)
library(adehabitatLT);
library(adehabitatHR);
library(dplyr);
library(RColorBrewer)


ATN_data <- read.csv("atnPacific_withUTMs.csv", header = TRUE, stringsAsFactors = FALSE)


kernelDensity <- function(species, individuals, conf){
  
  focalSp <- ATN_data[ATN_data$species  == species , ]
  focalSp <- na.omit(focalSp)
  focalSp$time <- gsub("T" , "_", focalSp$time)
  focalSp$time <- gsub("Z" , "", focalSp$time)
  focalSp$time <- as.POSIXct(strptime(focalSp$time,format="%Y-%m-%d_%H:%M:%S"))
  indivs <- unique(focalSp$serialNumber)
  
  sequence <- 1 : individuals
  
  for ( i in sequence){
    
    indiv <- subset(focalSp, focalSp$serialNumber == indivs[i])
    ptsMatrix <- matrix(data = c(indiv$UTM.east, indiv$UTM.north), ncol = 2 )	
    res <- SpatialPoints(coords = ptsMatrix, proj4string = CRS(paste0("+proj=utm +zone=", median(indiv$UTMzone) ))) 
    
    kd <- kernelUD(res, h = "href", grid = 1000, same4all = TRUE, kern = "bivnorm" )
    homerange <- getverticeshr(kd, percent = conf)
    
    if (i == sequence[1]) {
      
      indivPolys <- homerange
      
    }else{
      
      indivPolys <- rbind(indivPolys, homerange)	
      
    }
  }
  
  
  sdf_poly <- spTransform(indivPolys, CRS("+proj=longlat +datum=WGS84")) 
  sdf_poly$id <- indivs[sequence]
  pal <- brewer.pal(length(sequence), "Paired")
  
  
  leaflet(sdf_poly) %>% addTiles() %>% addPolygons(weight = 3, opacity = 0.5, color = pal , fillColor = pal)  %>%  addLegend('bottomleft', colors = pal, labels = sdf_poly$id,  title = c(unique(focalSp$commonName)) )

  
  
}

  
  
  
  
  
  
  
  