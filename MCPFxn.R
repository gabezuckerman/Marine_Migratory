rm(list = ls())


library(leaflet);
library(rgdal);
library(raster)
library(adehabitatHR);
library(dplyr);
library(RColorBrewer)


ATN_data <- read.csv("../atnPacific_withUTMs.csv", header = TRUE, stringsAsFactors = FALSE)
# unique(ATN_data$species)



plot.mcp <- function(species, individuals, conf) {
  
  focalSp <- ATN_data[ATN_data$species == species , ]
  focalSp <- na.omit(focalSp)
  
  if (length(unique(focalSp$serialNumber)) >= individuals){
    
    individuals <- individuals
    
  }else{
    
    individuals <- length(unique(focalSp$serialNumber))
      
  }
  
    ## can only compute for individuals with more than 5 observations
    greater5 <- table(focalSp$serialNumber) >= 5
    greater5true <- greater5[greater5 == TRUE]

    sequence <- unique(focalSp$serialNumber)[order(table(focalSp$serialNumber), decreasing = TRUE)][1:individuals]
    sequence <- sequence[sequence %in% names(greater5true)]
  
    indiv <- subset(focalSp, focalSp$serialNumber %in% sequence)
    idsp <- data.frame(indiv$serialNumber)
      
    coordinates(idsp) <- cbind(indiv$UTM.east, indiv$UTM.north)
    
    focalSp.mcp <- mcp(idsp, percent = conf, unin = 'm', unout = 'km2')
      
    proj4string(focalSp.mcp) <- CRS(paste0("+proj=utm +zone=", median(indiv$UTMzone)))
    
    mcp_poly <- spTransform(focalSp.mcp, CRS("+proj=longlat +datum=WGS84")) 
    mcp_poly$id <- sequence
    pal <- brewer.pal(length(sequence), "Paired")
    
    m <- leaflet(mcp_poly) %>% addTiles() %>% addPolygons(weight = 3, opacity = 0.5, color = pal , fillColor = pal)  %>%  addLegend('bottomleft', colors = pal, labels = mcp_poly$id,  title = c(unique(focalSp$commonName)) )
  
    return(m)
}


plot.mcp("Blue Whale", 10, 50)



## doesnt look right~

