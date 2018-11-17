rm(list = ls())

library(leaflet);
library(rgdal);
library(raster)
library(adehabitatHR);
library(dplyr);
library(RColorBrewer);
library(gridExtra)


ATN_data <- read.csv("atnPacific_withUTMs.csv", header = TRUE, stringsAsFactors = FALSE)
# unique(ATN_data$species)

plot.mcp <- function(species, individuals, conf){
  
  focalSp <- ATN_data[ATN_data$species == species , ]
  focalSp <- na.omit(focalSp)
  
  ## if user selects more individuals than there are in the dataset, return data for all individuals
  if (length(unique(focalSp$serialNumber)) >= individuals){
    individuals <- individuals
  }else{
    individuals <- length(unique(focalSp$serialNumber))
  }
  ## return individuals with most data
  sequence <- unique(focalSp$serialNumber)[order(table(focalSp$serialNumber), decreasing = TRUE)][1:individuals]
  indiv <- subset(focalSp, focalSp$serialNumber %in% sequence)

  ## calculate MCP per individual, per zone
  for ( i in sequence ){
    for ( j in unique(indiv$UTMzone) ){
      indiv.zone <- indiv[indiv$serialNumber == i & indiv$UTMzone == j ,  ]
      
      if(nrow(indiv.zone) >= 5){
        idsp <- data.frame(indiv.zone$serialNumber)
        coordinates(idsp) <- cbind(indiv.zone$UTM.east, indiv.zone$UTM.north)
        
        focalSp.mcp <- mcp(idsp, percent = conf, unin = 'm', unout = 'km2')
        proj4string(focalSp.mcp) <- CRS(paste0("+proj=utm +zone=", j ))
        mcp_poly <- spTransform(focalSp.mcp, CRS("+proj=longlat +datum=WGS84")) }
      
        if ( i == sequence[1] ){
        indivPolys <- mcp_poly
         }else{
        indivPolys <- bind(indivPolys, mcp_poly)}
    } 
  }
  
  ## bind them back together by individual
  for ( k in sequence ){
    indivPoly <- aggregate(indivPolys[indivPolys$id == k , ])
    if ( k == sequence[1] ){
      polyToPlot <- indivPoly
    }else{
      polyToPlot <- bind(polyToPlot, indivPoly)
    } 
  } 


  polyToPlot$id <- sequence
  
  if (length(sequence) <= 12){
    pal <- brewer.pal(length(sequence), "Paired")
  }else{
    extra <- length(sequence) - 12
    pal <- c(brewer.pal(12, "Paired"),  brewer.pal(extra, "Paired") )
  }
  
  leaflet(polyToPlot) %>% addTiles() %>% addPolygons(weight = 3, opacity = 0.5, color = pal , fillColor = pal)  %>%  addLegend('bottomleft', colors = pal, labels = sequence,  title = c(unique(focalSp$species)) )
}


plot.mcp("Leatherback Sea Turtle", 10, 90)

plot.mcp("Laysan Albatross", 5, 75)

plot.mcp("Northern Elephant Seal", 12, 50)
