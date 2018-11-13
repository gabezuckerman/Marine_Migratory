rm(list = ls())

setwd('/Users/ewesteen/Dropbox/MMA')

library(leaflet);
library(rgdal);
library(raster)
library(adehabitatLT);
library(adehabitatHR);
library(dplyr);
library(RColorBrewer)


ATN_data <- read.csv("all_ATN.csv", header = TRUE, stringsAsFactors = FALSE)
ATN_data <- na.omit(ATN_data)

## unique(ATN_data$commonName)

focalSp <- ATN_data[ATN_data$commonName == "Shortfin Mako Shark" , ]
focalSp <- na.omit(focalSp)
focalSp$time <- gsub("T" , "_", focalSp$time)
focalSp$time <- gsub("Z" , "", focalSp$time)
focalSp$time <- as.POSIXct(strptime(focalSp$time,format="%Y-%m-%d_%H:%M:%S"))
indivs <- unique(focalSp$serialNumber)

## use longitude to get utm zone


focalSp$longitude <- as.numeric(focalSp$longitude) 
focalSp$longitude[focalSp$longitude > 180] <- focalSp$longitude[focalSp$longitude > 180] - 360 


UTMzone <- floor((focalSp$longitude + 180) / 6) + 1

focalSp$UTMzone <- UTMzone

focalSp$UTM.east <- NA
focalSp$UTM.north <- NA


for ( xx in 1 : nrow(focalSp)) {
	
	xy <- data.frame(X = as.numeric(focalSp$longitude[xx]), Y = as.numeric(focalSp$latitude[xx]))
	coordinates(xy) <- c("X", "Y")
	proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
	
	res <- spTransform(xy, CRS(paste0("+proj=utm +zone=", focalSp$UTMzone[xx], " ellps=WGS84")))
	
	focalSp[xx, c("UTM.east", "UTM.north")] <- coordinates(res)
	
}


write.csv(focalSp, file = "MakoShark")


##indivs <- unique(focalSp$serialNumber)
##order <- order(table(focalSp$serialNumber))
##sequence <- order[1 : 6]

sequence <- c(1:5)

for ( i in sequence ){
	
	indiv <- subset(focalSp, focalSp$serialNumber == indivs[i])
	ptsMatrix <- matrix(data = c(indiv$UTM.east, indiv$UTM.north), ncol = 2 )	
	res <- SpatialPoints(coords = ptsMatrix, proj4string = CRS(paste0("+proj=utm +zone=", median(indiv$UTMzone) ))) 

	kd <- kernelUD(res, h = "href", grid = 1000, same4all = TRUE, kern = "bivnorm" )
	homerange <- getverticeshr(kd, percent = 75)
	
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




