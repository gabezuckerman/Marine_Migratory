setwd('/Users/ewesteen/Dropbox/MMA')

rm(list = ls())

library(raster);
library(rgdal);
library(adehabitatLT);
library(adehabitatHR);
library(BBMM);
library(move);
library(ggplot2);
library(leaflet)


ATN_data <- read.csv("all_ATN.csv", header = TRUE, stringsAsFactors = FALSE)
ATN_data <- na.omit(ATN_data)

focalSp <- ATN_data[ATN_data$commonName == "Loggerhead Sea Turtle", ]
focalSp <- na.omit(focalSp)
focalSp$time <- gsub("T" , "_", focalSp$time)
focalSp$time <- gsub("Z" , "", focalSp$time)

focalSp$time <- as.POSIXct(strptime(focalSp$time,format="%Y-%m-%d_%H:%M:%S"))


focalSp <- focalSp[order(focalSp$time), ]


timediff <- diff(focalSp$time)
focalSp <- focalSp[-1,]
focalSp$TimeDiff <- abs(as.numeric(timediff))

move.df <- data.frame(ID = focalSp$serialNumber, TimeDiff = focalSp$TimeDiff, Burst = focalSp$time )

long <- as.numeric(focalSp$longitude)
lat <- as.numeric(focalSp$latitude)
xy <- matrix(data = NA, ncol = 2, nrow = length(long))
xy[, 1] <- long
xy[, 2] <- lat

coordinates(move.df) <- xy
ltraj <- as.ltraj(coordinates(move.df), move.df$Burst, id = move.df$ID)
plot(ltraj)


#########################################################
## brownian bridge simulation for a single individual

turt <- subset(focalSp, focalSp$serialNumber == unique(focalSp$serialNumber[1]))
xy <- data.frame(X = as.numeric(turt$longitude), Y = as.numeric(turt$latitude))
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
res <- spTransform(xy, CRS("+proj=utm +zone=11 ellps=WGS84"))



turt.BBMM = brownian.bridge(x= as.numeric(res$X), y= as.numeric(res$Y), time.lag= (turt$TimeDiff / 60), location.error = 20  , cell.size=100)
bbmm.summary(turt.BBMM)

contours <- bbmm.contour(turt.BBMM, levels=c(50, 90, 95, 99), plot=TRUE)

plot(subset$X, subset$Y)
points(subset$X, subset$Y, type = "l")

xyz <- data.frame(turt.BBMM$x, turt.BBMM$y, turt.BBMM$probability)
bbmm.map <- rasterFromXYZ(xyz)

colScheme <- colorRampPalette(c("#0571b0", "#92c5de", "white", "orange", "red3"), bias=3)
plot(bbmm.map, col=colScheme(100))

#########################################################
## dynamic brownian bridge

head(turt)

indiv.move <- move(x=res$X, y=res$Y, time=turt$time, proj=CRS("+proj=utm +zone=11 ellps=WGS84"), data=turt, animal=turt$serialNumber)

dbbmm <- brownian.bridge.dyn(object=indiv.move, location.error=22, margin=7, window.size=19, ext = c(788997 , 1105414 , 1105414 , 2935019 ))


plot(indiv.move)








