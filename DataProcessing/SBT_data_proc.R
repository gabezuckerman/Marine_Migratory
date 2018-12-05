library(lubridate)
library(maps)
library(dplyr)
library(scales)
library(RColorBrewer)

setwd('/home/ben/Desktop/MMA_Data/SeaBirdTracking/')
seabird_files <- list.files()

all_seabird <- read.csv(seabird_files[2], stringsAsFactors = F)

for (i in 3:length(seabird_files)) all_seabird <- rbind(all_seabird, read.csv(seabird_files[i], stringsAsFactors = F))

extract_individuals_sbt <- function(dataset) {
  individuals <- unique(dataset$track_id)
  output <- list()
  for (i in 1:length(individuals)) {
    this_one <- dataset[dataset$track_id == individuals[i],]
    output[[i]] <- this_one
  }
  return(output)
}

inds <- extract_individuals_sbt(all_seabird)


shift <- 200
plot.map("world", center = shift, col = "white", bg = "gray96", 
         fill = TRUE, ylim = c(-60,90), mar = c(0,0,0,0))

for (x in 1:length(inds)) {
  inds[[x]] <- inds[[x]] %>% arrange(date_gmt)
  data_to_plot <- inds[[x]]
  # data_to_plot$longitude <- data_to_plot$longitude + shift
  data_to_plot$longitude[inds[[x]]$longitude < 0] <- data_to_plot$longitude[inds[[x]]$longitude < 0] + shift
  data_to_plot$longitude[inds[[x]]$longitude > 0] <- data_to_plot$longitude[inds[[x]]$longitude > 0] + (shift-360)
  points(data_to_plot$longitude, data_to_plot$latitude, type="l", col=alpha("maroon4", 0.1))
  # points(data_to_plot$lon_colony + shift, data_to_plot$lat_colony, pch=16, col = "limegreen")
}