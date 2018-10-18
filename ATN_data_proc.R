library(lubridate)
library(maps)
library(dplyr)
library(scales)
library(RColorBrewer)

#-------------Function definitions-------------#

# Takes a df containing all observations of a species.
# Returns a list of dfs, each df being observations for a single individual.
extract_individuals_atn <- function(dataset) {
  individuals <- unique(dataset$toppID)
  output <- list()
  for (i in 1:length(individuals)) {
    this_one <- dataset[dataset$toppID == individuals[i],]
    output[[i]] <- this_one
  }
  return(output)
}

# Function for shifting the map to be centered on the Pacific.
plot.map<- function(database,center,...){
  Obj <- map(database,...,plot=F)
  coord <- cbind(Obj[[1]],Obj[[2]])
  
  # split up the coordinates
  id <- rle(!is.na(coord[,1]))
  id <- matrix(c(1,cumsum(id$lengths)),ncol=2,byrow=T)
  polygons <- apply(id,1,function(i){coord[i[1]:i[2],]})
  
  # split up polygons that differ too much
  polygons <- lapply(polygons,function(x){
    x[,1] <- x[,1] + center
    x[,1] <- ifelse(x[,1]>180,x[,1]-360,x[,1])
    if(sum(diff(x[,1])>300,na.rm=T) >0){
      id <- x[,1] < 0
      x <- rbind(x[id,],c(NA,NA),x[!id,])
    }
    x
  })
  # reconstruct the object
  polygons <- do.call(rbind,polygons)
  Obj[[1]] <- polygons[,1]
  Obj[[2]] <- polygons[,2]
  
  map(Obj,...)
}

# Takes a df of all ATN data and a species name.
# Saves a plot of that species' movement data.
plot_species_atn <- function(atn_data = NULL, species_name, 
                             plot_fn = paste0(gsub(" ", "_", species_name), "_plot.png"),
                             linecol = "maroon4", a = NULL, month = NULL, add = F) {
  if (is.null(atn_data)) {
    atn_data <- read.csv("all_ATN.csv", stringsAsFactors = F)
  }
  if (!(species_name %in% atn_data$commonName)) {
    stop(paste0('No data for species "', species_name, '"'))
  }
  atn_species <- atn_data[atn_data$commonName==species_name,]
  atn_species$time <- ymd_hms(atn_species$time)
  if (!is.null(month)) {
    atn_species <- atn_species[month(atn_species$time) == month,]
  }
  inds <- extract_individuals_atn(atn_species)
  
  if (is.null(a)) { 
    if (nrow(atn_species) < 30000) a <- 1
    else if (nrow(atn_species) < 40000 ) a <- 0.2
    else a <- 0.1
  }
  
  shift <- 200
  shiftATN <- -160
  
  # Start plotting
  # png(plot_fn, width = 1800, height = 900)
  if (!add) plot.map("world", center = shift, col = "white", bg = "gray96", 
                      fill = TRUE, ylim = c(-70,90), mar = c(0,0,0,0))
  
  for (i in 1:length(inds)) {
    coords_to_plot <- inds[[i]]
    coords_to_plot$longitude <- as.numeric(coords_to_plot$longitude)
    coords_to_plot$latitude <- as.numeric(coords_to_plot$latitude)
    coords_to_plot$longitude <- coords_to_plot$longitude + shiftATN
    points(coords_to_plot$longitude, coords_to_plot$latitude, col=alpha(linecol, a), type = "l")
  }
  # title(paste0("ATN data for movement of ", species_name), cex.main=3)
  
  # dev.off()
}

#-------------END Function definitions-------------#

# Read in ATN data
# setwd("/home/ben/Desktop/MMA_Data/ERDDAP_ATN/")
atn_data <- read.csv('all_ATN.csv', stringsAsFactors = F)
atn_species_counts <- read.csv("ATN_spec_cts.csv")

# Plot species with top n most points
atn_species_counts <- atn_species_counts %>% arrange(-n)
n <- 1
colors <- brewer.pal(max(n, 3), "Paired")

for (i in 1:n) {
  plot_species_atn(atn_data=atn_data, species_name=as.character(atn_species_counts$commonName[i]), linecol=colors[i])
}

# Month by month
for (i in 1:12) {
  plot_species_atn(atn_data = atn_data, month = i,
                   species_name = as.character(atn_species_counts$commonName[2]), 
                   linecol=colors[2], plot_fn = paste0(as.character(atn_species_counts$commonName[2]), i, ".png"))
}



## Make a basemap with 5 species

pretty_species <- c("Laysan Albatross", "Leatherback Sea Turtle", "Salmon Shark", 
                    "Northern Elephant Seal","Southern Elephant Seal","Shortfin Mako Shark")
# colors <- brewer.pal(max(length(pretty_species), 3), "Paired")
colors <- c("#33a02c", "#984ea3","#386cb0", "#ffff99", "#f0027f", "#fdc086" )

png(filename = "PrettyMap.png", width = 1800, height = 1200)
for (i in 1:length(pretty_species)) {
  plot_species_atn( atn_data, species_name = pretty_species[i], linecol = colors[i],
                    add = (i != 1), a = 0.17)
}
legend("bottomleft", legend = pretty_species, fill = colors, cex = 2)
title("Sample movement patterns from ATN", cex.main = 4)

dev.off()



