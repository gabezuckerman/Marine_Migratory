library(taxize)
library(dplyr)
library(maps)
library(scales)
library(robis)
library(ggplot2)
library(mapdata)

# setwd('/home/ben/Desktop/MMA_Data/OBIS/')


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


plot_species_obis <- function(sci_name = NULL, common_name = NULL, 
                              input_data = chords, plot_fn = "Plot_Default.png") {
  name <- NULL
  if (!is.null(common_name)) {
    spec_data <- input_data[input_data$commonName == common_name,]
    name <- common_name
  } else if (!is.null(sci_name)) {
    spec_data <- input_data[input_data$species == sci_name,]
    name <- sci2comm(get_uid(sci_name)) %>% as.character()
  } else {
    stop("In plot_species_obis, please specify a scientific or common name")
  }
  
  shift <- 200
  data_to_plot <- spec_data
  
  png(plot_fn, width = 1800, height = 900)
  plot.map("world", center = shift, col = "white", bg = "gray96", 
           fill = TRUE, ylim = c(-60,90), mar = c(0,0,0,0))
  
  data_to_plot$decimalLongitude[spec_data$decimalLongitude < 0] <- data_to_plot$decimalLongitude[spec_data$decimalLongitude < 0] + shift
  data_to_plot$decimalLongitude[spec_data$decimalLongitude > 0] <- data_to_plot$decimalLongitude[spec_data$decimalLongitude > 0] + (shift-360)
  
  points(data_to_plot$decimalLongitude, data_to_plot$decimalLatitude, col = alpha("blue", 0.1))
  title(paste0("OBIS data for occurrence of ", name), cex.main=3)
  
  dev.off()
}

# all_data <- rbind(
#               read.csv("Northern_Pacific/fb38a48fa31168346af76a8b084e96a80a79fbb5.csv", stringsAsFactors = F),
#               read.csv("Southern_Pacific/a8278ba2b7dc45a16f91daf01cab3c9a63e707d7.csv", stringsAsFactors = F)
#             )
# chords <- all_data[all_data$phylum == "Chordata",]
# 
# obis_spec_to_plot <- chords %>% filter(species != "") %>% group_by(species) %>% count() %>% arrange(-n)
obis_spec_to_plot <- c("Caretta caretta", "Dermochelys coriacea", "Diomedea exulans", "Mirounga angustirostris",
             "Phoebastria immutabilis", "Phoebastria nigripes", "Physeter macrocephalus",
             "Puffinus griseus", "Thalassarche chrysostoma", "Thalassarche melanophris")

# use_eol()
# chords$commonName <- sci2comm(chords$scientificName)
for (i in 1:10) {
  species <- obis_spec_to_plot[i]
  # species_occurrences <- occurrence(scientificname = species)
  spec_data <- read.csv(paste0("OBIS_Files/OBIS_all_chordata", gsub(" ", "_", species), ".csv"))
  plot_species_obis(sci_name = species, input_data = spec_data, 
                    plot_fn = paste0(gsub(" ", "_", species), "_plot.png"))
}
