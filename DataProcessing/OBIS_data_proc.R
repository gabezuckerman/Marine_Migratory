library(taxize)
library(plyr)
library(dplyr)
library(maps)
library(scales)
library(robis)
library(ggplot2)
library(mapdata)

globalGridSize <- 1 # How many degrees are our grids

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

add_blocks <- function(point_data) {
  point_data$blockID <- floor(point_data$decimalLongitude / globalGridSize) + 
                        1000 * floor(point_data$decimalLatitude / globalGridSize)
  return(point_data)
}

add_latlong_to_blocks <- function(blocked_data) {
  blocked_data$decimalLongitude <- (blocked_data$blockID %% 1000) * globalGridSize + (globalGridSize/2)
  blocked_data$decimalLatitude <- floor(blocked_data$blockID / 1000) * globalGridSize + (globalGridSize/2)
  return(blocked_data)
}

plot_one_species_obis <- function(sci_name = NULL, common_name = NULL, 
                                  input_data = NULL, plot_fn = "Plot_Default.png") {
  name <- NULL
  if (is.null(input_data)) {
    spec_data <- occurrence(sci_name)
    name <- sci_name
  } else if (!is.null(common_name)) {
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

plot_mult_species_obis <- function(sci_names = NULL, input_data = NULL,
                                   plot_fn = "Plot_Default.png", distinguish = F) {
  if (is.null(input_data)) {
    species_data <- list()
    for (i in 1:length(sci_names)) {
      try(
        species_data[[i]] <- occurrence(sci_names[i])
      )
    }
    input_data <- bind_rows(species_data)
  }
  
  if (distinguish) {
    shift <- 200
    spec_data <- input_data
    spec_data$decimalLongitude <- spec_data$decimalLongitude + shift
    
    blocked_data <- add_blocks(spec_data)
    blocked_data <- blocked_data[,c("blockID", "species")]
    blocked_data$n_plc <- 1
    
    blocked_data <- ddply(blocked_data, ~blockID, summarise,
                          nspecies=length(unique(species)), n=sum(n_plc))
    blocked_data <- add_latlong_to_blocks(blocked_data)
  
    data_to_plot <- blocked_data
    
    world <- map_data("world", center = shift, col = "white", bg = "gray96", 
             ylim = c(-60,90), mar = c(0,0,0,0))
    world$long2 <- world$long + 360
    
    print("RUN RUN RUN")
    
    p <- ggplot() + 
         geom_raster(data = data_to_plot, aes(x = decimalLongitude, y = decimalLatitude, fill = n)) +
         scale_color_gradient(low = "#f4df42", high = "#f44141", aesthetics = "fill") +
         geom_polygon(data = world, fill = "#e8e8e8", col="black",
                      aes(x=long, y = lat, group = group)) +
         geom_polygon(data = world, fill = "#e8e8e8", col="black",
                      aes(x=long2, y = lat, group = group)) +
         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) + xlim(100,300)
    ggsave(filename = plot_fn, device = "png", plot = p)
    
  } else {
    shift <- 200
    data_to_plot <- input_data
    
    png(plot_fn, width = 1800, height = 900)
    plot.map("world", center = shift, col = "white", bg = "gray96", 
             fill = TRUE, ylim = c(-60,90), mar = c(0,0,0,0))
    
    data_to_plot$decimalLongitude[input_data$decimalLongitude < 0] <- data_to_plot$decimalLongitude[input_data$decimalLongitude < 0] + shift
    data_to_plot$decimalLongitude[input_data$decimalLongitude > 0] <- data_to_plot$decimalLongitude[input_data$decimalLongitude > 0] + (shift-360)
    
    points(data_to_plot$decimalLongitude, data_to_plot$decimalLatitude, col = alpha("blue", 0.1))
    title(paste0("Compound species occurrence"), cex.main=3)
    
    dev.off()
    
    print("RUN RUN RUN RUN")
  }
}

obis_spec_cts <- read.csv("OBIS_Species_cts_rough.csv", stringsAsFactors = F)
obis_spec_cts <- obis_spec_cts[obis_spec_cts$n > 10,]
obis_spec_cts$commonName <- obis_spec_cts$species
for (i in 1:nrow(obis_spec_cts)) {
  obis_spec_cts$commonName[i] <- try(sci2comm(id = get_uid(obis_spec_cts$species[i]), db = "iucn"))
}
obis_spec_cts$commonName <- as.character(obis_spec_cts$commonName)

plot_mult_species_obis(sci_names = obis_spec_cts[1:5, "species"], 
                       plot_fn = "OBIS_Plots/test_pmso13.png", distinguish = T)

# for (i in 100:100) {
#   species <- obis_spec_cts$species[i]
#   plot_species_obis(sci_name = species, input_data = NULL,
#                     plot_fn = paste0("../OBIS_Plots/", gsub(" ", "_", species), "_plot.png"))
# }
