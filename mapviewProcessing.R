obis_batch <- function(list_of_species) {
  species_data <- list()
  for (i in 1:length(list_of_species)) {
    species_data[[i]] <- occurrence(list_of_species[[i]])
  }
  return(bind_rows(species_data))
}


ws <- obis_batch("Rhincodon typus")

sf <- st_as_sf(ws, coords = c("decimalLongitude", "decimalLatitude"), crs = 3349)
mapview()





atn <- read.csv("../Marine_Migratory/MMA_Data/ERDDAP_ATN/all_ATN.csv")[-1,]
colnames(atn)[8] <- "decimalLongitude"
colnames(atn)[9] <- "decimalLatitude"
colnames(atn)[1] <- "species"

atnPacific <- pacificProcessing(atn)
