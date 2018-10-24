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
