library(robis)
library(bench)
library(doParallel)
library(dplyr)
# library(taxize)

list_of_species <- 
    c("Caretta caretta", "Dermochelys coriacea", "Diomedea exulans", "Mirounga angustirostris",
      "Phoebastria immutabilis", "Phoebastria nigripes", "Physeter macrocephalus",
      "Puffinus griseus", "Thalassarche chrysostoma", "Thalassarche melanophris")

obis_spec_cts <- read.csv(file = "OBIS_Species_cts_rough.csv", stringsAsFactors = F)

obis_species <- obis_spec_cts[obis_spec_cts$n > 100,]

registerDoParallel(4)

system_time(
  all_specs <- foreach(
    x = obis_species$species,
    .packages = c("robis"),
    .inorder = F,
    .errorhandling = 'remove'
  ) %dopar% {
    return(occurrence(x))
  }
)

all_spec_data <- bind_rows(all_specs)
write.csv(specdata, file = "OBIS_all_chordata.csv")

spec <- obis_species$species[i]
occurrence(scientificname = spec)
