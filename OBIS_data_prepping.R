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

for (i in 0:4) {
  cat("Round ", i, "\n")
  system_time(
    all_specs <- foreach(
      x = obis_species$species[(i*25 + 1):((i+1)*25)],
      .packages = c("robis"),
      .inorder = F,
      .errorhandling = 'remove'
    ) %dopar% {
      write.csv(occurrence(x), file = paste0("OBIS_Files/OBIS_all_chordata", gsub(" ", "_", x), ".csv"))
    }
  )
}
