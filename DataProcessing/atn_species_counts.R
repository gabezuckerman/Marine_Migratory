atn <- read.csv("Data/atnPacificOnly.csv", stringsAsFactors = F)
species <- list()
numInd <- list()

for (i in seq(1, length(unique(atn$species)))) {
  print(unique(atn$species)[i])
  species[i] <- unique(atn$species)[i]
  numInd[i] <- length(unique((atn %>% filter(species == unique(atn$species)[i]))$serialNumber))
}
specCounts <- data_frame(as.character(species), as.numeric(numInd))
colnames(specCounts)[2] <- "numInds"
colnames(specCounts)[1] <- "species"
specCounts <- as.data.frame(specCounts)
specCounts <- specCounts[order(specCounts$numInds, decreasing = T),]
setwd("~/Desktop/Fall 2018/MMA/Marine_Migratory/movit/")
write_csv(specCounts, "Data/atnPacificOnlyNumIndsBySpec.csv")
