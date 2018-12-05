atn <- read.csv("../Marine_Migratory/MMA_Data/ERDDAP_ATN/all_ATN.csv", stringsAsFactors = F)[-1,]
colnames(atn)[8] <- "decimalLongitude"
colnames(atn)[9] <- "decimalLatitude"
colnames(atn)[1] <- "species"
atn$decimalLatitude <- as.numeric(atn$decimalLatitude)
atn$decimalLongitude <- as.numeric(atn$decimalLongitude)

atnPacific <- pacificProcessing(atn)
class(atnPacific)
write.csv(as.matrix(atnPacific), "atnPacificOnly.csv")


atn_species_counts <- atnPacific %>% group_by(species) %>% count() %>% arrange(-n)
write.csv(as.matrix(atn_species_counts), "atnPacificOnlySpecCounts.csv")
