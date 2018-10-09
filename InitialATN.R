library(mapview)
library(sf)
library(tidyverse)
library(lubridate)

atn <- read.csv("MMA_Data/ERDDAP_ATN/all_ATN.csv", header = T, stringsAsFactors = F)
atn <- atn[-1,]
atn$realTime <- ymd_hms(atn$time)
ids <- unique(atn$toppID)

whaleSharks <- atn %>% filter(commonName == "Whale Shark")
whaleSharks$longitude <- as.numeric(whaleSharks$longitude)
whaleSharks$latitude <- as.numeric(whaleSharks$latitude)
sf <- st_as_sf(whaleSharks, coords = c("longitude", "latitude"), crs = 3832)
mapview(sf)
