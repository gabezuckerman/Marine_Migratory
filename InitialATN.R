library(mapview)
library(sf)
library(tidyverse)
library(lubridate)

atn <- read.csv("MMA_Data/ERDDAP_ATN/all_ATN.csv", header = T, stringsAsFactors = F)
atn <- atn[-1,]
atn$realTime <- ymd_hms(atn$time)
