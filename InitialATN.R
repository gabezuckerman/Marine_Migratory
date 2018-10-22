library(mapview)
library(sf)
library(tidyverse)
library(lubridate)
library(gganimate)

atn <- read.csv("MMA_Data/ERDDAP_ATN/all_ATN.csv", header = T, stringsAsFactors = F)
atn <- atn[-1,]
atn$time <- ymd_hms(atn$time)
ids <- unique(atn$toppID)



whaleSharks <- atn %>% filter(commonName == "Whale Shark")
whaleSharks$longitude <- as.numeric(whaleSharks$longitude)
whaleSharks$latitude <- as.numeric(whaleSharks$latitude)
whaleSharks$year <- format(whaleSharks$realTime, '%Y')
whaleSharks$stand_time <- as.POSIXct(paste0('2000-', format(whaleSharks$time, '%m-%d %T')))

# Create topo background
earth <- st_as_sf(rnaturalearth::countries110)
lon_range <- range(whaleSharks$longitude) + c(-20, 20)
lat_range <- range(whaleSharks$latitude) + c(-20, 20)
bbox <- st_polygon(list(cbind(lon_range[c(1,1,2,2,1)], lat_range[c(1,2,2,1,1)])))
bbox <- st_sfc(bbox)
st_crs(bbox) <- st_crs(earth)
area <- st_intersection(earth, bbox)


p <- ggplot(whaleSharks) + 
  geom_sf(data = area, fill = 'white') + 
  geom_point(aes(longitude, latitude, group = toppID, colour = year), size = 2.5) +
  coord_sf(xlim = range(whaleSharks$longitude), ylim = range(whaleSharks$latitude)) + 
  labs(title = 'Tracking of whale sharks over the year',
       subtitle = 'Date: {format(frame_time, "%b %e")}',
       x = NULL, y = NULL) +
  theme(panel.background = element_rect(fill = 'lightblue'),
        legend.position = 'bottom') +
  transition_components(id, stand_time) + 
  shadow_trail(distance = 0.01, size = 0.3)
animate(p, 200, 10)











sf <- st_as_sf(whaleSharks, coords = c("longitude", "latitude"), crs = 3832)
mapview(sf)
