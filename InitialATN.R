library(mapview)
library(sf)
library(tidyverse)
library(lubridate)
library(gganimate)
library(plotly)
library(maps)
library(ggmap)
library(ggthemes)

atn <- read.csv("MMA_Data/ERDDAP_ATN/all_ATN.csv", header = T, stringsAsFactors = F)
atn <- atn[-1,]
atn$time <- ymd_hms(atn$time)
ids <- unique(atn$toppID)



whaleSharks <- atn %>% filter(commonName == "Whale Shark")
whaleSharks$longitude <- as.numeric(whaleSharks$longitude)
whaleSharks$latitude <- as.numeric(whaleSharks$latitude)
whaleSharks$year <- format(whaleSharks$time, '%Y')
whaleSharks$day <- format(whaleSharks$time, '%D')
whaleSharks$stand_time <- as.POSIXct(paste0('2000-', format(whaleSharks$time, '%m-%d %T')))

# Create topo background
earth <- st_as_sf(rnaturalearth::countries110)
lon_range <- range(whaleSharks$longitude) + c(-20, 20)
lat_range <- range(whaleSharks$latitude) + c(-20, 20)
bbox <- st_polygon(list(cbind(lon_range[c(1,1,2,2,1)], lat_range[c(1,2,2,1,1)])))
bbox <- st_sfc(bbox)
st_crs(bbox) <- st_crs(earth)
area <- st_intersection(earth, bbox)

maxid <- NULL
maxNum <- -1

for(i in unique(whaleSharks$serialNumber)) {
  table <- whaleSharks %>% filter(serialNumber == i)
  num <- as.numeric(nrow(table))
  if (num > maxNum) {
    maxNum <- num
    maxid <- as.numeric(i)
  }
}

whaleSharkMost <- whaleSharks %>% filter(serialNumber == maxid)
whaleSharkMost$timeSeries <- 1:nrow(whaleSharkMost)

p <- ggplot(whaleSharkMost, aes(longitude, latitude)) +
  geom_point(aes(frame = timeSeries))

p <- ggplotly(p, mode = 'lines', line = list(simplyfy = F), type = 'scatter')

p

whaleSharkMost$longitude <- whaleSharkMost$longitude - 360






p <- ggplot(whaleSharkMost,aes(x=longitude, y=latitude, frame = timeSeries)) + geom_line()




basemap <- get_map(maptype = "satellite", zoom = 8)

p <- ggplotly(p) %>%
  layout(
    title = "A Single Whale Shark",
    yaxis = list(
      title = "Latitude",
      zeroline = F
    ),
    xaxis = list(
      title = "Longitude",
      zeroline = F, 
      showgrid = F
    )
  ) %>% 
  animation_opts(
    frame = 100, 
    transition = 0, 
    redraw = FALSE
  ) %>%
  animation_slider(
    currentvalue = list(
      prefix = "Point Number "
    )
  )

ggplotly(ggmap(mapImageData)) 


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

plot(p)









sf <- st_as_sf(whaleSharks, coords = c("longitude", "latitude"), crs = 3832)
mapview(sf)



xmin <- min(whaleSharkMost$longitude) - 20
xmax <- max(whaleSharkMost$longitude) + 20
ymin <- min(whaleSharkMost$latitude) - 20
ymax <- max(whaleSharkMost$latitude) + 20

Amap <- ggplot() + 
  geom_line(aes(x = longitude, y = latitude), data = whaleSharkMost, colour = "grey") + 
  coord_cartesian(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) + 
  theme_bw()
Amap

























