
# Retrieve the names of species for which OBIS data is available
getOBISnames <- function() {
  spec <- read.csv("Data/obis_spec_cts_named.csv", stringsAsFactors = F)
  return(spec$commonName)
}

# Take a dataframe with columns for decimalLatitude and decimalLongitude and
#   assign each a block ID based on a grid
add_grid_to_points <- function(obs_data, degree) {
  obs_data$block <- (floor(obs_data$decimalLatitude / degree) + 
                       1000 * floor(obs_data$decimalLongitude / degree)) %>% 
    as.factor() %>% as.numeric()
  obs_data$blockID <- floor(obs_data$decimalLatitude / degree) + 
    1000 * floor(obs_data$decimalLongitude / degree)
  return(obs_data)
}

# Take a dataframe with blockIDs and retrieve the central decimalLatitude and
#   decimalLongitude for each block
add_latlong_to_grid <- function(gridsummary, degree) {
  gridsummary$decimalLatitude <- (gridsummary$blockID %% 1000) * degree + (degree/2)
  gridsummary$decimalLongitude <- floor(gridsummary$blockID / 1000) * degree + (degree/2)
  return(gridsummary)
}

#loading in ATN Data
loadATN <- function(list_species){
  atn_data <- read.csv('Data/atnPacific_withUTMs.csv', stringsAsFactors = F)
  atn_data$decimalLongitude[atn_data$decimalLongitude < 0] <- 
    atn_data$decimalLongitude[atn_data$decimalLongitude < 0] + 360
  l <- list()
  for(i in 1:length(list_species)) {
    s <- atn_data %>% filter(species == list_species[i])
    l[[i]] <- s
  }
  table <- bind_rows(l)
  return(table)
}

#gets common names for ATN
getATNnames <- function() {
  spec <- read.csv("Data/atnPacificOnlySpecCounts.csv", stringsAsFactors = F)
  return(spec$species)
}

# load OBIS data
loadOBIS <- function(list_of_species) {
  if (list_of_species == "") {
    return(NULL)
  }
  species_data <- list()
  spec_names <- read.csv("Data/obis_spec_cts_named.csv")
  for (i in 1:length(list_of_species)) {
    spec_specified <- list_of_species[[i]]
    if (toupper(spec_specified) %in% toupper(spec_names$commonName)) {
      sciname <- spec_names$species[toupper(spec_names$commonName) == toupper(spec_specified)]
      species_data[[i]] <- occurrence(sciname)
    } else if (toupper(spec_specified) %in% toupper(spec_names$species)) {
      sciname <- spec_names$species[toupper(spec_specified) == toupper(spec_names$species)]
      species_data[[i]] <- occurrence(sciname)
    } else {
      print(paste0("Species '", list_of_species[[i]], "' not found"))
      return(NULL)
    }
  }
  obis <- bind_rows(species_data)
  obis$decimalLongitude <- as.numeric(obis$decimalLongitude)
  obis$decimalLongitude <- map(obis$decimalLongitude, shift)
  return(obis)
}

#functions that help with keeping only instances in the Pacific Ocean
inPacific <- function(long, lat) {
  if (long <= 290 && long >= 280 && lat >= -80 && lat <= 9) {
    return(TRUE)
  }
  else if(long <= 280 && long >= 276 && lat >= -80 && lat <= 9) {
    return(TRUE)
  }
  else if(long <= 276 && long >= 270 && lat >= -80 && lat <= 14) {
    return(TRUE)
  }
  else if(long <= 270 && long >= 260 && lat >= -80 && lat <= 18) {
    return(TRUE)
  }
  else if(long <= 260 && long >= 145 && lat >= -80 && lat <= 66) {
    return(TRUE)
  }
  else if(long <= 145 && long >= 100 && lat >= 0 && lat <= 66) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

shift <- function(longitude) {
  if (longitude < 0) {
    return(longitude + 360)
  }
  else {
    return(longitude)
  }
}

#need to pre-process atn data
pacificProcessing<- function(mytable) {
  if(is.null(mytable)) {
    return(NULL)
  }
  #reformats longitude
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
  mytable$decimalLongitude <- map(mytable$decimalLongitude, shift)

  #keeps rows in pacific determined by lon, lat
  pacific <- map2(mytable$decimalLongitude, mytable$decimalLatitude, ~inPacific(.x, .y))
  head(mytable)
  mytable$inPO <- pacific
  head(mytable)
  mytable <- filter(mytable, inPO == TRUE)
  return(mytable)
}

#creates a leaflet object from an obis or atn table
pacificMapPoints <- function(mytable, m = NULL, pass = F) {
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
  if (is.null(m)) m <- leaflet() %>% addTiles()
  m <- m %>%
    addCircleMarkers(data = mytable, ~decimalLongitude, ~decimalLatitude, 
                     radius = 3, stroke = F, opacity = 0.2,
                     popup = ~as.character(species),
                     label = ~as.character(species)) 
  if (!pass) m <- m %>% 
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    setView(lng = 180, lat = 0, zoom = 2) %>%
    onRender(
      "function(el, x) {
            L.easyPrint({
              sizeModes: ['A4Landscape', 'A4Portrait'],
              filename: 'MOViTmap',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
    )
  return(m)
}

pacificMapHeatmap <- function(mytable, m = NULL, pass = F) {
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
  
  if (is.null(m)) m <- leaflet() %>% addTiles()
  
  degree <- 1
  
  grid_mytable <- add_grid_to_points(mytable, degree)
  blocks <- grid_mytable %>% group_by(block, blockID) %>% count()
  blocks_ltlng <- add_latlong_to_grid(blocks, degree)
  
  pal <- brewer.pal(9, "YlOrRd")
  blocks_ltlng$col <- pal[pmin(floor(log(blocks_ltlng$n)), 9) + 1]
  
  blocks_ltlng$decimalLatitude[blocks_ltlng$decimalLatitude > 100] <- 
    blocks_ltlng$decimalLatitude[blocks_ltlng$decimalLatitude > 100] - 1000
  
  m <- m %>%
    addRectangles(data = blocks_ltlng, 
                  lng1=~decimalLongitude-(degree/2), lng2=~decimalLongitude+(degree/2),
                  lat1=~decimalLatitude-(degree/2), lat2=~decimalLatitude+(degree/2),
                  fillColor = ~col, fillOpacity = 0.5, stroke = F) 
  if (!pass) m <- m %>% 
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    setView(lng = 180, lat = 0, zoom = 2) %>%
    onRender(
      "function(el, x) {
            L.easyPrint({
              sizeModes: ['A4Landscape', 'A4Portrait'],
              filename: 'MOViTmap',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
    )
  return(m)
}


pacificMapLines <- function(mytable, numInds = 5, cb = "species", m = NULL) {
  mytable <- mytable %>% arrange(time)
  
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
  if (is.null(m)) m <- leaflet(data = mytable) %>% addTiles() 
  
  inds <- mytable %>% group_by(serialNumber, species) %>% count() %>% arrange(-n)
  spec_opts <- unique(inds$species)
  
  if (cb == "species") {
    pal <- c("#663ec4", "#bf3b3b", "#e0a831")
    for (s in 1:length(spec_opts)) {
      inds_to_plot <- inds[inds$species == spec_opts[s],]
      for (i in 1:numInds) {
        m <- m %>% addPolylines(data = mytable[mytable$serialNumber == 
                                                 inds_to_plot$serialNumber[i],],
                                ~decimalLongitude, ~decimalLatitude, 
                                popup = ~as.character(species),
                                label = ~as.character(species),
                                color = pal[s])
      }
    }
  } else {
    pallettes <- c("YlGn", "RdPu", "PuBu", "BuPu", "Greys", "Oranges", "Reds")
    for (s in 1:length(spec_opts)) {
      pal <- brewer.pal(name = pallettes[s], n = min(numInds, 9))
      pal <- rev(c(pal, pal))
      inds_to_plot <- inds[inds$species == spec_opts[s],]
      for (i in 1:numInds) {
        m <- m %>% addPolylines(data = mytable[mytable$serialNumber == 
                                                 inds_to_plot$serialNumber[i],],
                                ~decimalLongitude, ~decimalLatitude, 
                                popup = ~as.character(species),
                                label = ~as.character(species),
                                color = pal[i])
      }
    }
  }
  
  m <- m %>% addProviderTiles(providers$Esri.OceanBasemap) %>%
    setView(lng = 180, lat = 0, zoom = 2) %>%
    onRender(
      "function(el, x) {
            L.easyPrint({
              sizeModes: ['A4Landscape', 'A4Portrait'],
              filename: 'MOViTmap',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
    )
  return(m)
}

plot.mcp <- function(atn, numInds, conf){
  
  focalSp <- na.omit(atn)
  
  ## if user selects more individuals than there are in the dataset, return data for all individuals
  if (length(unique(focalSp$serialNumber)) >= numInds){
    numInds <- numInds
  }else{
    numInds <- length(unique(focalSp$serialNumber))
  }
  ## return individuals with most data
  sequence <- unique(focalSp$serialNumber)[order(table(focalSp$serialNumber), decreasing = TRUE)][1:numInds]
  indiv <- subset(focalSp, focalSp$serialNumber %in% sequence)
  
  
  ## calculate MCP per individual, per zone
  for ( i in sequence ){
    for ( j in unique(indiv$UTMzone) ){
      indiv.zone <- indiv[indiv$serialNumber == i & indiv$UTMzone == j ,  ]
      if(nrow(indiv.zone) >= 5){
        idsp <- data.frame(indiv.zone$serialNumber)
        coordinates(idsp) <- cbind(indiv.zone$UTM.east, indiv.zone$UTM.north)
        
        focalSp.mcp <- mcp(idsp, percent = conf, unin = 'm', unout = 'km2')
        proj4string(focalSp.mcp) <- CRS(paste0("+proj=utm +zone=", j ))
        mcp_poly <- spTransform(focalSp.mcp, CRS("+proj=longlat +datum=WGS84")) }
      
        if ( i == sequence[1] ){
        indivPolys <- mcp_poly
         }else{
        indivPolys <- bind(indivPolys, mcp_poly)}
    } 
  }
  
  ## bind them back together by individual and create a poly object to plot
  for ( k in sequence ){
    indivPoly <- aggregate(indivPolys[indivPolys$id == k , ])
    ##indivPoly <- unionSpatialPolygons(indivPolys[indivPolys$id == k , ], IDs = rep(1, sum(indivPolys$id == k)))
    
    if ( k == sequence[1] ){
      polyToPlot <- indivPoly
    }else{
      polyToPlot <- bind(polyToPlot, indivPoly)
    } 
  } 

  polyToPlot$id <- sequence
  
  if (length(sequence) <= 12){
    pal <- brewer.pal(max(length(sequence), 3), "Paired")
  }else{
    extra <- length(sequence) - 12
    pal <- c(brewer.pal(12, "Paired"),  brewer.pal(extra, "Paired") )
  }
  
  return(
    leaflet(polyToPlot) %>% addProviderTiles(providers$Esri.OceanBasemap) %>% 
    addPolygons(weight = .3, opacity = 1 , color = pal[1:numInds] , fillColor = pal) %>% 
    addLegend('topleft', colors = pal[1:numInds], labels = sequence,  title = c(unique(focalSp$species))) %>%
      onRender(
        "function(el, x) {
            L.easyPrint({
              sizeModes: ['A4Landscape', 'A4Portrait'],
              filename: 'MOViTmap',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
      )
  )
}
