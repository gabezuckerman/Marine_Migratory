# -----------------------------------------------------------------------------
# MOViT
# filename: movit-functions.R
# authors: Gabe Zuckerman, Ben Goldstein, Erin Westeen
# last updated: Dec 2 2018
# -----------------------------------------------------------------------------

# A vector of custom colors for graphic output. Selected and reordered from RColorBrewer.
masterpalette <- c("#6A3D9A", "#FF7F00", "#023858", "#33A02C", "#B15928", "#E31A1C",
                   "#CAB2D6", "#FDBF6F", "#1F78B4", "#B2DF8A", "#FFFF99", "#FB9A99")

# -----------------------------------------------------------------------------
# getOBISnames(): 
# Purpose: retrieves a list of OBIS species names and scientific names for
#          the user to select.
getOBISnames <- function() {
  spec <- read.csv("Data/obis_spec_cts_named.csv", stringsAsFactors = F)
  return(spec$commonName)
}

# -----------------------------------------------------------------------------
# getATNnames(): 
# Purpose: retrieves a list of ATN species names for the user to select.
getATNnames <- function() {
  spec <- read.csv("Data/atnPacificOnlySpecCounts.csv", stringsAsFactors = F)
  return(spec$species)
}

# -----------------------------------------------------------------------------
# getATNandOBISnames(): 
# Purpose: retrieves a list of ATN and OBIS species names for the user to select.
getATNandOBISnames <- function() {
  obis <- read.csv("Data/obis_spec_cts_named.csv", stringsAsFactors = F)
  atn <- read.csv("Data/atnPacificOnlySpecCounts.csv", stringsAsFactors = F)
  return(c(obis$commonName, atn$species))
}


# -----------------------------------------------------------------------------
# loadATN(list_of_species): 
# Purpose: Loads in ATN data corresponding to the specified species.
# Arguments:
#   list_of_species is a vector of one or more string common or scientific names of species.
loadATN <- function(list_of_species){
  if (length(list_of_species) == 0) {
    return(NULL)
  }
  atn_data <- read.csv('Data/atnPacific_withUTMs.csv', stringsAsFactors = F)
  atn_data$decimalLongitude[atn_data$decimalLongitude < 0] <- 
    atn_data$decimalLongitude[atn_data$decimalLongitude < 0] + 360
  l <- list()
  for(i in 1:length(list_of_species)) {
    s <- atn_data %>% filter(species == list_of_species[i])
    l[[i]] <- s
  }
  table <- bind_rows(l)
  return(table)
}

# -----------------------------------------------------------------------------
# loadOBIS(list_of_species): 
# Purpose: Loads in OBIS data corresponding to the specified species.
# Arguments:
#   list_of_species is a vector of one or more string common names of species.
loadOBIS <- function(list_of_species) {
  if (length(list_of_species) == 0) {
    return(NULL)
  }
  species_data <- list()
  spec_names <- read.csv("Data/obis_spec_cts_named.csv")
  for (i in 1:length(list_of_species)) {
    spec_specified <- list_of_species[[i]]
    print(spec_specified)
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

# -----------------------------------------------------------------------------
# pacificMapPoints(mytable, m = NULL, pass = F)
# Purpose: Creates a map showing the points in space of each observation in mytable.
# Arguments:
#   mytable is a data frame containing records of species observations 
#       specified by a decimal lat-long.
#   m is a leaflet object. If not NULL, the function layers the produced 
#       point map on top of m.
#   pass tells the function whether this is the final map or if this map 
#       will be given more layers.
pacificMapPoints <- function(mytable, m = NULL, pass = F) {
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
  if (is.null(m)) m <- leaflet() %>% addTiles()
  
  factpal <- colorFactor(masterpalette[1:length(unique(mytable$species))], mytable$species)
  
  m <- m %>%
    addCircleMarkers(data = mytable, ~decimalLongitude, ~decimalLatitude, 
                     radius = 3, stroke = F, opacity = 0.2, color = ~factpal(species),
                     popup = ~as.character(species),
                     label = ~as.character(species)) %>%
    addLegend('topleft', colors = factpal(unique(mytable$species)), 
              labels = unique(mytable$species), title = "Species")
  if (!pass) m <- m %>% 
    addProviderTiles(providers$Esri.OceanBasemap) %>%
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

# -----------------------------------------------------------------------------
# pacificMapHeatmap(mytable, m = NULL, pass = F)
# Purpose: Creates heat map of point density of the observations in mytable.
# Arguments:
#   mytable is a data frame containing records of species observations 
#       specified by a decimal lat-long.
#   m is a leaflet object. If not NULL, the function layers the produced 
#       point map on top of m.
#   pass tells the function whether this is the final map or if this map 
#       will be given more layers.
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

# -----------------------------------------------------------------------------
# pacificMapLines(mytable, numInds = 5, cb = "species", m = NULL)
# Purpose: Creates a map showing the points in space of each observation in mytable.
# Arguments:
#     mytable is a data frame containing records of species observations specified 
#         by a decimal lat-long.
#     numInds specifies how many individuals will be plotted (max 12)
#     cb refers to "color by". Can be "species" or "individual".
#     m is a leaflet object. If not NULL, the function layers the produced point 
#         map on top of m.
pacificMapLines <- function(mytable, numInds = 5, cb = "species", m = NULL) {
  mytable <- mytable %>% arrange(time)
  
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
  if (is.null(m)) m <- leaflet() %>% addTiles()
  
  inds <- mytable %>% group_by(serialNumber, species) %>% count() %>% arrange(-n)
  spec_opts <- unique(inds$species)
  
  if (cb == "species") {
    pal <- masterpalette
    for (s in 1:length(spec_opts)) {
      inds_to_plot <- inds[inds$species == spec_opts[s],]
      for (i in 1:numInds) {
        species <- mytable[mytable$serialNumber == inds_to_plot$serialNumber[i],c("species")][1]
        myline <- Line(mytable[mytable$serialNumber == inds_to_plot$serialNumber[i],
                               c("decimalLatitude", "decimalLongitude")])
        myline <- SpatialLines(list(Lines(list(Line(myline)), "id")))
        if (nrow(as.data.frame(coordinates(myline))) > 100) {
            simple_line <- ms_simplify(myline, keep = 100/(nrow(as.data.frame(coordinates(myline))) * numInds))
            simple_coords <- coordinates(simple_line) %>% as.data.frame()
        } else {
            simple_coords <- coordinates(myline) %>% as.data.frame()
        }
        colnames(simple_coords) <- c("decimalLatitude", "decimalLongitude")
        simple_coords$species <- species
        
        m <- m %>% addPolylines(data = simple_coords,
                                ~decimalLongitude, ~decimalLatitude,
                                popup = ~as.character(species),
                                label = ~as.character(species),
                                color = pal[s],
                                opacity = 0.1)
      }
    }
    m <- m %>% addLegend('topleft', colors=pal[1:length(spec_opts)], label=spec_opts, title="Species")
  } else {
    pallettes <- c("PuBu", "Oranges", "BuPu", "YlGn", "Greys", "Reds", "RdPu")
    for (s in 1:length(spec_opts)) {
      pal <- brewer.pal(name = pallettes[s], n = min(numInds, 9))
      pal <- rev(c(pal, pal))
      inds_to_plot <- inds[inds$species == spec_opts[s],]
      for (i in 1:numInds) {
        species <- mytable[mytable$serialNumber == inds_to_plot$serialNumber[i],c("species")][1]
        myline <- Line(mytable[mytable$serialNumber == inds_to_plot$serialNumber[i],
                               c("decimalLatitude", "decimalLongitude")])
        myline <- SpatialLines(list(Lines(list(Line(myline)), "id")))
        if (nrow(as.data.frame(coordinates(myline))) > 100) {
            simple_line <- ms_simplify(myline, keep = 100/(nrow(as.data.frame(coordinates(myline))) * numInds))
            simple_coords <- coordinates(simple_line) %>% as.data.frame()
        } else {
            simple_coords <- coordinates(myline) %>% as.data.frame()
        }
        colnames(simple_coords) <- c("decimalLatitude", "decimalLongitude")
        simple_coords$species <- species
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

# -----------------------------------------------------------------------------
# plot.mcp(atn, numInds, conf)
# Purpose: Creates a map showing confidence intervals for animal tracking data.
# Arguments:
#     atn contains the input telemetry data
#     numInds specifies how many individuals will be plotted (max 12)
#     cb refers to "color by". Can be "species" or "individual".
#     conf is the confidence interval provided for the kernel density plot. 
#         Specify as an integer, not a decimal: 95% is 95, not 0.95.
plot.mcp <- function(atn, numInds, conf) {
  
  focalSp <- na.omit(atn)
  atn$decimalLongitude <- map(atn$decimalLongitude, shift)
  
  cts <- table(focalSp$serialNumber)
  
  ## if user selects more individuals than there are in the dataset, return data for all individuals
  if (length(cts[cts > 5]) >= numInds){
    numInds <- numInds
  }else{
    numInds <- length(cts[cts > 5])
  }
  ## return individuals with most data
  sequence <- table(focalSp$serialNumber)[order(table(focalSp$serialNumber), decreasing = TRUE)][1:numInds] %>%
              names() %>%
              as.numeric()

  indiv <- focalSp[focalSp$serialNumber %in% sequence, ]
  
  
  ## calculate MCP per individual, per zone
  for ( i in sequence ){
    for ( j in unique(indiv$UTMzone) ){
      indiv.zone <- indiv[indiv$serialNumber == i & indiv$UTMzone == j ,  ]
      if (nrow(indiv.zone) >= 5){
        idsp <- data.frame(indiv.zone$serialNumber)
        coordinates(idsp) <- cbind(indiv.zone$UTM.east, indiv.zone$UTM.north)
        
        focalSp.mcp <- mcp(idsp, percent = conf, unin = 'm', unout = 'km2')
        proj4string(focalSp.mcp) <- CRS(paste0("+proj=utm +zone=", j ))
        mcp_poly <- spTransform(focalSp.mcp, CRS("+proj=longlat +datum=WGS84 +lon_wrap=180")) 
      
        if ( i == sequence[1] ){
          indivPolys <- mcp_poly
        } else {
          indivPolys <- bind(indivPolys, mcp_poly)
        }
      }
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
  plotNum <- length(polyToPlot)
  print(plotNum)
  
  if (length(sequence) <= 12){
    pal <- masterpalette[1:numInds]
  }else{
    extra <- length(sequence) - 12
    pal <- c(masterpalette, masterpalette, masterpalette)[1:numInds]
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

# -----------------------------------------------------------------------------
# pacificProcessing(mytable): 
# Purpose: Conducts processing on generic data to trim to the Pacific.
# Arguments:
#   mytable is a data frame of occurrences with decimalLatitude and 
#       decimalLongitude columns.
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

# -----------------------------------------------------------------------------
# inPacific(long, lat): 
# Purpose: Checks if a given set of decimal longitude and latitude 
#     coordinates are in the Pacific.
# Arguments:
#   long is a single decimal longitude value.
#   lat is a single decimal latitude value.
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

# -----------------------------------------------------------------------------
# shift(longitude): 
# Purpose: Ensures longitude data are on a 0-360 scale instead of a -180-180 scale.
# Arguments:
#   longitude is a single decimal longitude value.
shift <- function(longitude) {
  if (longitude < 0) {
    return(longitude + 360)
  }
  else {
    return(longitude)
  }
}

# -----------------------------------------------------------------------------
# add_grid_to_points(obs_data, degree): 
# Purpose: Helper function for pacificMapHeatmap gridding.
# Arguments:
#   obs_data is a data frame containing the observations.
#   degree specifies the dimensions of a cell in the heatmap in decimal degrees.
add_grid_to_points <- function(obs_data, degree) {
  obs_data$block <- (floor(obs_data$decimalLatitude / degree) + 
                       1000 * floor(obs_data$decimalLongitude / degree)) %>% 
    as.factor() %>% as.numeric()
  obs_data$blockID <- floor(obs_data$decimalLatitude / degree) + 
    1000 * floor(obs_data$decimalLongitude / degree)
  return(obs_data)
}

# -----------------------------------------------------------------------------
# add_latlong_to_grid(gridsummary, degree): 
# Purpose: Helper function for pacificMapHeatmap gridding.
# Arguments:
#   gridsummary is a data frame containing gridded cell IDs and their counts.
#   degree specifies the dimensions of a cell in the heatmap in decimal degrees.
add_latlong_to_grid <- function(gridsummary, degree) {
  gridsummary$decimalLatitude <- (gridsummary$blockID %% 1000) * degree + (degree/2)
  gridsummary$decimalLongitude <- floor(gridsummary$blockID / 1000) * degree + (degree/2)
  return(gridsummary)
}

