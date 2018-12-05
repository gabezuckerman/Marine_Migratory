require(shiny)
require(shinydashboard)
require(shinyjs)
library(tidyverse)
library(glue)
library(sf)
library(raster)
library(mapview)
library(broom)
library(leaflet)
library(leaflet.extras)
library(DT)
library(resample)
library(robis)
library(lubridate)
library(RColorBrewer)

atn <- NULL
obis <- NULL
#get commonNames for OBIS
getOBISnames <- function() {
  spec <- read.csv("../DataProcessing/obis_spec_cts_named.csv", stringsAsFactors = F)
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
  atn_data <- read.csv('../atnPacificOnly.csv', stringsAsFactors = F)
  l <- list()
  for(i in 1:length(list_species)) {
    s <- atn_data %>% filter(species == list_species[i])
    l[[i]] <- s
  }
  return(bind_rows(l))
}

#gets common names for ATN
getATNnames <- function() {
  spec <- read.csv("../atnPacificOnlySpecCounts.csv", stringsAsFactors = F)
  return(spec$species)
}

# load OBIS data
loadOBIS <- function(list_of_species) {
  if (list_of_species == "") {
    return(NULL)
  }
  species_data <- list()
  spec_names <- read.csv("../DataProcessing/obis_spec_cts_named.csv")
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
  #reformats longitude
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
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
                  setView(lng = 180, lat = 0, zoom = 2)
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
                  setView(lng = 180, lat = 0, zoom = 2)
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
    setView(lng = 180, lat = 0, zoom = 2)
  return(m)
}



server <- shinyServer(function(input, output, session) {
  
  values <- reactiveValues(selectedTab = 1)
  #syncs sidebar and main tabs
  observeEvent(input$navbar, {
    toggle("tab1_sidebar", condition = input$navbar == "tab1_val")
    toggle("tab2_sidebar", condition = input$navbar == "tab2_val")
    toggle("tab3_sidebar", condition = input$navbar == "tab3_val")
  })
  
  #starting text on each of the main tabs
  output$tab1_valuebox <- renderValueBox({
    box(width = 12, title = "Identifying Potential Locations for New Marine Protected Areas",  solidHeader = TRUE, status = "info", align = "center",
        "An open-source database approach", br(), 'In cooperation with Emily Owen and Stacy Baez of the Pew Bertarelli Ocean Legacy'
    )
  })
  
  
  output$info_box <- renderValueBox({
    box(width = 12, solidHeader = TRUE, status = "info", align = "center",
        'Created by Ben Goldstein, Erin Westeen and Gabe Zuckerman')
  })
  
  output$tab2_valuebox <- renderValueBox({
    box(status = "info", 'Choose data source and select which species you would like download.', br(), 'Please limit to 3 species', br(),
        'Alternatively, load additional data in the form of a .csv file.',
        solidHeader = TRUE, align = "left"
    )
  })
  
  output$tab3_valuebox <- renderValueBox({
    box(status = 'info', 'Choose map type to begin mapping process.', br(), 
        'You chose the following species:', br(), input$species)
  })
  
  
  output$datasource <- renderUI({
    if(input$datasource == "ATN") {
      selectInput("species", "Species (ATN)",
                  choices = c(
                    `Select One or More` = "",
                   getATNnames()
                  ), multiple = TRUE)
    }
    else if(input$datasource == "OBIS") {
      actionButton("loadOBIS", "Load")
      selectInput("species", "Species (OBIS)",
                  choices = c(
                    `Select One or More` = "",
                    getOBISnames()
                  ), multiple = TRUE)
    }
    
    else if(input$datasource == "Both") {
      actionButton("loadBoth", "Load")
      selectInput("species", "Species (OBIS and ATN)",
                  choices = c(
                    `Select One or More` = "",
                    getATNnames()
                  ), multiple = TRUE)
    }
  })
  
    
    observeEvent(
      input$loadData, 
      if(input$datasource == "OBIS") {
        obis <<- pacificProcessing(loadOBIS(input$species))
        output$OBISTable <- renderDataTable(obis, options = list(scrollX = TRUE))
      }
      else if(input$datasource == "ATN") {
        atn <<- loadATN(input$species)
        output$ATNTable <- renderDataTable(atn, options = list(scrollX = TRUE))
      }
      else if(input$datasource == "Both") {
        atn <<- loadATN(input$species)
        obis <<- loadOBIS(input$species)
        output$ATNTable <- renderDataTable(atn, options = list(scrollX = TRUE))
      }

    )
  
  
  
  observeEvent(
    input$mapButton,
    output$map <- renderLeaflet({
      if (input$datasource == "ATN") {
        if (input$maptype == "heat") pacificMapHeatmap(atn)
        else if (input$maptype == "point") pacificMapPoints(atn)
        else pacificMapLines(atn, numInds = input$numInds, cb = input$colorby)
      } else if (input$datasource == "OBIS") {
        if (input$maptype == "point") pacificMapPoints(obis)
        else pacificMapHeatmap(obis)
      } else if (input$datasource == "Both") {
          pacificMapLines(atn, numInds = input$numInds, cb = input$colorby,
                        m = pacificMapHeatmap(obis, pass = T))
      }
    })
  )
  
  #downloads created map, not reacitve
  # output$downloadMap <- downloadHandler(
  #   filename = function() { paste0(main(),'.tif') },
  #   #predictions() not a thing yet, so cannot save it
  #   content = function(file) {
  #     raster::writeRaster(pred, file)
  #   }
  # )
})

### TODO
# Ben:
  # Plot both OBIS and ATN at the same time
# Gabe:
  # Get everything in one pane
# Erin:
  # Get kernel density and BBMM into app (or give to Gabe to pu into app)