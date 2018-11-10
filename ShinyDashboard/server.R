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
library(DT)
library(resample)
library(robis)

atn <- NULL
obis <- NULL
#get commonNames for OBIS
getOBISnames <- function() {
  spec <- read.csv("../DataProcessing/obis_spec_cts_named.csv", stringsAsFactors = F)
  return(spec$commonName)
}

#loading in ATN Data
loadATN <- function(list_species){
  print("reached load function")
  atn_data <- read.csv('../all_ATN.csv', stringsAsFactors = F)
  colnames(atn_data)[8] <- "decimalLongitude"
  colnames(atn_data)[9] <- "decimalLatitude"
  colnames(atn_data)[1] <- "species"
  print("loaded in data")
  l <- list()
  for(i in 1:length(list_species)) {
    s <- atn_data %>% filter(species == list_species[i])
    l[[i]] <- s
  }
  print("got species")
  print(bind_rows(l))
  return(bind_rows(l))
}

#gets common names for ATN
getATNnames <- function() {
  spec <- read.csv("../ATN_spec_cts.csv", stringsAsFactors = F)
  return(spec$commonName)
}

# load OBIS data
obis_batch <- function(list_of_species) {
  if (list_of_species == "") {
    return(NULL)
  }
  species_data <- list()
  spec_names <- read.csv("../DataProcessing/obis_spec_cts_named.csv")
  for (i in 1:length(list_of_species)) {
    spec_specified <- list_of_species[[i]]
    if (spec_specified %in% spec_names$commonName) {
      sciname <- spec_names$species[spec_names$commonName == spec_specified]
      species_data[[i]] <- occurrence(sciname)
    } else if (spec_specified %in% spec_names$species) {
      sciname <- spec_specified
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
pacificMap <- function(mytable) {
  mytable$decimalLongitude <- as.numeric(mytable$decimalLongitude)
  mytable$decimalLatitude <- as.numeric(mytable$decimalLatitude)
  m <- leaflet(data = mytable) %>% addTiles() %>%
    addMarkers(~decimalLongitude, ~decimalLatitude, 
               popup = ~as.character(species),
               label = ~as.character(species)) %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    setView(lng = 180, lat = 0, zoom = 1)
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
  
  
  # output$loadData <- renderUI({
  #     renderDataTable(obis_batch(input$species), options = list(scrollX = TRUE))
  #     
  # })
  
  
  
  # observeEvent(
  #   input$loadData,
  #   output$speciesTable <-renderDataTable(obis_batch(input$species), options = list(scrollX = TRUE))
  #   # species <- input$species
  #   # datasource <- input$new
  #   # print(datasource, species)
  #   )
  
  # output$new <- renderUI({
  #   if(input$new == "opensource") {
  #     radioButtons("datasource", "Which datasource would you like to use?",
  #                  choices = c("OBIS", "ATN"), inline = T)
  #     uiOutput("datasource")
  #   }
  #   
  # })
  output$datasource <- renderUI({
    if(input$datasource == "ATN") {
      selectInput("species", "Species (ATN)",
                  choices = c(
                    `Select One or More` = "",
                   getATNnames()
                  ), multiple = TRUE)
      #actionButton("loadATN", "Load")
    }
    else if(input$datasource == "OBIS") {
      actionButton("loadOBIS", "Load")
      selectInput("species", "Species (OBIS)",
                  choices = c(
                    `Select One or More` = "",
                    getOBISnames()
                  ), multiple = TRUE)
    }
    #actionButton("loadObis", "Load")
  })
  
   #output$loadOBIS <- renderDataTable(obis_batch(input$species), options = list(scrollX = TRUE))

    
    observeEvent(
      input$loadData, 
      if(input$datasource == "OBIS") {
        obis <<- pacificProcessing(obis_batch(input$species))
        output$OBISTable <- renderDataTable(obis, options = list(scrollX = TRUE))
      }
      else if(input$datasource == "ATN") {
        atn <<- loadATN(input$species)
        output$ATNTable <- renderDataTable(atn, options = list(scrollX = TRUE))
      }
    )
  
  
  
  # output$datasource <- renderDataTable(obis_batch(input$species), options = list(scrollX = TRUE))
  
  observeEvent(
    input$mapButton,
    output$map <- renderLeaflet({
      if(input$datasource == "ATN") {
        pacificMap(atn)
      }
      else if(input$datasource == "OBIS") {
        pacificMap(obis)
      }
    })
  )
  
  

  # output$toMap <- renderUI({
  #   if (is.null(logit())) return(NULL)
  #   actionButton("toMap", "See Map!")
  # })
  # 
  # shinyjs::onclick('toMap',expr={
  #   # move to Map Results
  #   updateTabsetPanel(session, "navbar", 'tab4_val')
  # })
  
  
  #downloads created map, not reacitve
  # output$downloadMap <- downloadHandler(
  #   filename = function() { paste0(main(),'.tif') },
  #   #predictions() not a thing yet, so cannot save it
  #   content = function(file) {
  #     raster::writeRaster(pred, file)
  #   }
  # )
})
