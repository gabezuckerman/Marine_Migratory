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

# load data
obis_batch <- function(list_of_species) {
  species_data <- list()
  for (i in 1:length(list_of_species)) {
    species_data[[i]] <- occurrence(list_of_species[[i]])
  }
  return(bind_rows(species_data))
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

pacificProcessing<- function(obisTable) {
  #reformats longitude
  obisTable$decimalLongitude <- as.numeric(map(obisTable$decimalLongitude, shift))
  #keeps rows in pacific determined by lon, lat
  pacific <- map2(obisTable$decimalLongitude, obisTable$decimalLatitude, ~inPacific(.x, .y))
  obisTable$inPO <- pacific
  obisTable <- filter(obisTable, inPO == TRUE)
  return(obisTable)
}

#creates a leaflet object from an obis table
pacificMap <- function(obisTable) {
  p <- pacificProcessing(obisTable)
  m <- leaflet(data = p) %>% addTiles() %>%
    addMarkers(~decimalLongitude, ~decimalLatitude, 
               popup = ~as.character(species),
               label = ~as.numeric(individualCount)) %>%
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
  
  
  observeEvent(
    input$loadData,
    output$speciesTable <-renderDataTable(obis_batch(input$species), options = list(scrollX = TRUE))
    # species <- input$species
    # datasource <- input$new
    # print(datasource, species)
    )
  
  observeEvent(
    input$mapButton,
    output$map <- renderLeaflet({
      pacificMap(obis_batch(input$species))
    })
  )
  
  

  output$toMap <- renderUI({
    if (is.null(logit())) return(NULL)
    actionButton("toMap", "See Map!")
  })
  
  shinyjs::onclick('toMap',expr={
    # move to Map Results
    updateTabsetPanel(session, "navbar", 'tab4_val')
  })
  
  
  #downloads created map, not reacitve
  output$downloadMap <- downloadHandler(
    filename = function() { paste0(main(),'.tif') },
    #predictions() not a thing yet, so cannot save it
    content = function(file) {
      raster::writeRaster(pred, file)
    }
  )
})
