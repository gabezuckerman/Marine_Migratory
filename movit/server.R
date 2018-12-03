# -----------------------------------------------------------------------------
# MOViT
# filename: server.R
# authors: Gabe Zuckerman, Ben Goldstein, Erin Westeen
# last updated: Dec 2 2018
# -----------------------------------------------------------------------------

require(shiny)
library(tidyverse)
library(glue)
library(sf)
library(leaflet)
library(leaflet.extras)
library(DT)
library(resample)
library(robis)
library(lubridate)
library(RColorBrewer)
library(adehabitatHR)
library(gridExtra)
library(rgdal)
library(raster)
library(maptools)
library(rsconnect)
library(htmlwidgets)
library(dplyr)
library(rmapshaper)

atn <- NULL
obis <- NULL
both <- NULL
customTable <- NULL




#get functions
source('movit-functions.R')

server <- shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
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
    
    else if(input$datasource == "ATN and OBIS") {
      actionButton("loadBoth", "Load")
      selectInput("species", "Species (ATN and OBIS)",
                  choices = c(
                    `Select One or More` = "",
                    getATNandOBISnames()
                  ), multiple = TRUE)
    }
    else if (input$datasource == "Load in .csv file") {
      fileInput("file", label =  HTML("Must include following columns: 'species', 'decimalLongitude', and
                'decimalLatitude.' <br/> Don't forget to click Load!"), accept = '.csv')
      
    } 
  })
  
    output$mptp <- renderUI({
    if(input$datasource == "ATN") {
      selectInput("maptype", "Map Types", 
                  choices = c(
                    `Select One` = "",
                    `Heat Map` = "heat",
                    `Point Map` = "point",
                    `Trajectories` = "traj",
                    `Kernel Density` = "kernel"
                  ), multiple = FALSE)
    }
    else if(input$datasource == "OBIS") {
      selectInput("maptype", "Map Types", 
                  choices = c(
                    `Select One` = "",
                    `Heat Map` = "heat",
                    `Point Map` = "point"
                  ), multiple = FALSE)    }
    
    else if(input$datasource == "ATN and OBIS") {
      #actionButton("loadBoth", "Load")
      selectInput("maptype", "Map Types", 
                  choices = c(
                    `Select One` = "",
                    `Heat Map` = "heat",
                    `Point Map` = "point"
                  ), multiple = FALSE)  }
      else if(input$datasource == "Load in .csv file") {
        selectInput("maptype", "Map Types", 
                    choices = c(
                      `Select One` = "",
                      `Heat Map` = "heat",
                      `Point Map` = "point"
                    ), multiple = FALSE)
      }
  })
    
    output$confidence <- renderUI({
      if (input$datasource == "ATN" && input$maptype == "kernel") {
        sliderInput("confidence", "Confidence Interval Percentage",
                    min = 1, max = 100,
                    value = 95)
      }
    })
    
    output$colorby <- renderUI({
      if(input$datasource == "ATN" && (input$maptype == "traj")) {
            radioButtons("colorby", "Color by:",
             choices = c("species", "individual"),
             inline = F, selected = "species")
      }
    })
    
    output$numInds <- renderUI({
      if((input$datasource == "ATN") && (input$maptype == "traj" || input$maptype == "kernel")) {
        sliderInput("numInds", "Num. individuals per species",
                    min = 1, max = 12,
                    value = 2)
      }
    })


  observeEvent(
    input$species,
    if(input$datasource == "OBIS" || input$datasource == "ATN and OBIS") {
      output$startLoading <- renderText("May take a few seconds after Load is clicked...")
    }
  )
    
  observeEvent(
    input$loadData, 
    if(input$datasource == "OBIS") {
      obis <<- pacificProcessing(loadOBIS(input$species))
      output$startLoading <- NULL
      output$loaded <- renderUI("Done!")
    }
    else if(input$datasource == "ATN") {
      #output$startLoading <- renderText("Loading...")
      atn <<- loadATN(input$species)
      output$startLoading <- NULL
      output$loaded <- renderUI("Done!")
    }
    else if(input$datasource == "ATN and OBIS") {
      atnInput <- getATNnames()[which(getATNnames() %in% input$species)]
      obisInput <- getOBISnames()[which(getOBISnames() %in% input$species)]
      if (length(atnInput) != 0) {
        atn <<- loadATN(atnInput) %>% dplyr::select(species, decimalLongitude, decimalLatitude)
      }
      else {
        atn <<- NULL
      }
      if (length(obisInput) != 0) {
        obis <<- pacificProcessing(loadOBIS(obisInput)) %>% dplyr::select(species, decimalLongitude, decimalLatitude)
      }
      else {
        obis <<- NULL
      }
      # atn <<- loadATN(atnInput) %>% dplyr::select(species, decimalLongitude, decimalLatitude)
      # obis <<- pacificProcessing(loadOBIS(obisInput)) %>% dplyr::select(species, decimalLongitude, decimalLatitude)
      both <<- rbind(atn, obis)
      output$startLoading <- NULL
      output$loaded <- renderUI("Done!")
    }
    else if(input$datasource == "Load in .csv file") {
      #use loaded in csv and process
      inFile <- input$file
      print(inFile)
      customTable <<- read.csv(inFile$datapath) %>% pacificProcessing()
      output$startLoading <- NULL
      output$loaded <- renderUI("Done!")
    }
    
  )
  
  
  observeEvent(
    input$clear,
    if (TRUE) {
      output$map <- renderLeaflet({
        leaflet() %>%
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
      })
      atn <<- NULL
      obis <<- NULL
      both <<- NULL
      customTable <<- NULL
      output$loaded <- NULL
    }
  )
  
  
  observeEvent(
    input$mapButton,
    output$map <- renderLeaflet({
      if (input$datasource == "ATN") {
        if (input$maptype == "heat") pacificMapHeatmap(atn)
        else if (input$maptype == "point") pacificMapPoints(atn)
        else if (input$maptype == "kernel") plot.mcp(atn, numInds = input$numInds, conf = input$confidence)
        else pacificMapLines(atn, numInds = input$numInds, cb = input$colorby)
      } else if (input$datasource == "OBIS") {
        if (input$maptype == "point") pacificMapPoints(obis)
        else pacificMapHeatmap(obis)
      } else if (input$datasource == "ATN and OBIS") {
        if (input$maptype == "point") pacificMapPoints(both)
        else pacificMapHeatmap(both)
      }
      else if (input$datasource == "Load in .csv file") {
        if (input$maptype == "heat") pacificMapHeatmap(customTable)
        else if (input$maptype == "point") pacificMapPoints(customTable)
      }
    })
  )
  output$info <- renderUI({
    tagList("For more information please see ", url)
  })
  url <- a("our GitHub Wiki", href="https://github.com/gabezuckerman/Marine_Migratory/wiki")
  output$info2 <- renderUI({
    HTML("From top left (clockwise): California sea lions, Humpback whale, Black footed albatross, Sea otters<br/>Photo credit: José G. Martínez-Fonseca")
  })
})
