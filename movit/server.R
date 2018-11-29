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

atn <- NULL
obis <- NULL
customTable <- NULL

#get commonNames for OBIS
source('movit-functions.R')

server <- shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      setView(lng = 180, lat = 0, zoom = 2)
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
                    getATNnames()
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
                    `Joint Map` = "joint"
                  ), multiple = FALSE)    }
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
        sliderInput("confidence", "Condifence Interval Percentage",
                    min = 1, max = 100,
                    value = 95)
      }
    })
    
    output$colorby <- renderUI({
      if(input$datasource == "ATN" && (input$maptype == "traj" || input$maptype == "kernel")) {
            radioButtons("colorby", "Color by:",
             choices = c("species", "individual"),
             inline = F, selected = "species")
      }
    })
    
    output$numSpecies <- renderUI({
      if((input$datasource == "ATN" ||  input$datasource == "ATN and OBIS") && (input$maptype == "traj" || input$maptype == "kernel" || input$maptype == "joint")) {
        sliderInput("numInds", "Num. individuals per species",
                    min = 1, max = 12,
                    value = 2)
      }
    })

  
  
  observeEvent(
    input$loadData, 
    if(input$datasource == "OBIS") {
      obis <<- pacificProcessing(loadOBIS(input$species))
      output$loaded <- renderText("Done!")
      #output$OBISTable <- renderDataTable(obis, options = list(scrollX = TRUE))
    }
    else if(input$datasource == "ATN") {
      atn <<- loadATN(input$species)
      output$loaded <- renderText("Done!")
      #output$ATNTable <- renderDataTable(atn, options = list(scrollX = TRUE))
    }
    else if(input$datasource == "ATN and OBIS") {
      atn <<- loadATN(input$species)
      obis <<- pacificProcessing(loadOBIS(input$species))
      output$loaded <- renderText("Done!")
      #output$ATNTable <- renderDataTable(atn, options = list(scrollX = TRUE))
    }
    else if(input$datasource == "Load in .csv file") {
      #use loaded in csv and process
      customTable <<- reactive({
        inFile <- input$file
        if (is.null(inFile))
          return(NULL)
        tbl <- read.csv(inFile$datapath, header=input$header, sep=input$sep,  dec = input$dec)
        return(tbl)
      })
      output$loaded <- renderText("Done!")
    }
    
  )
  
  
  observeEvent(
    input$clear,
    if (TRUE) {
      output$map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.OceanBasemap) %>%
          setView(lng = 180, lat = 0, zoom = 2)
      })
      atn <<- NULL
      obis <<- NULL
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
        pacificMapLines(atn, numInds = input$numInds, cb = input$colorby,
                        m = pacificMapHeatmap(obis, pass = T))
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
  
  

### TODO
# Ben:
# Plot both OBIS and ATN at the same time
# Gabe:
# Get everything in one pane
# Erin:
# Get kernel density and BBMM into app (or give to Gabe to pu into app)