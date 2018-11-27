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
#get commonNames for OBIS
source('mmafunctions.R')

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
    
    else if(input$datasource == "Both") {
      actionButton("loadBoth", "Load")
      selectInput("species", "Species (OBIS and ATN)",
                  choices = c(
                    `Select One or More` = "",
                    getATNnames()
                  ), multiple = TRUE)
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
    
    else if(input$datasource == "Both") {
      actionButton("loadBoth", "Load")
      selectInput("maptype", "Map Types", 
                  choices = c(
                    `Select One` = "",
                    `Joint Map` = "joint"
                  ), multiple = FALSE)    }
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
    else if(input$datasource == "Both") {
      atn <<- loadATN(input$species)
      obis <<- pacificProcessing(loadOBIS(input$species))
      output$loaded <- renderText("Done!")
      #output$ATNTable <- renderDataTable(atn, options = list(scrollX = TRUE))
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
      output$loaded <- NULL
    }
  )
  
  
  observeEvent(
    input$mapButton,
    output$map <- renderLeaflet({
      if (input$datasource == "ATN") {
        if (input$maptype == "heat") pacificMapHeatmap(atn)
        else if (input$maptype == "point") pacificMapPoints(atn)
        else if (input$maptype == "kernel") plot.mcp(atn, numInds = input$numInds, conf = 95)
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
})

### TODO
# Ben:
# Plot both OBIS and ATN at the same time
# Gabe:
# Get everything in one pane
# Erin:
# Get kernel density and BBMM into app (or give to Gabe to pu into app)