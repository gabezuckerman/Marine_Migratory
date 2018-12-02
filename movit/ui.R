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

#file for saving map
jsfile <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js" 

ui <- shinyUI(
  navbarPage(title = div(
                  id = "logo",
                  img(src = "MOViT-theme.jpg",
                      height = 50,
                      width = 250
                     )
                  ),
             windowTitle = "MOViT",
            tabPanel("Interactive Map",
                      div(class="outer",
                          tags$head(
                            # Include custom CSS from Superzip example
                            includeCSS("styles.css")
                          ),
                          tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                          tags$head(tags$script(src = jsfile)),
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Load Data"),
                                        
                                        radioButtons("datasource", "Which datasource would you like to use?",
                                                     choices = c("ATN", "OBIS", "ATN and OBIS", "Load in .csv file"), inline = T),
                                        uiOutput("datasource"),
                                        
                                        actionButton("loadData", "Load"),
                                        uiOutput("startLoading", inline = T),
                                        uiOutput("loaded", inline = T), 
                                      
                                        
                                        #uiOutput("loadData"),
                                        h2("Map Type"),
                                        uiOutput("mptp"),
                                        uiOutput("confidence"),
                                        uiOutput("numInds"),
                                        uiOutput("colorby"),
                                        actionButton("mapButton", "Map It!", width = "50%"),
                                        actionButton("clear", "Clear Data and Map")
                          )
                      ),
                      tags$div(id="cite",
                               'Created by Erin Westeen, Ben Goldstein, and Gabe Zuckerman.', br(),
                               'In association with Emily Owen and Stacy Baez of the Pew Bertarelli Ocean Legacy'
                      )
             ),
            tabPanel("Information",
                     uiOutput("info"),
                     img(
                       src = "info.png",
                       height = 550,
                       width = 825,
                       align = "center"),
                     uiOutput("info2"))
            
  )
)
  
  
 
