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
library(shinythemes)



ui <- dashboardPage(
  dashboardHeader(
    title="Migratory Ocean Visuaization Tool",
    titleWidth = 300, tags$li(a(href = 'https://www.pewtrusts.org/en/projects/pew-bertarelli-ocean-legacy',
                                img(src = 'pew.jpg',
                                    title = "Pew Bertarelli Ocean Legacy", height = "30px"),
                                style = "padding-top:10px; padding-bottom:10px;"),
                              class = "dropdown")
    ),
  dashboardSidebar(
    useShinyjs(),
    width = 300,
    #sidebar setup
    div(id = "tab1_sidebar",
        menuItem("Information", tabName = "info", startExpanded = TRUE, icon = icon("dashboard")
        )),
    div(id = "tab2_sidebar",
        menuItem("Load", tabName = "load", startExpanded = TRUE, icon = icon("th"),
                 radioButtons("new", "Would you like to use opensource data or load in your own?",
                              choices = c("opensource", "my own", "both"),
                              inline = F, selected = "opensource"
                 ),
                uiOutput("new"),
                radioButtons("datasource", "If you are using opensource, which datasource would you like to use?",
                             choices = c("OBIS", "ATN"), inline = T),
                uiOutput("datasource"),
                 # selectInput("species", "Species",
                 #             choices = c(
                 #               `Select One or More` = "",
                 #               `Whale Shark` = "Rhincodon typus",
                 #               `Loggerhead Seaturtle` = "Caretta caretta",
                 #               `Blue Whale` = "bWhale",
                 #               `White Shark` = "whiteShark",
                 #               `Laysan Albatross` = "albatross"
                 #             ), multiple = TRUE),
                     actionButton("loadData", "Load"),
                uiOutput("loadData")
        )),
    div(id = "tab3_sidebar",
        menuItem("Choose Map Type", tabName = "type", startExpanded = TRUE, icon = icon("th"),
                 selectInput("maptype", "Map Types", 
                             choices = c(
                               `Select One` = "",
                               `Heat Map` = "heat",
                               `Point Map` = "point",
                               `Trajectories` = "traj"
                               
                             ), multiple = FALSE
                   ),
                 actionButton("mapButton", "Map It!", width = "50%")
                 
        ))
    
  ),
  dashboardBody(
    #main tab setup
    tabsetPanel(
      id = "navbar",
      tabPanel(title="Information",id="tab1",value='tab1_val',
               valueBoxOutput('tab1_valuebox', width = 12),
                 img(
                   src = "PrettyMap.png",
                   height = 400,
                   width = 600,
                   align = "center"
                ),
               valueBoxOutput('info_box', width = 12)
               ),
      tabPanel(title="Load and Review Data",id="tab2",value='tab2_val',
               valueBoxOutput('tab2_valuebox', width = 12),
               DT::dataTableOutput("speciesTable")
      ),
      tabPanel(title="Map with MPA Recommendations",id="tab3",value='tab3_val',
               valueBoxOutput('tab3_valuebox', width = 12),
               leafletOutput("map", height = 500)
      )
    )
  )
)