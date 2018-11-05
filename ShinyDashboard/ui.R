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

#points <- st_read("../../../../Dropbox/ABSK_CWD data/RiskModelUpdate/pts_inextent_UTM12N.shp")

WMU163_keys <- c(
  553, 10856, 10859, 10903, 10905, 10939, 10942, 10950, 12349,
  12567, 13395, 13473, 13927, 15139, 44895, 44898, 44933, 44935,
  44948
)

variable_list <- list(
  response = "cwd",
  global = c("sp", "sex_1", "harv", "time"),
  dist = c("e_min", "e_aver"),
  hum = c("Dtown", "Droad", "Road3km", "Road6km", "Road12km"),
  ter = c("dRiv", "dStrm", "Stream3km", "Stream6km", "Stream12km", "rugg3", "rugg6", "rugg12"),
  LCV = c(
    "Pcover3", "Pagri3", "Pgrass3", "Popen3", "Pcover6", "Pagri6", "Pgrass6", "Popen6",
    "Pcover12", "Ag12", "Pgrass12", "Popen12"
  ) # ,
  # soil = c("coarse3", "coarse6", "coarse12", "fine12", "fine3", "fine6", "med12", "med3", "med6")
)

#pred <- raster("../../../Dropbox/ABSK_CWD data/RiskModelUpdate/predict_R.tif")
#WMUs <- st_read("../../../../Dropbox/ABSK_CWD data/RiskModelUpdate/AB_WMUs.shp")
#rastercol <-  colorRampPalette(c("#49AD3F","#f1F904","#D73027"), bias = 2)(256)


ui <- dashboardPage(
  dashboardHeader(
    title="Migratory Marine Animals",
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
                 selectInput("species", "Species",
                             choices = read.csv("../DataProcessing/obis_spec_cts_named.csv",
                                                stringsAsFactors = F)$commonName, 
                             multiple = TRUE),
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