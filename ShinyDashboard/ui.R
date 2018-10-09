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
    title="Estimating Alberta's CWD Risk",
    titleWidth = 300
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
                 radioButtons("new", "Extracting data to new points?",
                              choices = c("yes", "no"),
                              inline = T, selected = "no"
                 ),
                 fileInput("datafile", "Choose CSV file",
                           accept = c("text/csv", "text/comma-separated-values")),
                 uiOutput("sexcol"),
                 uiOutput("spcol"),
                 uiOutput("harvcol"),
                 uiOutput("datecol"),
                 uiOutput("latcol"),
                 uiOutput("longcol"),
                 uiOutput("extract"), # need to make this dynamic on the above checkbox.
                 uiOutput("extractbutton"),
                 uiOutput("button"),
                 uiOutput("text"),
                 br(),
                 br(),
                 uiOutput("download")
        )),
    div(id = "tab3_sidebar",
        menuItem("Choose Variables", startExpanded = TRUE, tabName = "vars", icon = icon("th"),
                 checkboxGroupInput("Glob_Input", "Global Variables", # make this reactive based on a scale selector?
                                    choices = c(
                                      Species = "sp",
                                      Sex = "sex_1",
                                      `Harvest Method` = "harv",
                                      `Years Since 1st Positive` = "time"
                                    ),
                                    selected = variable_list$global
                 ),
                 selectInput("Dist_Input", "Proximity Variable",
                             choices = c(
                               `Select One` = "",
                               `Mean Distance to Positives` = "e_aver",
                               `Nearest Distance to Positive` = "e_min"
                             ), selected = "e_min"
                 ),
                 selectInput("Ter_Input", "Terrain Variable",
                             choices = c(
                               `Select One or More` = "",
                               `Distance to Major River` = "dRiv",
                               `Distance to Stream` = "dStrm",
                               `Ruggedness - 3km2` = "rugg3",
                               `Ruggedness - 6km2` = "rugg6",
                               `Ruggedness - 12km2` = "rugg12",
                               `Stream Density - 3km2` = "Stream3km",
                               `Stream Density - 6km2` = "Stream6km",
                               `Stream Density - 12km2` = "Stream12km"
                             ), multiple = TRUE,
                             selected = c("dRiv", "dStrm")
                 ),
                 selectInput("Hum_Input", "Human Disturbance Variable", # might want to make this a check box? but what about correlations?
                             choices = c(
                               `Select One Or More` = "",
                               `Road Density -- 3km2` = "Road3km",
                               `Road Density -- 6km2` = "Road6km",
                               `Road Density -- 12km2` = "Road12km",
                               `Distance to Town` = "Dtown",
                               `Distance to Road` = "Droad"
                             ), multiple = TRUE
                 ),
                 selectInput("LCV_Input", "Landcover Variable",
                             choices = c(
                               `Select One` = "",
                               `Agriculture -- 3km2` = "Pagri3",
                               `Agriculture -- 6km2` = "Pagri6",
                               `Agriculture -- 12km2` = "AG12",
                               `Cover -- 3km2` = "Pcover3",
                               `Cover -- 6km2` = "Pcover6",
                               `Cover -- 12km2` = "Pcover12",
                               `Open -- 3km2` = "Popen3",
                               `Open -- 6km2` = "Popen6",
                               `Open -- 12km2` = "Popen12",
                               `Grassland -- 3km2` = "Pgrass3",
                               `Grassland -- 6km2` = "Pgrass6",
                               `Grassland -- 12km2` = "Pgrass12"
                             ), selectize = F, selected = "AG12"
                 ),
                 textInput("interaction", "Interactions?", "dRiv*AG12"),
                 # selectInput("Soil_Input", "Soil Texture Variable", # make this reactive based on a scale selector?
                 #   choices = c(
                 #     `Select One` = "",
                 #     `Fine -- 3km2` = "fine3",
                 #     `Fine -- 6km2` = "fine6",
                 #     `Fine -- 12km2` = "fine12",
                 #     `Medium -- 3km2` = "med3",
                 #     `Medium -- 6km2` = "med6",
                 #     `Medium -- 12km2` = "med12",
                 #     `Coarse -- 3km2` = "coarse3",
                 #     `Coarse -- 6km2` = "coarse6",
                 #     `Coarse -- 12km2` = "coarse12"
                 #   ), selectize = F
                 # ),
                 actionButton("goButton", "Fit Model!", width = "50%")
                 
        )),
    div(id = "tab4_sidebar",
        menuItem("Choose Map Constants", tabName = "cons", startExpanded = TRUE, icon = icon("th"),
                 numericInput("maptime",
                              label = "Year",
                              value = 2018
                 ),
                 selectizeInput("mapsp", "Species", c(
                   "Mule Deer" = 1,
                   "White-Tailed Deer" = 0
                 )),
                 selectizeInput("mapsex", "Sex",
                                choices = c("Male" = 1, "Female" = 0),
                                selected = 1
                 ),
                 actionButton("mapButton", "Map It!", width = "50%"),
                 br(),
                 br(),
                 downloadButton("downloadMap", "Download Prediction Raster")
        ))
    
  ),
  dashboardBody(
    # useShinyjs(),
    # tabItems(
    #   tabItem(tabName = "load",
    #           h2("review data")
    #   )
    # )
    #main tab setup
    tabsetPanel(
      id = "navbar",
      tabPanel(title="Information",id="tab1",value='tab1_val',
               valueBoxOutput('tab1_valuebox', width = 12),
               valueBoxOutput('info_box', width = 12)),
      tabPanel(title="Load and Review Data",id="tab2",value='tab2_val',
               valueBoxOutput('tab2_valuebox', width = 12),
               DT::dataTableOutput("filetable")
      ),
      tabPanel(title="Regression Results",id="tab3",value='tab3_val',
               valueBoxOutput('tab3_valuebox', width = 12),
               strong(uiOutput("setup")),
               uiOutput("call"), br(),
               strong(uiOutput("setup2")),
               DT::dataTableOutput("summary"),
               uiOutput("toMap")),
      tabPanel(title="Risk Map",id="tab4",value='tab4_val',
               valueBoxOutput('tab4_valuebox', width = 12),
               leafletOutput("map", height = 800)
      )
    )
  )
)