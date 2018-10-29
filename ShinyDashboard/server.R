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


#obis plotting functions
plot.map<- function(database,center,...){
  Obj <- map(database,...,plot=F)
  coord <- cbind(Obj[[1]],Obj[[2]])
  
  # split up the coordinates
  id <- rle(!is.na(coord[,1]))
  id <- matrix(c(1,cumsum(id$lengths)),ncol=2,byrow=T)
  polygons <- apply(id,1,function(i){coord[i[1]:i[2],]})
  
  # split up polygons that differ too much
  polygons <- lapply(polygons,function(x){
    x[,1] <- x[,1] + center
    x[,1] <- ifelse(x[,1]>180,x[,1]-360,x[,1])
    if(sum(diff(x[,1])>300,na.rm=T) >0){
      id <- x[,1] < 0
      x <- rbind(x[id,],c(NA,NA),x[!id,])
    }
    x
  })
  # reconstruct the object
  polygons <- do.call(rbind,polygons)
  Obj[[1]] <- polygons[,1]
  Obj[[2]] <- polygons[,2]
  
  map(Obj,...)
}


plot_species_obis <- function(sci_name = NULL, common_name = NULL, 
                              input_data = chords, plot_fn = "Plot_Default.png") {
  name <- NULL
  if (!is.null(common_name)) {
    spec_data <- input_data[input_data$commonName == common_name,]
    name <- common_name
  } else if (!is.null(sci_name)) {
    spec_data <- input_data[input_data$species == sci_name,]
    name <- sci2comm(get_uid(sci_name)) %>% as.character()
  } else {
    stop("In plot_species_obis, please specify a scientific or common name")
  }
  
  shift <- 200
  data_to_plot <- spec_data
  
  png(plot_fn, width = 1800, height = 900)
  plot.map("world", center = shift, col = "white", bg = "gray96", 
           fill = TRUE, ylim = c(-60,90), mar = c(0,0,0,0))
  
  data_to_plot$decimalLongitude[spec_data$decimalLongitude < 0] <- data_to_plot$decimalLongitude[spec_data$decimalLongitude < 0] + shift
  data_to_plot$decimalLongitude[spec_data$decimalLongitude > 0] <- data_to_plot$decimalLongitude[spec_data$decimalLongitude > 0] + (shift-360)
  
  points(data_to_plot$decimalLongitude, data_to_plot$decimalLatitude, col = alpha("blue", 0.1))
  title(paste0("OBIS data for occurrence of ", name), cex.main=3)
  
  dev.off()
}



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

# source("DataProcessing/OBIS_data_proc.R")


#pred <- raster("../../../../Dropbox/ABSK_CWD data/RiskModelUpdate/predict_R.tif")
#WMUs <- st_read("../../../../Dropbox/ABSK_CWD data/RiskModelUpdate/AB_WMUs.shp")
#rastercol <-  colorRampPalette(c("#49AD3F","#f1F904","#D73027"), bias = 2)(256)

server <- shinyServer(function(input, output, session) {
  
  values <- reactiveValues(selectedTab = 1)
  #syncs sidebar and main tabs
  observeEvent(input$navbar, {
    toggle("tab1_sidebar", condition = input$navbar == "tab1_val")
    toggle("tab2_sidebar", condition = input$navbar == "tab2_val")
    toggle("tab3_sidebar", condition = input$navbar == "tab3_val")
    toggle("tab4_sidebar", condition = input$navbar == "tab4_val")
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
    box(status = 'info', 'Select which variables to model.', br(),
        'Click Fit Model! to generate Regression Function and Statistics.', br(),
        'Once regression is complete, click See Map! to continue to Map')
  })
  output$tab3_valuebox <- renderValueBox({
    box(status = 'info', 'Choose map type to begin mapping process.')
  })
  
  observeEvent(
    input$loadData,
    output$speciesTable <-renderDataTable(obis_batch(input$species), options = list(scrollX = TRUE))
    # species <- input$species
    # datasource <- input$new
    # print(datasource, species)
    )
  
  
  
  # This function is repsonsible for loading in the selected file
  #this is the functionality for the second tab
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read_csv(infile$datapath)
  })
  
  # This previews the CSV data file
  output$filetable <- renderDataTable({
    DT::datatable(filedata(), options = list(scrollX = TRUE))
  })
  
  # identify year, sex, sp, cwd, and harvest columns
  output$sexcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$new == "yes") return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("sex", "Sex Column (0/1):", items)
  })
  output$harvcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$new == "yes") return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("harv", "Harvest Method Column (0/1):", items)
  })
  output$spcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$new == "yes") return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("sp", "Species Column (0/1):", items)
  })
  output$datecol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$new == "yes") return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("date", "Date Column", items)
  })
  output$latcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("lat", "Easting (UTM 12N NAD83)", items)
  })
  output$longcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    items <- names(df)
    names(items) <- items
    selectInput("long", "Northing (UTM 12N NAD83)", items)
  })
  
  #Loading sidebar functionality ie Tab 2
  output$button <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$new == "yes") return(NULL)
    actionButton("modelbutton", "Build Regression!")
  })
  
  # on click of "Build Regression!"
  shinyjs::onclick('modelbutton',expr={
    # move to Regression Results
    updateTabsetPanel(session, "navbar", 'tab3_val')
  })
  

  
  #extraction--needs work!
  output$extract <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$new == "no") return(NULL)
    
    selectInput("extract", "Variables to Extract", variable_list[3:6],
                multiple = T, selected = c("e_min", "Pagri12", "Driver", "Dstreams")
    )
  })
  
  output$extractbutton <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (input$new == "no") return(NULL)
    actionButton("extractButton", "Extract Variables!")
  })
  
  output$new <- renderUI({
    if (input$new == "opensource") {
      selectInput("ds", "Data Sources",
                  choices = c(
                    `Select One or More` = "",
                    `ATN` = "atn",
                    `OBIS` = "obis"
                  ), multiple = TRUE
                )
    }
    else if (input$new == "both") {
      selectInput("ds", "Data Sources",
                  choices = c(
                    `Select One or More` = "",
                    `ATN` = "atn",
                    `OBIS` = "obis"
                  ), multiple = TRUE
                )
      
    }
    # else if (input$new == "both") {
    #   fileInput("datafile", "Choose CSV file to load in",
    #             accept = c("text/csv", "text/comma-separated-values"))
    #  }
    else {
      fileInput("datafile", "Choose CSV file to load in",
                accept = c("text/csv", "text/comma-separated-values"))
    }
  })
  
  text <- eventReactive(
    input$extractButton,
    "Extracting Environmental Variables... this may take a while..."
  )
  
  output$text <- renderText({
    text()
    progress <- Progress$new(session, min = 1, max = 30)
    on.exit(progress$close())
    
    progress$set(
      message = "Calculation in progress",
      detail = "Table will reload when complete."
    )
    
    # dummy progress
    for (i in 1:30) {
      progress$set(value = i)
      Sys.sleep(0.5)
    }
  })
  
  #Regression Functionality ie Tab 3
  regressioncall <- eventReactive(input$goButton, {
    as.formula(paste(
      "cwd ~",
      paste(c(
        glue_collapse(input$Glob_Input, " + "),
        input$Dist_Input,
        glue_collapse(input$Hum_Input, " + "),
        glue_collapse(input$Ter_Input, " + "),
        input$LCV_Input),
        collapse = " + "
      ), "+", input$interaction
    ))
  })
  
  #need to change so it uses loaded in data instead of "points"
  logit <- reactive({
    glm(regressioncall(), data = filter(points, !(key %in% WMU163_keys)), family = binomial(link = "logit"))
  })
  
  
  output$setup <-renderText({
    'Regression Function:'
  })
  
  output$setup2 <- renderText({
    'Regression Statistics:'
  })
  output$call <- renderPrint({
    regressioncall()
  })
  
  
  output$summary <- renderDataTable({
    DT::datatable(broom::tidy(logit()))
  })
  
  output$toMap <- renderUI({
    if (is.null(logit())) return(NULL)
    actionButton("toMap", "See Map!")
  })
  
  shinyjs::onclick('toMap',expr={
    # move to Map Results
    updateTabsetPanel(session, "navbar", 'tab4_val')
  })
  
  
  #MAP tab ie tab 4
  
  # predictions <- reactive(exp(
  #   raster::predict(
  #     object = rasters,
  #     model = logit(),
  #     fun = predict,
  #     const = data.frame(
  #       time = (input$maptime - 2001),
  #       sex_1 = as.numeric(input$mapsex),
  #       harv = 1,
  #       sp = as.numeric(input$mapsp)
  #     )
  #   )
  # ))
  # main <- reactive(
  #   paste(
  #     "CWD Risk", (input$maptime),
  #     ifelse(input$mapsex == "1", "Male", "Female"),
  #     ifelse(input$mapsp == "1", "Mule Deer", "White-Tailed Deer"), sep = "_"
  #   )
  # )
  
  
  # cannot work reactively right now, so just displaying map with WMUs layer
  #m <- mapview::mapview(st_geometry(WMUs), layer.name = "WMUs") #+
  #mapview::mapview(pred, col.regions = rastercol, layer.name = "Predicted Risk") #+
  #mapview::mapview(filter(points, cwd == 1), layer.name = "Positives")
  
  
  # m <- m + mapview::mapview(pred, col.regions = rastercol)
  
  # observeEvent(
  #   input$mapButton,
  #output$map <- leaflet::renderLeaflet({m@map})
  # )
  
  # output$map <- renderPlot({
  #   pred <- data.frame(rasterToPoints(predictions()))
  #   colnames(pred) <- c("X", "Y", "risk")
  #   pred$cut <- Hmisc::cut2(x = pred$risk,
  #                    cuts = signif(seq(range(pred$risk, na.rm = T)[1],
  #                                      range(pred$risk, na.rm = T)[2],
  #                                      length.out = 10), 3))
  #
  #   ggplot() +
  #     geom_sf(data = WMUs, aes(), colour = "#63636380", fill = "#fff7ec") +
  #     # add geom_sf_text() for label once updated on github
  #     geom_raster(data = pred, aes(X, Y, fill = cut)) +
  #     scale_fill_manual(values = gradient, name = "CWD risk") +
  #     geom_sf(data = rivers, aes(), colour = "#4eb3d360") +
  #     labs(x = "", y = "", title = main())
  #   })
  #
  
  # output$downloadMap <- renderUI({
  #   if (input$mapButton == 0) return(NULL)
  #   downloadButton("downloadMap", "Download Prediction Raster")
  # })
  
  #downloads created map, not reacitve
  output$downloadMap <- downloadHandler(
    filename = function() { paste0(main(),'.tif') },
    #predictions() not a thing yet, so cannot save it
    content = function(file) {
      raster::writeRaster(pred, file)
    }
  )
  
  
  
  
  # observeEvent(
  #   input$downloadMap,
  #   downloadHandler(
  #     filename = function() { paste0(main(),'.tif') },
  #     content = function(file) {
  #       raster::writeRaster(predictions(), file)
  #     }
  #   )
  # )
  
  # output$risk <- renderImage({
  #  list(src = "../OriginalRiskMap.png", height = "600", width = "400")
  # }, deleteFile = FALSE)
})