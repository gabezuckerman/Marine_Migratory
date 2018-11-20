ui <- shinyUI(
  navbarPage(title = div(
                  id = "logo",
                  img(src = "MOViT-theme.jpg",
                      height = 50,
                      width = 250
                     )
                  ),
            tabPanel("Interactive Map",
                      div(class="outer",
                          tags$head(
                            # Include custom CSS from Superzip example
                            includeCSS("styles.css")
                          ),
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Load Data"),
                                        
                                        radioButtons("datasource", "If you are using opensource, which datasource would you like to use?",
                                                     choices = c("OBIS", "ATN", "Both"), inline = T),
                                        uiOutput("datasource"),
                                        actionButton("loadData", "Load"),
                                        uiOutput("loaded", inline = T),
                                        #uiOutput("loadData"),
                                        h2("Map Options"),
                                        selectInput("maptype", "Map Types", 
                                                    choices = c(
                                                      `Select One` = "",
                                                      `Heat Map` = "heat",
                                                      `Point Map` = "point",
                                                      `Trajectories` = "traj"
                                                      
                                                    ), multiple = FALSE
                                        ),
                                        actionButton("mapButton", "Map It!", width = "50%"),
                                        # sliderInput("numInds", "Num. individuals per species",
                                        #             min = 1, max = 12,
                                        #             value = 2),
                                        radioButtons("colorby", "Color by:",
                                                     choices = c("species", "individual"),
                                                     inline = F, selected = "species"),
                                        actionButton("clear", "Clear Data and Map")
                          )
                      ),
                      tags$div(id="cite",
                               'Created by Erin Westeen, Ben Goldstein, and Gabe Zuckerman.', br(),
                               'In association with Emily Owen and Stacy Baez of the Pew Bertarelli Ocean Legacy'
                      )
             ),
            tags$head(tags$style('.navbar-nav {width: 90%;}
                                    .navbar-nav :first-child{float:right}'))
  )
)
  
  
 
