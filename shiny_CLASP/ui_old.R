
# library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
# library(RColorBrewer)
# library(sp)
# library(RCurl)
# library(devtools)
library(shinyjs)
# library(V8)
# library(pbapply)
# library(plotly)
# library(importr)
library(lubridate)
library(dygraphs)
library(DT)


# devtools::install_github("rstudio/shinydashboard")
library(shinydashboard)

jscode <- "shinyjs.refresh = function() { history.go(0); }"

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Monitoring data"),
                    
                    dashboardSidebar(
                      width = 250,
                      paste("Time:",Sys.time()),
                      sidebarMenu(
                        
                       
                        # fileInput('files', 'choose csv File (from databases)', multiple = TRUE,
                        #           accept = c(".csv")),
                        
                        
                        
                        
                        selectInput(
                          "variable", "Pollutant",
                          c(
                            "NO2" = "no2", "O3" = "o3", "PM10" = "pm10", "PM2.5" = "pm25",
                            "V10" = "v10", "V25" = "v25"
                          )
                        ),
                        

                        
                        dateInput("start", "Start Day",
                                  value = "2016-09-23", format = "yyyy-mm-dd"),

                        dateInput("end", "End Day",
                                  value = "2016-09-24", format = "yyyy-mm-dd"),
                      #  br(),
                      
                      
                      # selectInput(
                      #   "database", "Network",
                      #   c(
                      #     "AURN" = "archive", 
                      #     "WALES" = "Waq",
                      #     "AQENGLAND" = "aqengland",
                      #     "KENT" = "kent",
                      #     "Nothern Ireland" = "niarc",
                      #     "North Lincs" = "nlincs",
                      #     "Scotland" = "scotarc"
                      #   ), selected = "archive"
                      # ),
         
                      #  selectInput("site", "Site:", ""),
                      
                      selectInput("database", "Network",                  
                                  c("AURN" = "archive",
                                    "WALES" = "Waq"
                                    )),
                      

                      conditionalPanel(
                        condition = "input.database == 'archive'",
                        selectInput("site_archive", "Site", c("Aberdeen" = "abd", "Aberdeen Union Street Roadside" = "abd7","
                                                      Aberdeen Wellington Road" ="abd8",
                                                      "Armagh Roadside" = "arm6"))),
                      conditionalPanel(
                        condition = "input.database == 'Waq'",
                        selectInput("site_Waq", "Site", c("Anglesey Brynteg" = "ang5",
                                                      "Anglesey Felin Cafnan" = "ang10",
                                                      "Anglesey Llynfaes" = "ang2",
                                                      "Anglesey Penhesgyn" = "ang7",
                                                      "Cardiff" = "card"))),                        


                        fluidRow(
                          column (3,offset = 1,
                                  actionButton("goButton", strong("Process data"), icon("cog", lib = "glyphicon"), 
                                               style="color: #000000; background-color: #ffff7f ; border-color: #ffff7f"), # width = 150
                                  DT::dataTableOutput('contents')
                          )),

                        
                        menuItem("Statistics", tabName = "Stats", icon = icon("th")),
                        menuItem("Map", tabName = "MAP", icon = icon("th")),
                        # menuItem("Additional stuff", tabName = "Add_Stuff", icon = icon("th")),
                        
                        
                        fluidRow(
                          column (3,offset = 1,
                                  br(), #hr()
                                  useShinyjs(),
                                  extendShinyjs(text = jscode),
                                  actionButton("refresh", "refresh", icon("paper-plane"), 
                                               style="color: #fff; background-color: #7fbf7f; border-color: #2e6da4", width = 150)
                          ))
                        
                      )),
                    
                    
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      tabItems(
                        
                        # First tab content
                        tabItem(tabName = "MAP",
                                fluidRow(
                                  tabBox(
                                    height = 750, width = 950, selected = tags$b("Monitoring stations"),
                                    tabPanel(
                                      tags$b("Monitoring stations"), leafletOutput('myMap_all', height = 650, width = 750)
                                    ),
                                    tabPanel(
                                      tags$b("Site Station"), leafletOutput('myMap', height = 650, width = 750)
                                    )
                                  )
                                )),
                        
                        
                        
                        # Second tab content
                        tabItem(tabName = "Stats",
                                fluidRow(
                                  tabBox(
                                    height = 750, width = 550, selected = tags$b("Daily Averages - all stations"),
                                    tabPanel(
                                      tags$b("Daily Averages - all stations"), DT::dataTableOutput('stats_all')
                                    ),
                                    
                                    tabPanel(
                                      tags$b("Daily Averages by station"), DT::dataTableOutput('statss')
                                    ),

                                    tabPanel(
                                      tags$b("Time Series"), 
                                      box(title = " ", height = 750, width = 550, dygraphOutput("mean_dygraphs", width = "100%", height = "200px"),
                                                                                  dygraphOutput("z_score_dygraphs", width = "100%", height = "200px"))
                                    )
                                  )
                                ))
                        
                        
                        
                        # tabItem(tabName = "Add_Stuff",
                        #         fluidRow(
                        #           tabBox(
                        #             height = 750, width = 950, selected = tags$b("Data"), 
                        #             tabPanel(
                        #               tags$b("Data"), tableOutput('subsectors')
                        #             ),
                        #             tabPanel(
                        #               tags$b("Summary_plot"),
                        #               column(3, plotlyOutput("subsectors_plot", height = "700px", width = "850px"))
                        #             )
                        #           )
                        #         ))
                      ))
)