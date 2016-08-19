
library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
library(RCurl)
library(devtools)
library(shinyjs)
library(V8)
library(pbapply)
library(plotly)
# library(importr)
library(lubridate)


# devtools::install_github("rstudio/shinydashboard")
library(shinydashboard)

jscode <- "shinyjs.refresh = function() { history.go(0); }"

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Monitoring data"),
                    
                    dashboardSidebar(
                      width = 250,
                      paste("Time:",Sys.time()),
                      sidebarMenu(
                        
                       
                        fileInput('files', 'choose csv File (from databases)', multiple = TRUE,
                                  accept = c(".csv")),
                        
                         selectInput(
                          "database", "Network",
                          c(
                            "AURN" = "archive", 
                            "WALES" = "Waq",
                            "AQENGLAND" = "aqengland",
                            "KENT" = "kent",
                            "Nothern Ireland" = "niarc",
                            "North Lincs" = "nlincs",
                            "Scotland" = "scotarc"
                          )
                        ),
                        
                        selectInput(
                          "variable", "Pollutant",
                          c(
                            "NO2" = "no2", "O3" = "o3"
                          )
                        ),
                        
                        
                        # selectInput(
                        #   "start", "Start Day",
                        #   c(
                        #     "2016-08-15" = "2016-08-15", "2016-08-16" = "2016-08-16"
                        #   )
                        # ),
                        # 
                        # selectInput(
                        #   "end", "End Day",
                        #   c(
                        #     "2016-08-15" = "2016-08-15", "2016-08-16" = "2016-08-16", "2016-08-17" = "2016-08-17"
                        #   )
                        # ),
                        
                        
                        dateInput("start", "Start Day",
                                  value = "2016-08-15", format = "yyyy-mm-dd"),

                        dateInput("end", "End Day",
                                  value = "2016-08-15", format = "yyyy-mm-dd"),
            
                        fluidRow(
                          column (3,offset = 1,
                                  actionButton("goButton", strong("Process xlsm file"), icon("cog", lib = "glyphicon"), 
                                               style="color: #000000; background-color: #ffff7f ; border-color: #ffff7f"), # width = 150
                                  tableOutput('contents')
                          )),
                        
                        
                        menuItem("Statistics", tabName = "Stats", icon = icon("th")),
                        menuItem("Map", tabName = "MAP", icon = icon("th")),
                        menuItem("Additional stuff", tabName = "Add_Stuff", icon = icon("th")),
                        
                        
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
                      tabItems(
                        
                        # First tab content
                        tabItem(tabName = "MAP",
                                fluidRow(
                                  tabBox(
                                    height = 750, width = 950, selected = tags$b("Monitoring stations"),
                                    tabPanel(
                                      tags$b("Monitoring stations"), leafletOutput('myMap', height = 650, width = 750)
                                    )
                                  )
                                )),
                        
                        
                        
                        # Second tab content
                        tabItem(tabName = "Stats",
                                fluidRow(
                                  tabBox(
                                    height = 750, width = 950, selected = tags$b("Daily Averages"),
                                    tabPanel(
                                      tags$b("Daily Averages"), tableOutput('stats')
                                    ),
                                    # tabPanel(
                                    #   tags$b("Summary_plot_1"),
                                    #   box(title = " ", height = 550, width = 750,includeHTML("sectors_Buenos_Aires.html")
                                    #   )
                                    # ),
                                    tabPanel(
                                      tags$b("Time Series"), 
                                      box(title = " ", height = 550, width = 650, plotlyOutput("sectors_plot"))
                                    ),
                                    
                                    tabPanel(
                                      tags$b("Summary II"), leafletOutput('myMap_2', height = 650, width = 750)
                                    )
                                  )
                                )),
                        
                        
                        
                        tabItem(tabName = "Add_Stuff",
                                fluidRow(
                                  tabBox(
                                    height = 750, width = 950, selected = tags$b("Data"), 
                                    tabPanel(
                                      tags$b("Data"), tableOutput('subsectors')
                                    ),
                                    tabPanel(
                                      tags$b("Summary_plot"),
                                      column(3, plotlyOutput("subsectors_plot", height = "700px", width = "850px"))
                                    )
                                  )
                                ))
                      ))
)