
library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
library(shinydashboard)
# library(importr)
library(lubridate)
 
source("helpers.R")
source("import_stats.R")

# select a specifc day
day <- "2016-08-15"
# start <- "2016-08-15"
# end <- "2016-08-15"

# Sys.setenv(https_proxy="https://harproxy02:3128")

shinyServer(function(input, output) {
  
  
output$contents <- renderTable({  
 
  
##### run code below only if you use trials inputs files from Ricardo data base ############
#####---------------------------------------------------------------------------------------
############################################################################################ 
   
  # inFile <- input$files   #.csv files to be loaded (you can upload multiple files)
  # 
  # withProgress(message = "processing.....",  detail = 'this may take a while...', value = 0.25, { background= "blue"
  # # Number of times we'll go through the loop
  # n <- 100
  # 
  #  if (is.null(inFile) | input$goButton == 0)  # the start position of the button is 0 (OFF)
  #    return(NULL)
  # 
  # 
  # file <- inFile$datapath
  # 
  # info_sites <- read.csv(paste("info_sites_",input$database,".csv", sep = ""))
  # stats <- read.csv(paste("stats_",input$database, ".csv", sep = ""))
  # 
  # 
  # stats$date <- as.Date(stats$date, format= "%Y-%m-%d")
  # stats$date_end <- as.Date(stats$date_end, format= "%Y-%m-%d")
  # 
  # 
  # info_sites <- info_sites %>%
  #   filter(data_table == "measurement", is.na(date_ended)) %>%
  #   distinct(site_name, .keep_all = TRUE) %>%
  #   select(site_name,
  #          longitude,
  #          latitude,
  #          site_type,
  #          database_name)
  #
  #  stats <-  filter(stats, date == day)
    
  ##### run code below only if you are connected at Ricardo's servers ##############
  #####-----------------------------------------------------------------------------
  ##################################################################################
           
    withProgress(message = "processing.....",  detail = 'this may take a while...', value = 0.25, { background= "blue"
    # Number of times we'll go through the loop
    n <- 100

    if (input$goButton == 0)  # the start position of the button is 0 (OFF)
      return(NULL)

    info_sites <- search_database(input$database, "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
    # remove empty lat, lon
    info_sites <- info_sites[!is.na(info_sites$lat),]
    # remove lat=0 lon=0
    info_sites <- info_sites[info_sites$lat!= 0,]
    info_sites <- subset(info_sites, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

    site_vector <- unique(info_sites$site)

    info_sites <- info_sites %>%
      filter(data_table == "measurement", is.na(date_ended)) %>%
      distinct(site_name, .keep_all = TRUE) %>%
      select(site_name,
             longitude,
             latitude,
             site_type,
             database_name)

    stats <- import_stats(input$database,site = site_vector,
                               variable = input$variable,
                               # start = "2016-08-15",
                               start = format(input$start),
                               # end = "2016-03-31",
                               end = format(input$end),
                               statistic = "daily_mean",
                               extra = TRUE) 
   
    Sys.sleep(1)
     
  #####---------------------------------------------------------------------
  ##########################################################################
  
  stats <- stats %>%
            select(date,
             date_end,
             site,
             site_name,
             variable,
             value,
             network_id)
    
            
     
    
    ## join data + lat, lon ---------------------------------------------------------
    ## and select a variable (NO2, O3, pm25, pm20 etc.) 
    stats <- stats %>%
      left_join(info_sites, "site_name") %>%
      filter(variable == input$variable)
    
    # remove empty rows
    stats <- stats[!is.na(stats$value),]
    
    write.csv(stats, "all_stats.csv")     
    

    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("gathering data....."))
    # Pause for 0.5 seconds to simulate a long computation.
    Sys.sleep(0.5)
    
    
  })
  
})
    


## tables with sectors & subsectors  data---------------------------------------------


output$stats <- renderTable({
  if(input$goButton == 0)  # when the button is not clicked
    return(NULL)
read.csv("all_stats.csv")
})




## Map------------------------------------------------------------------------------------------   

finalMap <- reactive({

   if(input$goButton == 0)
     return(NULL)
   stats <- read.csv("all_stats.csv")  
  
  popup_NO2 <- paste0("<strong><i>",
                      stats$site_name,
                      "</i></strong><br>Daily mean NO<sub>2</sub>: <strong> ", round(stats$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  
  
  map <- leaflet(stats) %>%
    addTiles() %>%
    # setView(lng = -2, lat = 53.5, zoom = 6) %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Thunderforest.Transport", group = "Thunderforest") %>%
    addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
    addProviderTiles("Hydda.Base", group = "Hydda_Base") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    addCircleMarkers(
      lng = ~ longitude, lat = ~ latitude,
      popup = ~popup_NO2,
      weight = 3, radius = 10,
      group = "variable"
    ) %>%
  
  addLayersControl(
    # baseGroups = background,
    baseGroups = c("Road map", "Hydda_Full", "Toner Lite","Thunderforest", "Hydda_Base", "Satellite"),
    overlayGroups = c("variable"),
    options = layersControlOptions(collapsed = TRUE)
  )
  
  
  map
  
})
  

# Return to client
output$myMap = renderLeaflet(finalMap())


observeEvent(input$refresh, {
  js$refresh();
})

})

    
