
library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(sp)
library(shinydashboard)
library(importr)
library(lubridate)
library(dygraphs)
library(threadr)
library(tidyr)
 
# source("helpers.R")
# source("import_stats.R")
# source("import_measures.R")

# setwd("C:/ED62769_CLASP_STFC/shiny_CLASP")

# select a specifc day
# day <- "2016-08-15"
# start <- "2016-08-15"
# end <- "2016-08-15"

# Sys.setenv(https_proxy="https://harproxy02:3128")

shinyServer(function(input, output, session) {
  
  
output$contents <- DT::renderDataTable(DT::datatable({ 
 
  
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
  # data_site_info <- read.csv(paste("data_site_info_",input$database,".csv", sep = ""))
  # data_time <- read.csv(paste("data_time_",input$database, ".csv", sep = ""))
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
    
     
  #  write.csv(stats, "all_stats.csv")     
    

    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("gathering data....."))
    # Pause for 0.5 seconds to simulate a long computation.
    Sys.sleep(0.5)
    
    
  })
  
  table <- stats
  

    
# table <- read.csv("all_stats.csv")

## tables with sectors & subsectors  data---------------------------------------------

# output$stats <- DT::renderDataTable(DT::datatable({
 # if(input$goButton == 0)  # when the button is not clicked
  #  return(NULL)
 # table <- read.csv("all_stats.csv")
   
   
 observe({
   if(input$goButton == 0)  # when the button is not clicked
   return(NULL)
   updateSelectInput(session, "site", "Site:",
                     choices = c("All", as.character(table$site_name)))

 })
   
 output$statss <- DT::renderDataTable(DT::datatable({
 if(input$goButton == 0)  # when the button is not clicked
  return(NULL)
 table2 <- table[table$site_name == as.character(input$site),]   
 
}))
 
 
 output$stats_all <- DT::renderDataTable(DT::datatable({
   if(input$goButton == 0)  # when the button is not clicked
     return(NULL)
   table
 }))
 


## Map------------------------------------------------------------------------------------------   

finalMap <- reactive({

   if(input$goButton == 0)
     return(NULL)
  # stats <- read.csv("all_stats.csv") 
   stats <- table[table$site_name == as.character(input$site),]   
   
  
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
  
 
 finalMap_all <- reactive({
   
   if(input$goButton == 0)
     return(NULL)
   table <- stats 
   stats_all <- table 
   
   popup_NO2_all <- paste0("<strong><i>",
                           stats_all$site_name,
                           "</i></strong><br>Daily mean NO<sub>2</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
   
   map_all <- leaflet(stats_all) %>%
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
       popup = ~popup_NO2_all,
       weight = 3, radius = 10,
       group = "variable"
     ) %>%
     
     addLayersControl(
       # baseGroups = background,
       baseGroups = c("Road map", "Hydda_Full", "Toner Lite","Thunderforest", "Hydda_Base", "Satellite"),
       overlayGroups = c("variable"),
       options = layersControlOptions(collapsed = TRUE)
     )
   
   
   map_all
   
 })

 ## create interactive plots with dygraph
 
 output$annual_dygraphs <- renderDygraph({ 
   
   # data_site_info <- read.csv(paste("data_site_info_",input$database,".csv", sep = ""))
   # data_time <- read.csv(paste("data_time_",input$database, ".csv", sep = ""))
   
   data_site_info <- search_database(input$database, extra = TRUE) %>%
     filter(site_name == as.character(input$site),
            variable_friendly %in% c("no2", "o3"),
            is.na(date_ended) | date_ended == ymd("2015-12-31"))
   
   # data_site_info <- search_database("kent",extra = TRUE) %>%
   #   filter(site_name == "Canterbury",
   #          variable_friendly %in% c("no2", "o3"),
   #          is.na(date_ended) | date_ended == ymd("2015-12-31"))
 
   
 # Import hourly data
 data_time <- import_measures(input$database, site_network_id = data_site_info$site_network_id,
                            start = format(input$start), end = format(input$end)) %>% 
   
   # data_time <- import_measures("kent", site_network_id = data_site_info$site_network_id,
   #                              start = "2016-08-15", end = "2016-08-19")%>% 

   select(date,
        site,
        site_name,
        variable,
        value) 
 
 # remove line with date == NA
 data_time <- data_time[!is.na(data_time$date),] 
 
 data_time <- data_time %>%
   spread(variable, value)
 
 # Build timeseries for plots
  time_series <- data_frame_to_timeseries(data_time)
 
 # plot <- dygraph(time_series$no2)
 
         # Get colour vector
      colour_vector <- threadr::ggplot2_colours(45)
      
      if (input$variable == "no2") {
        
        plot <- dygraph(time_series$no2) %>% 
          dyOptions(colors = colour_vector[1]) %>% 
          dySeries(label = "NO2") %>% 
          dyAxis("y", label = "Daily NO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>% 
          dyRangeSelector()
      }
      
      if (input$variable == "o3") {

        plot <- dygraph(time_series$o3) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "O3") %>%
          dyAxis("y", label = "Daily O<sub>3</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      # Return
      plot
      
 })
 

 
# Return to client
output$myMap = renderLeaflet(finalMap())

# Return to client
output$myMap_all = renderLeaflet(finalMap_all())


}))   ### this has been closed from the opening at ***

observeEvent(input$refresh, {
  js$refresh();
})

})

    
