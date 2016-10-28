
# library(shiny)
library(rgdal)
library(raster)
library(dplyr)
library(leaflet)
# library(RColorBrewer)
# library(sp)
library(shinydashboard)
library(importr)
library(lubridate)
library(dygraphs)
library(threadr)
library(tidyr)
library(DT)
 
# source("helpers.R")
# source("import_stats.R")
# source("import_measures.R")

options(warn=-1)

# setwd("C:/ED62769_CLASP_STFC/shiny_CLASP")

# select a specifc day
# day <- "2016-08-15"
# start <- "2016-08-15"
# end <- "2016-08-15"

# Sys.setenv(https_proxy="https://harproxy02:3128")


shinyServer(function(input, output, session) {
  
output$contents <- DT::renderDataTable(DT::datatable({      ##### opening at ***
 
  
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
  # stats$date <- as.Date(stats$date, format= "%Y-%m-%d")
  # stats$date_end <- as.Date(stats$date_end, format= "%Y-%m-%d")
  # 
  # info_sites <- info_sites %>%
  #   filter(data_table == "measurement", is.na(date_ended)) %>%
  #   distinct(site_name, .keep_all = TRUE) %>%
  #   select(site_name,
  #          longitude,
  #          latitude,
  #          site_type,
  #          database_name)

  
    
  ##### run code below only if you are connected at Ricardo's servers ##############
  #####-----------------------------------------------------------------------------
  ##################################################################################
           
    withProgress(message = "processing.....",  detail = 'this may take a while...
                 ..if no output is showed, it means there is no data', value = 0.25, { background= "blue"
    # Number of times we'll go through the loop
    n <- 100

    if (input$goButton == 0)  # the start position of the button is 0 (OFF)
      return(NULL)

    DB <- input$database

    if (DB == "archive") {
    info_sites <- search_database("archive", "o3|no2|ge10|gr10|pm25|gr25|at10|at25|v10|v25") 
    }
    
    
    if (DB == "Waq") {
      info_sites <- search_database("Waq", "o3|no2|ge10|gr10|pm25|gr25|at10|at25|v10|v25") 
    }
    
    
    if (DB == "aqengland") {
      info_sites <- search_database("aqengland", "o3|no2|ge10|gr10|pm25|gr25|at10|at25|v10|v25") 
    }
    
    
    if (DB == "kent") {
      info_sites <- search_database("kent", "o3|no2|ge10|gr10|pm25|gr25|at10|at25|v10|v25") 
    }
    
    
    if (DB == "niarc") {
      info_sites <- search_database("niarc", "o3|no2|ge10|gr10|pm25|gr25|at10|at25|v10|v25") 
    }
    
    if (DB == "nlincs") {
      info_sites <- search_database("nlincs", "o3|no2|ge10|gr10|pm25|gr25|at10|at25|v10|v25") 
    }
    
    if (DB == "scotarc") {
      info_sites <- search_database("scotarc", "o3|no2|ge10|gr10|pm25|gr25|at10|at25|v10|v25") 
    }
    
    
   # info_sites <- search_database(input$database, "o3|no2|pm10|pm25|at10|at25|v10|v25") 
  
    
    # remove empty lat, lon
    info_sites <- info_sites[!is.na(info_sites$lat),]
    # remove lat=0 lon=0
    info_sites <- info_sites[info_sites$lat!= 0,]
    info_sites <- subset(info_sites, variable %in% c("o3", "no2","ge10", "gr10", "pm25", "gr25",
                                                     "at10", "at25","v10", "v25")) 

    site_vector <- unique(info_sites$site)

    info_sites <- info_sites %>%
      filter(data_table == "measurement", is.na(date_ended)) %>%
      distinct(site_name, .keep_all = TRUE) %>%
      dplyr::select(site_name,
             longitude,
             latitude,
             site_type,
             database_name)
    
    
    if (DB == "archive") {
    stats <- import_stats("archive", site = input$site_archive,
    # stats <- import_stats(input$database, site = input$site,
                               variable = input$variable,
                              #  start = "2016-09-23",
                               start = format(input$start),
                              #  end = "2016-09-24",
                               end = format(input$end),
                               statistic = "daily_mean",
                               extra = TRUE) %>%
           group_by(date) %>% 
           mutate(z_score = scale(value),
           z_score = as.numeric(z_score),
           z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
    
    }
    
    
    if (DB == "Waq") {
      stats <- import_stats("Waq", site = input$site_Waq,
                            variable = input$variable,
                            start = format(input$start),
                            end = format(input$end),
                            statistic = "daily_mean",
                            extra = TRUE) %>%
        group_by(date) %>% 
        mutate(z_score = scale(value),
               z_score = as.numeric(z_score),
               z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
        data_db_failures <- stats %>%
          filter(!z_score_fail)
    }
    
    
    if (DB == "aqengland") {
      stats <- import_stats("aqengland", site = input$site_aqengland,
                            variable = input$variable,
                            start = format(input$start),
                            end = format(input$end),
                            statistic = "daily_mean",
                            extra = TRUE) %>%
        group_by(date) %>% 
        mutate(z_score = scale(value),
               z_score = as.numeric(z_score),
               z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
    }
    
    
    if (DB == "kent") {
      stats <- import_stats("kent", site = input$site_kent,
                            variable = input$variable,
                            start = format(input$start),
                            end = format(input$end),
                            statistic = "daily_mean",
                            extra = TRUE) %>%
        group_by(date) %>% 
        mutate(z_score = scale(value),
               z_score = as.numeric(z_score),
               z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
    }
    
    
    if (DB == "niarc") {
      stats <- import_stats("niarc", site = input$site_niarc,
                            variable = input$variable,
                            start = format(input$start),
                            end = format(input$end),
                            statistic = "daily_mean",
                            extra = TRUE) %>%
        group_by(date) %>% 
        mutate(z_score = scale(value),
               z_score = as.numeric(z_score),
               z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
    }
    
    
    
    if (DB == "nlincs") {
      stats <- import_stats("nlincs", site = input$site_nlincs,
                            variable = input$variable,
                            start = format(input$start),
                            end = format(input$end),
                            statistic = "daily_mean",
                            extra = TRUE) %>%
        group_by(date) %>% 
        mutate(z_score = scale(value),
               z_score = as.numeric(z_score),
               z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
    }
    
    
    if (DB == "scotarc") {
      stats <- import_stats("scotarc", site = input$site_scotarc,
                            variable = input$variable,
                            start = format(input$start),
                            end = format(input$end),
                            statistic = "daily_mean",
                            extra = TRUE) %>%
        group_by(date) %>% 
        mutate(z_score = scale(value),
               z_score = as.numeric(z_score),
               z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
    }
      
  
    Sys.sleep(1)
   
    
  #####---------------------------------------------------------------------
  ##########################################################################
  
  stats <- stats %>%
            dplyr::select(date,
             date_end,
             site,
             site_name,
             variable,
             value,
             network_id,
             z_score,
             z_score_fail)
    
            
     
    
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
  
  
# table <- read.csv("all_stats.csv")
 table <- stats 

## tables with sectors & subsectors  data---------------------------------------------

# observe({
#   if(input$goButton == 0)  # when the button is not clicked
#     return(NULL)
#   updateSelectInput(session, "site", "Site:",
#                     choices = c("All", as.character(table$site_name)))
#  })



output$statss <- DT::renderDataTable(DT::datatable({
  if(input$goButton == 0)  # when the button is not clicked
  return(NULL)
  
  if (DB == "archive") {
     table <- table[table$site_name == as.character(input$site_archive),]
  }
  
  if (DB == "Waq") {
    table <- table[table$site_name == as.character(input$site_waq),]
  }
  
  if (DB == "aqengland") {
    table <- table[table$site_name == as.character(input$site_aqengland),]
  }
  
  if (DB == "kent") {
    table <- table[table$site_name == as.character(input$site_kent),]
  }

  if (DB == "niarc") {
    table <- table[table$site_name == as.character(input$site_niarc),]
  }
  
  if (DB == "nlincs") {
    table <- table[table$site_name == as.character(input$site_nlincs),]
  }
  
  if (DB == "scotarc") {
    table <- table[table$site_name == as.character(input$site_scotarc),]
  }
    
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
  
  if (DB == "archive") {
  stats <- table[table$site_name == as.character(input$site_archive),]   
  }
  
  if (DB == "Waq") {
    stats <- table[table$site_name == as.character(input$site_Waq),]   
  }
  
  if (DB == "aqengland") {
    stats <- table[table$site_name == as.character(input$site_aqengland),]   
  }
  
  if (DB == "kent") {
    stats <- table[table$site_name == as.character(input$site_kent),]   
  }
  
  if (DB == "niarc") {
    stats <- table[table$site_name == as.character(input$site_niarc),]   
  }
  
  if (DB == "nlincs") {
    stats <- table[table$site_name == as.character(input$site_nlincs),]   
  }
  
  if (DB == "scotarc") {
    stats <- table[table$site_name == as.character(input$site_scotarc),]   
  }
  
  
  
if (input$variable == "no2") {
  popup <- paste0("<strong><i>",
                      stats$site_name,
                      "</i></strong><br>Daily mean NO<sub>2</sub>: <strong> ", round(stats$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
}
  
  
  if (input$variable == "o3") {
    popup <- paste0("<strong><i>",
                        stats$site_name,
                        "</i></strong><br>Daily mean O<sub>3</sub>: <strong> ", round(stats$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  if (input$variable == "pm10") {
    popup <- paste0("<strong><i>",
                    stats$site_name,
                    "</i></strong><br>Daily mean PM<sub>10</sub>: <strong> ", round(stats$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  if (input$variable == "pm25") {
    popup <- paste0("<strong><i>",
                    stats$site_name,
                    "</i></strong><br>Daily mean PM<sub>2.5</sub>: <strong> ", round(stats$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }

  
  if (input$variable == "v10") {
    popup <- paste0("<strong><i>",
                    stats$site_name,
                    "</i></strong><br>Daily mean V<sub>10</sub>: <strong> ", round(stats$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  if (input$variable == "v25") {
    popup <- paste0("<strong><i>",
                    stats$site_name,
                    "</i></strong><br>Daily mean V<sub>2.5</sub>: <strong> ", round(stats$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  
  
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
      popup = ~popup,
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
 
if (input$variable == "no2") {
  popup_all <- paste0("<strong><i>",
                      stats_all$site_name,
                      "</i></strong><br>Last daily mean NO<sub>2</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
}
  
  
  if (input$variable == "o3") {
    popup_all <- paste0("<strong><i>",
                        stats_all$site_name,
                        "</i></strong><br>Last daily mean O<sub>3</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  if (input$variable == "gr10") {
    popup_all <- paste0("<strong><i>",
                        stats_all$site_name,
                        "</i></strong><br>Last daily mean Gravimetric  PM<sub>10</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  if (input$variable == "ge10") {
    popup_all <- paste0("<strong><i>",
                        stats_all$site_name,
                        "</i></strong><br>Last daily mean Grav. Equiv. PM<sub>10</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  if (input$variable == "pm25") {
    popup_all <- paste0("<strong><i>",
                        stats_all$site_name,
                        "</i></strong><br>Last daily mean PM<sub>2.5</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  
  if (input$variable == "gr25") {
    popup_all <- paste0("<strong><i>",
                        stats_all$site_name,
                        "</i></strong><br>Last daily mean Gravimetric PM<sub>2.5</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  
  if (input$variable == "v10") {
    popup_all <- paste0("<strong><i>",
                        stats_all$site_name,
                        "</i></strong><br>Last daily mean V<sub>10</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
  if (input$variable == "v25") {
    popup_all <- paste0("<strong><i>",
                        stats_all$site_name,
                        "</i></strong><br>Last daily mean V<sub>2.5</sub>: <strong> ", round(stats_all$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")
  }
  
  
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
      popup = ~popup_all,
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
 
 output$mean_dygraphs <- renderDygraph({ 
   
   withProgress(message = "processing.....",  detail = 'this may take a while..', value = 0.25, { background= "blue"
                 # Number of times we'll go through the loop
                 n <- 100


   # data_site_info <- read.csv(paste("data_site_info_",input$database,".csv", sep = ""))[-1]
   # data_time <- read.csv(paste("data_time_",input$database, ".csv", sep = ""))[-1]
   

  # data_time <- data_time %>%
  # mutate(date = dmy_hm(date, tz = "UTC")) 
  # 
  # data_time <- data_time %>%
  #   mutate(date_end = dmy_hm(date_end, tz = "UTC")) 
  # 
  # data_site_info$date_start <- as.Date(data_site_info$date_start, format= "%Y-%m-%d")
  # data_site_info$date_ended <- as.Date(data_site_info$date_ended, format= "%Y-%m-%d")
  
  
  
  # data_site_info <- search_database(input$database,
  #                                   site = input$site,
  #                                   variable %in% c("no2", "o3"),
  #                                   extra = TRUE) 
     # filter(site_name == as.character(input$site),

  
  
  # data_site_info <- data_site_info %>%
  #   filter(site_name == as.character(input$site),
  #          variable_friendly %in% c("no2", "o3"),
  #          is.na(date_ended) | date_ended == ymd("2015-12-31"))
  
# Import hourly data
   if (DB == "archive") {
    # data_time <- import_measures(input$database, site = input$site_Waq, variable = input$variable,
     data_time <- import_measures("archive", site = input$site_archive, variable = input$variable,
                           start = format(input$start), end = format(input$end))
   }
   
   if (DB == "Waq") {
     data_time <- import_measures("Waq", site = input$site_Waq, variable = input$variable,
                                  start = format(input$start), end = format(input$end))
   }
   
   
   if (DB == "aqengland") {
     data_time <- import_measures("aqengland", site = input$site_aqengland, variable = input$variable,
                                  start = format(input$start), end = format(input$end))
   }
   
   
   if (DB == "kent") {
     data_time <- import_measures("kent", site = input$site_kent, variable = input$variable,
                                  start = format(input$start), end = format(input$end))
   }
   
   if (DB == "niarc") {
     data_time <- import_measures("niarc", site = input$site_niarc, variable = input$variable,
                                  start = format(input$start), end = format(input$end))
   }
   
   if (DB == "nlincs") {
     data_time <- import_measures("nlincs", site = input$site_nlincs, variable = input$variable,
                                  start = format(input$start), end = format(input$end))
   }
   
   if (DB == "scotarc") {
     data_time <- import_measures("scotarc", site = input$site_scotarc, variable = input$variable,
                                  start = format(input$start), end = format(input$end))
   }
 
                 
    # Increment the progress bar, and update the detail text.
    incProgress(1/n, detail = paste("gathering data....."))
    # Pause for 0.5 seconds to simulate a long computation.
    Sys.sleep(0.5)
                 
   })
   
 # data_time <- import_measures("archive", site = "abd", variable = "no2",
 #                              start = "2016-09-28", "2016-09-30")
 

  # data_time <- data_time %>%
  # filter(site_name == as.character(input$site)) %>%
  #  select(date,
  #       site,
  #       site_name,
  #       variable,
  #       value) 
 
 # remove line with date == NA
 data_time <- data_time[!is.na(data_time$date),]
 
 # data_time <- data_time %>%
 #   spread(variable, value)
 
 
 # Build timeseries for plots
  time_series <- threadr::data_frame_to_timeseries(data_time)

 
 # plot <- dygraph(time_series$no2)
 
         # Get colour vector
      colour_vector <- threadr::ggplot2_colours(45)
    
     
      if (input$variable == "no2") {

        plot <- dygraphs::dygraph(time_series$no2) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "NO2") %>%
          dyAxis("y", label = "Hourly NO<sub>2</sub> (&#956;g m<sup>-3</sup>)") %>%
            dyRangeSelector()
        
      }
      
      if (input$variable == "o3") {

        plot <- dygraphs::dygraph(time_series$o3) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "O3") %>%
          dyAxis("y", label = "Hourly O<sub>3</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      if (input$variable == "gr10") {
        
        plot <- dygraphs::dygraph(time_series$gr10) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "Gravimetric PM10") %>%
          dyAxis("y", label = "Hourly Gravimetric PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      if (input$variable == "ge10") {
        
        plot <- dygraphs::dygraph(time_series$ge10) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "Grav. Equiv. PM10") %>%
          dyAxis("y", label = "Hourly Grav. Equiv. PM<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      if (input$variable == "pm25") {
        
        plot <- dygraphs::dygraph(time_series$pm25) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "PM2.5") %>%
          dyAxis("y", label = "Hourly PM<sub>2.5</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      
      if (input$variable == "gr25") {
        
        plot <- dygraphs::dygraph(time_series$gr25) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "Gravimetric PM25") %>%
          dyAxis("y", label = "Hourly Gravimetric PM<sub>25</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      
      if (input$variable == "v10") {
        
        plot <- dygraphs::dygraph(time_series$v10) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "V10") %>%
          dyAxis("y", label = "Hourly V<sub>10</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      if (input$variable == "v25") {
        
        plot <- dygraphs::dygraph(time_series$v25) %>%
          dyOptions(colors = colour_vector[1]) %>%
          dySeries(label = "v25") %>%
          dyAxis("y", label = "Hourly V<sub>2.5</sub> (&#956;g m<sup>-3</sup>)") %>%
          dyRangeSelector()
      }
      
      
      # Return
      plot
      
 })
 

 
 output$z_score_dygraphs <- renderDygraph({ 

   # Build timeseries for plots
   time_series_z_score <- threadr::data_frame_to_timeseries(stats)
   
   # Get colour vector
   colour_vector <- threadr::ggplot2_colours(45)
 
 plot_z_score <- dygraphs::dygraph(time_series_z_score$z_score) %>%
   dyOptions(colors = colour_vector[1]) %>%
   dySeries(label = "z_score") %>%
   dyAxis("y", label = "Z score") %>%
   dyRangeSelector()
 
 # Return
 plot_z_score
 
 })

 
# Return to client
output$myMap = renderLeaflet(finalMap())


# Return to client
output$myMap_all = renderLeaflet(finalMap_all())

}))     ### this has been closed from the opening at ***

observeEvent(input$refresh, {
  js$refresh();
})

})

    
