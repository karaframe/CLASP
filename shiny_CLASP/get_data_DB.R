
# library(importr)
# library(dplyr)
# library(threadr)
# library(dygraphs)
# library(tidyr)
# library(devtools)
# # devtools::install_github("skgrange/threadr")
# 
# # example for site Chilbolton supersite
# 
# setwd("C:/RICARDO-AEA/hydrocarbons_plotting/HC_FK_Markdown")
# month_past <- 6
# quarter <- NA
# site_code <- "chbo" # Chilbolton supersite
# site <- "chbo"

get_measurement_time_series <- function(site_code, month_past = 6, quarter = NA) {

# data_site_info <- search_database("archive", extra = TRUE) %>% 
#   filter(site_friendly == site_code,
#          network_id == "hc" | variable_friendly %in% c("co", "nox"),
#          is.na(date_ended) | date_ended == ymd("2015-12-31"),
#          network_id != "marga")

# write.csv(data_site_info, "data_site.csv")
data_site_info <- read.csv("data_site.csv")[-1]

# Get start and end dates
if (!is.na(quarter)) {
  
  # Parse
  date_quarter <- parse_date_time(quarter, c("ymd", "dmy"))
  
  # Start date
  date_start <- floor_date(date_quarter, "quarter")
  # Preceding 2 weeks
  date_start <- date_start - weeks(2)
  
  # End date, addition is to ensure the quarter has been entered
  date_end <- ceiling_date(date_quarter + 1, "quarter")
  
  # Following two weeks too
  date_end <- date_end + weeks(2)
  
} else {
  
  # Get date range for query
  date_end <- Sys.Date()
  # n months in the past
  date_start <- date_end - months(month_past)
  
}

# To character for function
date_end <- as.character(date_end)
date_start <- as.character(date_start)


# Import hourly data
# data_hc <- import_measures("archive", site_network_id = data_site_info$site_network_id,
#                            start = date_start, end = date_end) %>% 
#   dplyr:: select(date,
#          site,
#          site_name,
#          variable,
#          value)

# write.csv(data_hc, "data_hc.csv")
data_hc <- read.csv("data_hc.csv")[-1]

data_hc <- data_hc %>%
  mutate(date = ymd_hms(date, tz = "UTC"))
                                

data_hc <- na.omit(data_hc)  # remove rows with NA values

# data_hc$row <- 1:nrow(data_hc)
data_hc <- data_hc %>%
  spread(variable, value)

# Build timeseries for plots
time_series <- data_frame_to_timeseries(data_hc)

# Return
time_series

}

# ts <- time_series$nox
# plot <- dygraph(time_series$nox, group = site)


############################################################################
############################################################################


# to create grouped interactive dygraphs
interactive_plot <- function(ts, type, group) {
  
  if (!is.null(ts)) {
    
    # Get colour vector
    colour_vector <- threadr::ggplot2_colours(45)
    
    if (type == "nox") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[1]) %>% 
        dySeries(label = "NOx") %>% 
        dyAxis("y", label = "Hourly NO<sub>x</sub> (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "co") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[2]) %>% 
        dySeries(label = "CO") %>% 
        dyAxis("y", label = "Hourly CO (mg m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "ethane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[12]) %>% 
        dySeries(label = "Ethane") %>% 
        dyAxis("y", label = "Hourly ethane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "ethene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[3]) %>% 
        dySeries(label = "Ethene") %>% 
        dyAxis("y", label = "Hourly Ethene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "propane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[4]) %>% 
        dySeries(label = "Propane") %>% 
        dyAxis("y", label = "Hourly Propane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "propene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[4]) %>% 
        dySeries(label = "Propene") %>% 
        dyAxis("y", label = "Hourly Propene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    # Extras
    if (type == "ibutane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[5]) %>% 
        dySeries(label = "iso-butane") %>% 
        dyAxis("y", label = "Hourly iso-butane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    # Here
    if (type == "nbutane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[6]) %>% 
        dySeries(label = "n-butane") %>% 
        dyAxis("y", label = "Hourly n-butane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "ethyne") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[7]) %>% 
        dySeries(label = "ethyne") %>% 
        dyAxis("y", label = "Hourly ethyne (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "1butene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[8]) %>% 
        dySeries(label = "1-Butene") %>% 
        dyAxis("y", label = "Hourly 1-Butene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "t2butene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[9]) %>% 
        dySeries(label = "trans-2-butene") %>% 
        dyAxis("y", label = "Hourly trans-2-butene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "c2butene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[10]) %>% 
        dySeries(label = "cis-2-butene") %>% 
        dyAxis("y", label = "Hourly cis-2-butene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "ipentane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[11]) %>% 
        dySeries(label = "iso-pentane") %>% 
        dyAxis("y", label = "Hourly iso-pentane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "npentane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[11]) %>% 
        dySeries(label = "n-pentane") %>% 
        dyAxis("y", label = "Hourly n-pentane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "13bdiene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[12]) %>% 
        dySeries(label = "1,3-butadiene") %>% 
        dyAxis("y", label = "Hourly 1,3-butadiene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "t2penten") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[13]) %>% 
        dySeries(label = "trans-2-pentene") %>% 
        dyAxis("y", label = "Hourly trans-2-pentene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "1penten") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[14]) %>% 
        dySeries(label = "1-pentene") %>% 
        dyAxis("y", label = "Hourly 1-pentene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "2mepent") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[15]) %>% 
        dySeries(label = "2-methylpentane") %>% 
        dyAxis("y", label = "Hourly 2-methylpentane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "isoprene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[16]) %>% 
        dySeries(label = "isoprene") %>% 
        dyAxis("y", label = "Hourly isoprene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "nhexane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[17]) %>% 
        dySeries(label = "n-hexane") %>% 
        dyAxis("y", label = "Hourly n-hexane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "nheptane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[18]) %>% 
        dySeries(label = "n-heptane") %>% 
        dyAxis("y", label = "Hourly n-heptane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "ioctane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[19]) %>% 
        dySeries(label = "iso-octane") %>% 
        dyAxis("y", label = "Hourly iso-octane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "noctane") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[20]) %>% 
        dySeries(label = "n-octane") %>% 
        dyAxis("y", label = "Hourly n-octane (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "benzene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[21]) %>% 
        dySeries(label = "benzene") %>% 
        dyAxis("y", label = "Hourly benzene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "toluene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[22]) %>% 
        dySeries(label = "toluene") %>% 
        dyAxis("y", label = "Hourly toluene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "ethbenz") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[23]) %>% 
        dySeries(label = "ethylbenzene") %>% 
        dyAxis("y", label = "Hourly ethylbenzene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "mpxylene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[24]) %>% 
        dySeries(label = "m+p-xylene") %>% 
        dyAxis("y", label = "Hourly m+p-xylene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "oxylene") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[25]) %>% 
        dySeries(label = "oxylene") %>% 
        dyAxis("y", label = "Hourly oxylene (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "135tmb") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[26]) %>% 
        dySeries(label = "1,3,5-TMB") %>% 
        dyAxis("y", label = "Hourly 1,3,5-TMB (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "124tmb") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[26]) %>% 
        dySeries(label = "1,2,4-TMB") %>% 
        dyAxis("y", label = "Hourly 1,2,4-TMB (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    if (type == "123tmb") {
      
      plot <- dygraph(ts, group = group) %>% 
        dyOptions(colors = colour_vector[26]) %>% 
        dySeries(label = "1,2,3-TMB") %>% 
        dyAxis("y", label = "Hourly 1,2,3-TMB (&#956;g m<sup>-3</sup>)") %>% 
        dyRangeSelector()
      
    }
    
    # Return
    plot
    
  }
  
}


interactive_map <- function(df) {
  
  # Map
  map <- leaflet() %>% 
    addTiles(group = "OpenStreetMap") %>% 
    addProviderTiles("Stamen.Toner", group = "Toner") %>% 
    addProviderTiles("Esri.WorldImagery", group = "Images") %>% 
    addCircleMarkers(data = df, lng = ~ longitude, lat = ~ latitude, 
                     color = "tomato", fillOpacity = 0.8, 
                     popup = ~ site_name, group = "Sites") %>% 
    addLayersControl(baseGroups = c( "OpenStreetMap", "Toner", "Images"),
                     overlayGroups = "Sites")
  
  # Return
  map
  
}

