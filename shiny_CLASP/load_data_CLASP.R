
library(dplyr)
library(tidyr)
library(lubridate)
library(devtools)
# library(importr)
library(leaflet)
# library(threadr)


setwd("C:/ED62769_CLASP_STFC/shiny_CLASP")

# select a specifc day
# day <- "2016-08-15"

# Helpers
# print_database_names()
# print_statistic_types()

source("helpers.R")
source("import_stats.R")
source("import_measures.R")

## AURN---------------------------------------------------------------------------------------

info_sites_archive <- search_database("archive", "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
info_sites_archive <- subset(info_sites_archive, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

write.csv(info_sites_archive, "info_sites_archive.csv")
info_sites_archive <- read.csv("info_sites_archive.csv")[-1]

site_vector_archive <- unique(info_sites_archive$site)

info_sites_archive <- info_sites_archive %>%
  filter(data_table == "measurement", is.na(date_ended)) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  select(site_name,
         longitude,
         latitude,
         site_type,
         database_name)

data_site_info_archive <- search_database("archive", extra = TRUE)

stats_archive <- import_stats("archive",site = site_vector_archive, 
                           variable = c("o3", "no2", "co", "pm10", "pm25", "at10", "at25"),
                           start = "2016-09-23",
                           # start = date_start,
                           # end = "2016-03-31",
                           statistic = "daily_mean",
                           extra = TRUE)

data_time_archive <- import_measures("archive", site_network_id = data_site_info_archive$site_network_id,
                                     variable = c("o3", "no2"),
                                     start = "2016-09-23")

write.csv(stats_archive, "stats_archive.csv")
stats_archive <- read.csv("stats_archive.csv")[-1]

write.csv(data_site_info_archive, "data_site_info_archive.csv")
write.csv(data_time_archive, "data_time_archive.csv")


stats_archive$date <- as.Date(stats_archive$date, format= "%Y-%m-%d")
stats_archive$date_end <- as.Date(stats_archive$date_end, format= "%Y-%m-%d")


# stats <-  subset(stats, date == day)
stats_archive <-  filter(stats_archive, date == day) %>%
  select(date,
         date_end,
         site,
         site_name,
         variable,
         value,
         network_id)


# join data + lat, lon
stats_archive <- stats_archive %>%
  left_join(info_sites_archive, "site_name")

## AQ England-----------------------------------------------------------------------------------

info_sites_aqengland <- search_database("aqengland", "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
info_sites_aqengland <- subset(info_sites_aqengland, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

write.csv(info_sites_aqengland, "info_sites_aqengland.csv")
site_vector_aqengland <- unique(info_sites_aqengland$site)

info_sites_aqengland <- info_sites_aqengland %>%
  filter(data_table == "measurement", is.na(date_ended)) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  select(site_name,
         longitude,
         latitude,
         site_type,
         database_name)

data_site_info_aqengland <- search_database("aqengland", extra = TRUE)


stats_aqengland <- import_stats("aqengland",site = site_vector_aqengland, 
                           variable = c("o3", "no2", "co", "pm10", "pm25", "at10", "at25"),
                           start = "2016-09-18",
                           # start = date_start,
                           # end = "2016-03-31",
                           statistic = "daily_mean",
                           extra = TRUE)

data_time_archive <- import_measures("aqengland", site_network_id = data_site_info_aqengland$site_network_id,
                                     start = "2016-09-18")

write.csv(stats_aqengland, "stats_aqengland.csv")

write.csv(data_site_info_aqengland, "data_site_info_aqengland.csv")
write.csv(data_time_aqengland, "data_time_aqengland.csv")

stats_aqengland$date <- as.Date(stats_aqengland$date, format= "%Y-%m-%d")
stats_aqengland$date_end <- as.Date(stats_aqengland$date_end, format= "%Y-%m-%d")

stats_aqengland <-  filter(stats_aqengland, date == day) %>%
  select(date,
         date_end,
         site,
         site_name,
         variable,
         value,
         network_id)


# join data + lat, lon
stats_aqengland <- stats_aqengland %>%
  left_join(info_sites_aqengland, "site_name")


## WALES----------------------------------------------------------------------------------------

info_sites_waq <- search_database("waq", "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
# remove empty lat, lon
info_sites_waq <- info_sites_waq[!is.na(info_sites_waq$lat),] 
# remove lat=0 lon=0
info_sites_waq <- info_sites_waq[info_sites_waq$lat!= 0,]   

info_sites_waq <- subset(info_sites_waq, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

write.csv(info_sites_waq, "info_sites_waq.csv")
site_vector_waq <- unique(info_sites_waq$site)

info_sites_waq <- info_sites_waq %>%
  filter(data_table == "measurement", is.na(date_ended)) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  select(site_name,
         longitude,
         latitude,
         site_type,
         database_name)

data_site_info_waq <- search_database("waq", extra = TRUE)

stats_waq <- import_stats("waq",site = site_vector_waq, 
                                variable = c("o3", "no2", "co", "pm10", "pm25", "at10", "at25"),
                                start = "2016-09-18",
                                # start = date_start,
                                # end = "2016-03-31",
                                statistic = "daily_mean",
                                extra = TRUE)

data_time_archive <- import_measures("waq", site_network_id = data_site_info_waq$site_network_id,
                                     start = "2016-09-18")

write.csv(stats_waq, "stats_waq.csv")
write.csv(data_site_info_waq, "data_site_info_waq.csv")
write.csv(data_time_waq, "data_time_waq.csv")

stats_waq$date <- as.Date(stats_waq$date, format= "%Y-%m-%d")
stats_waq$date_end <- as.Date(stats_waq$date_end, format= "%Y-%m-%d")

stats_waq <-  filter(stats_waq, date == day) %>%
  select(date,
         date_end,
         site,
         site_name,
         variable,
         value,
         network_id)


# join data + lat, lon
stats_waq <- stats_waq %>%
  left_join(info_sites_waq, "site_name")


## KENT--------------------------------------------------------------------------------------

info_sites_kent <- search_database("kent", "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
info_sites_kent <- subset(info_sites_kent, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

write.csv(info_sites_kent, "info_sites_kent.csv")
site_vector_kent <- unique(info_sites_kent$site)

info_sites_kent <- info_sites_kent %>%
  filter(data_table == "measurement", is.na(date_ended)) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  select(site_name,
         longitude,
         latitude,
         site_type,
         database_name)

data_site_info_kent <- search_database("kent", extra = TRUE)

stats_kent <- import_stats("kent",site = site_vector_kent, 
                            variable = c("o3", "no2", "co", "pm10", "pm25", "at10", "at25"),
                            start = "2016-09-18",
                            # start = date_start,
                            # end = "2016-03-31",
                            statistic = "daily_mean",
                            extra = TRUE)

data_time_kent <- import_measures("kent", site_network_id = data_site_info_kent$site_network_id,
                                     start = "2016-09-18")

write.csv(stats_kent, "stats_kent.csv")
write.csv(data_site_info_kent, "data_site_info_kent.csv")
write.csv(data_time_kent, "data_time_kent.csv")

stats_kent$date <- as.Date(stats_kent$date, format= "%Y-%m-%d")
stats_kent$date_end <- as.Date(stats_kent$date_end, format= "%Y-%m-%d")

stats_kent <-  filter(stats_kent, date == day) %>%
  select(date,
         date_end,
         site,
         site_name,
         variable,
         value,
         network_id)


# join data + lat, lon
stats_kent <- stats_kent %>%
  left_join(info_sites_kent, "site_name")

## Northern IRELAND---------------------------------------------------------------------------------

info_sites_niarc <- search_database("niarc", "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
info_sites_niarc <- subset(info_sites_niarc, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

write.csv(info_sites_niarc, "info_sites_niarc.csv")
site_vector_niarc <- unique(info_sites_niarc$site)

info_sites_niarc <- info_sites_niarc %>%
  filter(data_table == "measurement", is.na(date_ended)) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  select(site_name,
         longitude,
         latitude,
         site_type,
         database_name)

data_site_info_niarc <- search_database("niarc", extra = TRUE)

stats_niarc <- import_stats("niarc",site = site_vector_niarc, 
                           variable = c("o3", "no2", "co", "pm10", "pm25", "at10", "at25"),
                           start = "2016-09-18",
                           # start = date_start,
                           # end = "2016-03-31",
                           statistic = "daily_mean",
                           extra = TRUE)

data_time_niarc <- import_measures("niarc", site_network_id = data_site_info_niarc$site_network_id,
                                  start = "2016-09-18")

write.csv(stats_niarc, "stats_niarc.csv")
write.csv(data_site_info_niarc, "data_site_info_niarc.csv")
write.csv(data_time_niarc, "data_time_niarc.csv")

stats_niarc$date <- as.Date(stats_niarc$date, format= "%Y-%m-%d")
stats_niarc$date_end <- as.Date(stats_niarc$date_end, format= "%Y-%m-%d")

stats_niarc <-  filter(stats_niarc, date == day) %>%
  select(date,
         date_end,
         site,
         site_name,
         variable,
         value,
         network_id)


# join data + lat, lon
stats_niarc <- stats_niarc %>%
  left_join(info_sites_niarc, "site_name")


## North LINCS-----------------------------------------------------------------

info_sites_nlincs <- search_database("nlincs", "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
info_sites_nlincs <- subset(info_sites_nlincs, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

write.csv(info_sites_nlincs, "info_sites_nlincs.csv")
site_vector_nlincs <- unique(info_sites_nlincs$site)

info_sites_nlincs <- info_sites_nlincs %>%
  filter(data_table == "measurement", is.na(date_ended)) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  select(site_name,
         longitude,
         latitude,
         site_type,
         database_name)

data_site_info_nlincs <- search_database("nlincs", extra = TRUE)


stats_nlincs <- import_stats("nlincs",site = site_vector_nlincs, 
                            variable = c("o3", "no2", "co", "pm10", "pm25", "at10", "at25"),
                            start = "2016-09-18",
                            # start = date_start,
                            # end = "2016-03-31",
                            statistic = "daily_mean",
                            extra = TRUE)

data_time_nlincs <- import_measures("nlincs", site_network_id = data_site_info_nlincs$site_network_id,
                                   start = "2016-09-18")

write.csv(stats_nlincs, "stats_nlincs.csv")
write.csv(data_site_info_nlincs, "data_site_info_nlincsc.csv")
write.csv(data_time_nlincs, "data_time_nlincs.csv")

stats_nlincs$date <- as.Date(stats_nlincs$date, format= "%Y-%m-%d")
stats_nlincs$date_end <- as.Date(stats_nlincs$date_end, format= "%Y-%m-%d")

stats_nlincs <-  filter(stats_nlincs, date == day) %>%
  select(date,
         date_end,
         site,
         site_name,
         variable,
         value,
         network_id)


# join data + lat, lon
stats_nlincs <- stats_nlincs %>%
  left_join(info_sites_nlincs, "site_name")


## Scottish air------------------------------------------------------------------------

info_sites_scotarc <- search_database("scotarc", "o3|no2|co|pm10|pm25|at10|at25") #agreed with RAL-Space
info_sites_scotarc <- subset(info_sites_scotarc, variable %in% c("o3", "no2", "co", "pm10", "pm25", "at10", "at25")) ### metadata

write.csv(info_sites_scotarc, "info_sites_scotarc.csv")
site_vector_scotarc <- unique(info_sites_scotarc$site)

info_sites_scotarc <- info_sites_scotarc %>%
  filter(data_table == "measurement", is.na(date_ended)) %>%
  distinct(site_name, .keep_all = TRUE) %>%
  select(site_name,
         longitude,
         latitude,
         site_type,
         database_name)

data_site_info_scotarc <- search_database("scotarc", extra = TRUE)


stats_scotarc <- import_stats("scotarc",site = site_vector_scotarc, 
                             variable = c("o3", "no2", "co", "pm10", "pm25", "at10", "at25"),
                             start = "2016-09-18",
                             # start = date_start,
                             # end = "2016-03-31",
                             statistic = "daily_mean",
                             extra = TRUE)

data_time_scotarcs <- import_measures("scotarc", site_network_id = data_site_info_scotarc$site_network_id,
                                    start = "2016-09-18")

write.csv(stats_scotarc, "stats_scotarc.csv")
write.csv(data_site_info_scotarc, "data_site_info_scotarc.csv")
write.csv(data_time_scotarc, "data_time_scotarc.csv")

stats_scotarc$date <- as.Date(stats_scotarc$date, format= "%Y-%m-%d")
stats_scotarc$date_end <- as.Date(stats_scotarc$date_end, format= "%Y-%m-%d")


stats_scotarc <-  filter(stats_scotarc, date == day) %>%
  select(date,
         date_end,
         site,
         site_name,
         variable,
         value,
         network_id)


# join data + lat, lon
stats_scotarc <- stats_scotarc %>%
  left_join(info_sites_scotarc, "site_name")


       
date_end <- Sys.Date()

# To character for function
# date_start <- as.character(date_start)
# date_end <- as.character(date_end)




########################################################################################################

# bind all data together

stats <- rbind(stats_AURN,
               stats_AQENGLAND,
               stats_WALES,
               stats_NLINCS,
               stats_KENT,
               stats_NIARC,
               stats_SCOTARC)


# filter data by variable-----------------------

stats_no2 <- stats %>%
  filter(variable =="no2")

# remove empty rows  
stats_no2 <- stats_no2[!is.na(stats_no2$value),] 

# make a elaflet map

popup_NO2 <- paste0("<strong><i>", 
                    stats_no2$site_name,
                    "</i></strong><br>Daily mean NO<sub>2</sub>: <strong> ", round(stats_no2$value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")


map <- leaflet(data = stats_no2[,]) %>% 
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~ longitude, lat = ~ latitude,
    popup = ~popup_NO2,
    weight = 3, radius = 10,
    group = "variable"
  ) 
map



