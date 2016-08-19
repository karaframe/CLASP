#' Function to search databases' look-up tables for a string. 
#' 
#' \code{search_database} is useful for finding site identifiers, names, and
#' variables. 
#' 
#' @param database Database name to search. Note, this is a name, not a database
#' connection. \code{database} can also take the value \code{all} if the database
#' name is not known. Use \code{print_database_names} to find databases which 
#' can be connected to. 
#' 
#' @param string String/text to search for within a database. \code{string} can
#' be a vector for searching multiple strings and is not case sensitive.
#' 
#' @param extra Should the function return extra data? Default is \code{FALSE}. 
#' 
#' @param site_network_id Search for a specific \code{site_network_id}. This is
#' the indexed key in the databases which relate to a "process" (a site-pollutant)
#' combination. This will override other arguments if used. 
#' 
#' @seealso \code{\link{print_database_names}}, \code{\link{print_statistic_types}},
#' \code{\link{import_stats}}, \code{\link{import_measures}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Search archive for bath
#' search_database(database = "archive", string = "bath")
#' 
#' # Search all databases for bath
#' search_database(database = "all", string = "bath")
#' 
#' # Search all databases for oxford and get more information
#' search_database(database = "all", string = "oxford", extra = TRUE)
#' 
#' }
#' 
#' @export
search_database <- function(database, string = "", extra = FALSE, 
                            site_network_id = NA) {
  
  # Add database name here, in parser, and in tests
  if (database == "all") {
    
    database <- c("archive", "aqengland", "waq", "scotarc", "niarc", "gibraltar",
                  "andorra", "kent", "nlincs", "ndarchive")
    
    
  }
  
  # Parse arguments
  database <- sapply(database, parse_database_name, USE.NAMES = FALSE)
  string <- stringr::str_c(string, collapse = "|")
  
  # Search databases
  df <- plyr::ldply(database, searcher, string = string)
  
  if (!is.na(site_network_id[1])) {
    
    # Filter to site_network_id
    df <- df[df$site_network_id %in% site_network_id, ]
    
  } else {
    
    # Select important variables
    if (!extra) {
      
      # Select most important variables
      df <- subset(df, select = c(site_network_id,
                                  site_friendly, 
                                  site_name, 
                                  variable_friendly, 
                                  date_started, 
                                  date_ended, 
                                  site_type,
                                  latitude, 
                                  longitude, 
                                  data_table, 
                                  database_name))
      
      # Rename friendly variables
      names(df) <- stringr::str_replace(names(df), "_friendly$", "")
      
    }
    
  }
  
  # Arrange by site name
  df <- dplyr::arrange(df, site_name)
  
  # Return
  df
  
}


#' Function to list the database names which can be used. 
#' 
#' @seealso \code{\link{import_stats}}, \code{\link{import_measures}}
#' 
#' @author Stuart K. Grange
#' 
#' @export
print_database_names <- function(extra = FALSE) {
  
  # Get look-up table location
#  file_name <- system.file("extdata/database_names.csv", package = "importr")
  file_name <- "database_names.csv"

  # Load data
  df <- read.csv(file_name, stringsAsFactors = FALSE)
  
  # Select single variable, a vector not data frame
  if (!extra) df <- df$database_name
  
  # Return
  df
  
}


#' Function to list the statistic types which can be used. 
#' 
#' @seealso \code{\link{import_stats}}, \code{\link{import_measures}}
#' 
#' @author Stuart K. Grange
#' 
#' @export
print_statistic_types <- function(extra = FALSE) {
  
  # Load data
  df <- load_statistics_data()
  
  # Select single variable, a vector not data frame
  if (!extra) df <- df$statistic
    
  # Return
  df
  
}


# Function to create a database connection based on a database name
# 
# No export
db_connect_with_name <- function(name) {
  
  # Create a database connection, hornbeam system
  con <- DBI::dbConnect(RMySQL::MySQL(), host = "172.31.4.157", 
                        dbname = name, user = "webuser",  
                        password = "viewonly")
  
  # Return
  con
  
}


# Function to create a look-up statement. No arguments, filtering is done post
# query.
#
# No export
lookup_statement <- function(database) {
  
  # archive has some extras which are useful to have
  # To-do: trim this down a little. It leads to a larger table than nessassary
  if (database == "archive") {
    
    statement <-  "SELECT site_network_id, 
    site_network.site_id AS site,
    parameter_id AS variable,
    site_network.date_started, 
    site_network.date_ended, 
    data_table, 
    site.site_name, 
    site.latitude, 
    site.longitude,
    site.altitude, 
    site_network.uka_site_id,
    site_network.EU_site_id AS eu_site_id,
    site_network.old_EOI_code AS old_eoi_code,
    site_network.network_parameter_id,
    LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id,
    network.network_name,
    LCASE(REPLACE(environment.environment_code, ' ', '_')) AS site_type,
    REPLACE(environment.EU_type, ' ', '_') AS site_type_eu,
    zone_region.region_name AS region,
    agglomeration_region.region_name AS agglomeration
    FROM site_network
    LEFT JOIN network_parameter
    ON site_network.network_parameter_id = network_parameter.network_parameter_id
    LEFT JOIN site
    ON site_network.site_id = site.site_id
    LEFT JOIN network
    ON network.network_id = network_parameter.network_id
    LEFT JOIN environment
    ON site_network.environment_id = environment.environment_id
    LEFT JOIN zone_region
    ON site.zone_region = zone_region.region_id
    LEFT JOIN agglomeration_region
    ON site.agglomeration_region = agglomeration_region.region_id
    WHERE data_table IN ('measurement', 'md_measurement')
    ORDER BY site_network.site_id"
    
  } 
  
  if (database == "aqengland") {
    
    statement <-  "SELECT site_network_id, 
    site_network.site_id AS site,
    parameter_id AS variable,
    site_network.date_started, 
    site_network.date_ended, 
    data_table, 
    site.site_name, 
    site.latitude, 
    site.longitude,
    site.altitude, 
    site_network.EU_site_id AS eu_site_id,
    site_network.network_parameter_id,
    LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id,
    network.network_name,
    LCASE(REPLACE(environment.environment_code, ' ', '_')) AS site_type,
    REPLACE(environment.EU_type, ' ', '_') AS site_type_eu,
    zone_region.region_name AS region,
    agglomeration_region.region_name AS agglomeration
    FROM site_network
    LEFT JOIN network_parameter
    ON site_network.network_parameter_id = network_parameter.network_parameter_id
    LEFT JOIN site
    ON site_network.site_id = site.site_id
    LEFT JOIN network
    ON network.network_id = network_parameter.network_id
    LEFT JOIN environment
    ON site_network.environment_id = environment.environment_id
    LEFT JOIN zone_region
    ON site.zone_region = zone_region.region_id
    LEFT JOIN agglomeration_region
    ON site.agglomeration_region = agglomeration_region.region_id
    WHERE data_table IN ('measurement', 'md_measurement')
    ORDER BY site_network.site_id"
    
  }
  
  if (database %in% c("waq", "scotarc", "niarc", "kent", "nlincs")) {
    
    statement <-  "SELECT site_network_id, 
    site_network.site_id AS site,
    parameter_id AS variable,
    site_network.date_started, 
    site_network.date_ended, 
    data_table, 
    site.site_name, 
    site.latitude, 
    site.longitude,
    site.altitude, 
    site_network.network_parameter_id,
    LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id,
    network.network_name,
    LCASE(REPLACE(environment.environment_code, ' ', '_')) AS site_type,
    zone_region.region_name AS region,
    agglomeration_region.region_name AS agglomeration
    FROM site_network
    LEFT JOIN network_parameter
    ON site_network.network_parameter_id = network_parameter.network_parameter_id
    LEFT JOIN site
    ON site_network.site_id = site.site_id
    LEFT JOIN network
    ON network.network_id = network_parameter.network_id
    LEFT JOIN environment
    ON site_network.environment_id = environment.environment_id
    LEFT JOIN zone_region
    ON site.zone_region = zone_region.region_id
    LEFT JOIN agglomeration_region
    ON site.agglomeration_region = agglomeration_region.region_id
    WHERE data_table IN ('measurement', 'md_measurement')
    ORDER BY site_network.site_id"
    
  }
  
  if (database %in% c("gibraltar", "andorra")) {
    
    statement <-  "SELECT site_network_id, 
    site_network.site_id AS site,
    parameter_id AS variable,
    site_network.date_started, 
    site_network.date_ended, 
    data_table, 
    site.site_name, 
    site.latitude, 
    site.longitude,
    site.altitude, 
    site_network.network_parameter_id,
    LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id,
    network.network_name,
    LCASE(REPLACE(environment.environment_code, ' ', '_')) AS site_type,
    REPLACE(environment.EU_type, ' ', '_') AS site_type_eu
    FROM site_network
    LEFT JOIN network_parameter
    ON site_network.network_parameter_id = network_parameter.network_parameter_id
    LEFT JOIN site
    ON site_network.site_id = site.site_id
    LEFT JOIN network
    ON network.network_id = network_parameter.network_id
    LEFT JOIN environment
    ON site_network.environment_id = environment.environment_id
    WHERE data_table IN ('measurement', 'md_measurement')
    ORDER BY site_network.site_id"
    
  }
  
  
  # No zone or agglomeration
  if (database == "ndarchive") {
    
    statement <-  "SELECT site_network_id, 
    site_network.site_id AS site,
    parameter_id AS variable,
    site_network.date_started, 
    site_network.date_ended, 
    data_table, 
    site.site_name, 
    site.latitude, 
    site.longitude,
    site.altitude, 
    site_network.network_parameter_id,
    LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id,
    network.network_name,
    LCASE(REPLACE(environment.environment_code, ' ', '_')) AS site_type
    FROM site_network
    LEFT JOIN network_parameter
    ON site_network.network_parameter_id = network_parameter.network_parameter_id
    LEFT JOIN site
    ON site_network.site_id = site.site_id
    LEFT JOIN network
    ON network.network_id = network_parameter.network_id
    LEFT JOIN environment
    ON site_network.environment_id = environment.environment_id
    WHERE data_table IN ('measurement', 'md_measurement')
    ORDER BY site_network.site_id"
    
  }
  
  # Return
  statement
  
}


# Function to use look-up statement and then do some cleaning. 
# 
# No export
get_lookup_table <- function(con, database_name) {
  
  # Get look up table
  suppressWarnings(
    df <- DBI::dbGetQuery(con, lookup_statement(database_name))
  )
  
  # Add database name
  df$database_name <- database_name
  
  # Clean dates
  df$date_started <- lubridate::ymd(df$date_started, quiet = TRUE)
  df$date_ended <- ifelse(df$date_ended == "0000-00-00", NA, df$date_ended)
  df$date_ended <- lubridate::ymd(df$date_ended, quiet = TRUE)
  
  # Transform
  df$site_friendly <- clean_strings_post_query(df$site, "site")
  df$variable_friendly <- clean_strings_post_query(df$variable, "variable")
  
  # Return
  df
  
}


# Function to catch understandable, but incorrect names for the databases. 
# 
# No export
parse_database_name <- function(name) {
  
  # Lowercase
  name <- stringr::str_to_lower(name)
  
  # Add here and in parser
  name <- switch(name, 
                 "ukair" =, "uk_air" =, "archive" = "archive", 
                 "england" =, "aqengland" = "aqengland", 
                 "wales" =, "waq" = "waq", 
                 "scotland" =, "scotarc" = "scotarc", 
                 "northern_ireland" =, "nireland" =, "n_ireland" =, "niarc" = "niarc",
                 "gib" =, "gibraltar" = "gibraltar", 
                 "andorra" = "andorra", 
                 "kent" = "kent", 
                 "nlincs" = "nlincs",
                 "ndarchive" = "ndarchive")
  
  # Return
  name
  
}


# Parse dates. This will handle normal and UK locale preference as well as 
# strings or integers which are years. 
#
# No export
parse_date_arguments <- function(date, what) {
  
  # Start of year
  if (what == "start") {
    
    # Catch for when years are used as dates
    if (!is.na(date) & nchar(date) == 4) date <- stringr::str_c(date, "-01-01")
    
    # Round
    date <- ifelse(is.na(date), 
       as.character(lubridate::floor_date(Sys.Date(), "year")), date)
    
  }
  
  # End of year
  if (what == "end") {
    
    if (!is.na(date) & nchar(date) == 4) date <- stringr::str_c(date, "-12-31")
    
    # Round
    date <- ifelse(is.na(date), 
      as.character(lubridate::ceiling_date(Sys.Date(), "year")), date)
    
  }
  
  # Parse date
  date <- lubridate::parse_date_time(date, c("ymd", "dmy"))
  
  # Return
  date
  
}


# Function to search multiple variables for a string
#
# No export
searcher <- function(database, string) {
  
  # Connect to database
  con <- db_connect_with_name(database)
  
  # Get look up table
  df <- get_lookup_table(con, database)
  
  # Disconnect
  DBI::dbDisconnect(con)
  
  # Filtering to string using many variables
  # Warnings are due to character locale issues in Wales database
  suppressWarnings(
    logical_list <- lapply(df, function (x) grepl(string, x, ignore.case = TRUE))
  )
  
  logical_matrix <- do.call("rbind", logical_list)
  logical_vector <- apply(logical_matrix, 2, function (x) any(x))
  
  # Filter
  df <- df[logical_vector, ]
  row.names(df) <- NULL
  
  # Return
  df
  
}


# Function to switch useful statistic-type name to the integers used in the 
# databases. Uses a static look-up table. 
#
# No export
switch_statistic <- function(string) {
  
  # Clean
  string <- stringr::str_to_lower(string)
  string <- stringr::str_replace_all(string, "\\.| ", "_")
  
  # Load lookup table
  df <- load_statistics_data()
  
  # Find matching
  index <- ifelse(string == df$statistic, TRUE, FALSE)
  
  # Filter
  integer <- df$integer[index]
  
  if (length(integer) != 1) stop("Statistic not recognised.", call. = FALSE)
  
  # Return
  integer
  
}


# Function to load statistics look-up table
#
# No export
load_statistics_data <- function() {
  
  # Get look-up table location
#  file_name <- system.file("extdata/statistic_types.csv", package = "importr")
 
  file_name <- "statistic_types.csv"

    
  # Load data
  df <- read.csv(file_name, stringsAsFactors = FALSE)
  
  # Return
  df
  
}


clean_strings_post_query <- function(string, variable) {
  
  # For variable codes
  if (variable == "variable") {
    
    # Cobalt, collides with carbon monoxide
    string <- stringr::str_replace(string, "Co", "cobalt")
    
    # Lower case
    string <- stringr::str_to_lower(string)
    
    # Spaces
    string <- stringr::str_replace_all(string, " ", "_")
    
    # Wind variables
    # Is the m_ prefix for modeled? 
    string <- stringr::str_replace_all(string, "m_sped|sped", "ws")
    string <- stringr::str_replace_all(string, "m_dir|dir", "wd")
    
    # NOx
    string <- stringr::str_replace_all(string, "noxasno2", "nox")
    
    # Temperature
    string <- stringr::str_replace_all(string, "\\bm_t\\b|\\bt\\b|\\bt\\t", "temp")
    
    # Rain
    string <- stringr::str_replace_all(string, "\\br\\b", "rain")
    
    # pm25 components for marga network
    string <- stringr::str_replace_all(string, "2.5$", "25")
    
  }
  
  # For site codes
  if (variable == "site") string <- stringr::str_to_lower(string)
  
  # For statistic types
  # Percent causes issues in R's data frame headers
  if (variable == "statistic") string <- stringr::str_replace(string, "^%_", "")
  
  # Return
  string
  
}


# Function to move variables to left hand side of data frame. Taken from threadr.
# No export
arrange_left <- function(df, variable) {
  variable <- stringr::str_c("\\b", variable, "\\b")
  index <- sapply(variable, function(x) grep(x, names(df)), 
                  USE.NAMES = FALSE)
  df <- df[, c(c(index), (1:ncol(df))[-index])]
  df
}


#' Function to print validity table. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
print_validity <- function() {
  
  # Connect
  con <- db_connect_with_name("archive")
  
  # Query and return
  df <- DBI::dbGetQuery(con, "SELECT validity_id,
                              Description AS description 
                              FROM validity")
  
  # Disconnect
  DBI::dbDisconnect(con)
  
  # Return
  df
  
}


#' Function to print verification table. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
print_verification <- function() {
  
  # Connect
  con <- db_connect_with_name("archive")
  
  # Query and return
  df <- DBI::dbGetQuery(con, "SELECT verify_id, 
                              Status AS status 
                              FROM verification")
  
  # Disconnect
  DBI::dbDisconnect(con)
  
  # Return
  df
  
}


#' Function to print ratified table. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
print_ratified <- function() {
  
  # Connect
  con <- db_connect_with_name("archive")
  
  # Query and return
  df <- DBI::dbGetQuery(con, "SELECT ratified_id, 
                              LCASE(ratified_name) AS ratified_name
                              FROM ratified")
  
  # Disconnect
  DBI::dbDisconnect(con)
  
  # Return
  df
  
}


#' Function to print units table. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
print_units <- function() {
  
  # Connect
  con <- db_connect_with_name("archive")
  
  # Query and return
  df <- DBI::dbGetQuery(con, "SELECT unit_id,
                              unit_name,
                              unit_long_name, 
                              LCASE(unit_type) AS unit_type
                              FROM units
                              ORDER BY unit_id")
  
  # Disconnect
  DBI::dbDisconnect(con)
  
  # Return
  df
  
}
