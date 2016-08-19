#' Function to import pre-calculated air quality statistics from databases. 
#' 
#' \code{import_stats} is a user friendly wrapper for SQL statements which can 
#' be used with the various air quality measurement databases. 
#' 
#' \code{import_stats} forces uses to specify a database name because there is 
#' potential for particular sites to be duplicated among the different databases.
#' If you need to query more than one database, you will need to call the 
#' function multiple times.
#'  
#' \code{import_stats} makes as few manipulations to the data as possible,
#' however there are some exceptions:
#' \itemize{
#'   \item Dates are parsed and are always named \code{date}.
#'   \item Wind speed and wind direction variable names are always forced to be
#'   \code{ws} and \code{wd}. This is done because \code{ws} and \code{wd} are
#'   reserved words for \strong{openair}'s functions such as \code{timeAverage} 
#'   and \code{windRose}.
#'   \item \code{nox} is used to represent \code{NOXasNO2} for the input and output of
#'   the function. 
#'   \item \code{cobalt} is used to represent \code{Co} for the input and output
#'   of the function. 
#'   \item Atmospheric temperature and rainfall are labelled as \code{temp} and
#'   \code{rain}. 
#'   \item The string arguments are case insensitive and sites and variables will
#'   be returned as lower-case. 
#'   \item This function does no rounding, so what the function returns is what 
#'   is stored in the databases.
#' }
#' 
#' @param database What is the name of the database you want to use? Note, this
#' is a name, not a connection. Use \code{\link{print_database_names}} to find
#' databases which can be connected to. 
#' 
#' @param site What site(s) should data be returned for? This is a site code, 
#' not the site name. Use \code{\link{search_database}} to search for sites. 
#' 
#' @param variable What variable(s) should be returned? Use 
#' \code{\link{search_database}} to search for sites. 
#' 
#' @param start What is the start date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and
#' will floor-rounded. . 
#' 
#' @param end What is the end date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and 
#' will be ceiling-rounded. 
#' 
#' @param statistic What statistic(s) do you want to return? Use 
#' \code{print_statistics_type} to find what values this argument can take. The
#' default is \code{"annual_mean"} and \code{"annual_data_capture"}. 
#' 
#' @param extra Should the returned data frame contain things like unit, validity,
#' and ratification data? The default is \code{FALSE}. Set \code{extra} to 
#' \code{TRUE} to get these data. 
#' 
#' @param site_network_id Get data for a specific \code{site_network_id}. This 
#' is the indexed key in the databases which relate to a "process" (a 
#' site-pollutant) combination. This will override \code{site} and \code{variable}
#' arguments if used. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{import_measures}}, \code{\link{search_database}},
#' \code{\link{print_database_names}}, \code{\link{print_statistic_types}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get annual means for PM10 (gravimetric) for the Heathrow Green Gates site
#' stats_green_gates <- import_stats(
#'   "aqengland", site = "t55", variable = "ge10", start = 1970, 
#'   statistic = "annual_mean")
#' 
#' # Get two pollutants and different statistic types for Heathrow Green Gates
#' import_stats(
#'   "aqengland", site = "t55", variable = c("ge10", "no2"), start = 1970, 
#'   statistic = c("annual_mean", "annual_max", "annual_min"), extra = FALSE)
#' 
#' }
#'
#' @export
import_stats <- function(database = "archive", site = NA, variable = "no2", 
                         start = NA, end = NA, 
                         statistic = c("annual_mean", "annual_data_capture"), 
                         extra = FALSE, site_network_id = NA) {
  
  # Do some checking
  if (length(database) != 1) stop("Only one database can be used.", call. = FALSE)
  
  # # A constraint to keep return not too messy
  # if (length(variable) != 1 & !extra) {
  #
  #   stop("Only one variable can be used when `extra` is 'FALSE'.", call. = FALSE)
  # 
  # }
  
  # Parse arguments
  if (!is.na(site[1])) {
    
    site <- stringr::str_to_lower(site)
    site <- stringr::str_trim(site)
    
  }
  
  variable <- stringr::str_to_lower(variable)
  variable <- stringr::str_trim(variable)
  
  # Date arguments
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")

  # Parse database name
  database <- parse_database_name(database)
  
  # Switch statistic
  statistic_integer <- sapply(statistic, switch_statistic, USE.NAMES = FALSE)
  
  # Connect to database
  con <- db_connect_with_name(database)
  
  # Get look up table which is used for getting database keys
  df_look <- get_lookup_table(con, database)
  
  # Use of 'site_network_id' will over-ride all other arguments if used
  if (!is.na(site_network_id[1])) {
    
    # Filter to site_network_id
    df_look <- df_look[df_look$site_network_id %in% site_network_id, ]
    
    # Power users will probably want extra data
    extra <- TRUE
    
  } else {
    
    # Use arguments to filter look-up table
    if (grepl(variable[1], "all", ignore.case = TRUE)) {
      
      # Filter only to site
      df_look <- df_look[df_look$site_friendly %in% site, ]
      
    } else {
      
      # Filter to site and variable
      df_look <- df_look[df_look$site_friendly %in% site & 
                           df_look$variable_friendly %in% variable, ]
      
    }
    
  }
  
  # Stop if no data is within tables
  if (nrow(df_look) == 0) stop("No data found in database tables.", call. = FALSE)
  
  # Rename friendly variables
  names(df_look) <- stringr::str_replace(names(df_look), "_friendly$", "")
  
  # Replicate look-up table for n statistic-types
  df_look <- df_look[rep(seq_len(nrow(df_look)), each = length(statistic_integer)), ]
  
  # Add a statistics variable, recycle the elements to fit the data frame
  df_look <- cbind(df_look, statistic, statistic_integer)
  
  # Query database
  df <- get_statistics_data(database, con, df_look, start, end)
  
  # Clean strings
  df$site <- clean_strings_post_query(df$site, "site")
  df$variable <- clean_strings_post_query(df$variable, "variable")
  df$statistic_type <- clean_strings_post_query(df$statistic_type, "statistic")

  if (extra) {
    
    # Just arrange table
    df <- arrange_left(df, c("date", "date_end", "site", "site_name", "variable",
                             "value"))
    
  } else {
    
    # Reshape data
    # Select what is needed
    df <- dplyr::select(df, date, date_end, site, site_name, variable,
                        statistic_type, value)
    
    # Only distinct values
    df_distinct <- dplyr::distinct(df, date, variable, site, statistic_type)
    
    # Warn if data has been removed for reshaping
    if (nrow(df) > nrow(df_distinct)) {
      
      warning("Duplicate statistic/date pairs ('site_network_id's) detected. Data has been removed.",
              call. = FALSE)
      
    }
    
    # Reshape to make wide data, use distinct values and reassign here
    df <- tidyr::spread(df_distinct, statistic_type, value)
    
    # Arrange
    df <- dplyr::arrange(df, site, variable, date)
    
    # Ensure names are unique
    if (any(duplicated(names(df)))) { 
      
      names(df) <- make.names(names(df), unique = TRUE)
      names(df) <- stringr::str_replace_all(names(df), "\\.", "_")
      
    }
    
  }
  
  # Return
  data.frame(df)
  
}


# Function to build statement to select statistics data. 
# 
# No export
statistics_statement <- function(database_name, id, start, end, statistic) {
  
  # Archive has `verification` and `validity` tables
  if (database_name == "archive") {
    
    statement <- stringr::str_c(
      "SELECT statistics.site_network_id, 
      statistics.statistic_year AS year, 
      CASE WHEN statistics.statistic_month = 0 THEN 1 ELSE statistics.statistic_month END AS month,
      CASE WHEN statistics.statistic_day = 0 THEN 1 ELSE statistics.statistic_day END AS day,
      statistics.statistic_time AS time,
      CASE WHEN statistics.statistic = -9999 THEN NULL ELSE statistics.statistic END AS value,
      statistics.statistic_type_id,
      statistics.ratified_id, 
      statistics.verify_id,
      statistics.validity_id,
      statistics.unit_id,
      units.unit_name,
      LCASE(REPLACE(statistic_type.statistic_type, ' ', '_')) AS statistic_type,
      site_network.network_parameter_id,
      LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id,
      site_network.site_id AS site, 
      site.site_name,
      network_parameter.parameter_id AS variable
      FROM statistics
      LEFT JOIN units 
      ON statistics.unit_id = units.unit_id
      LEFT JOIN statistic_type
      ON statistic_type.statistic_type_id = statistics.statistic_type_id
      LEFT JOIN site_network
      ON statistics.site_network_id = site_network.site_network_id
      LEFT JOIN network_parameter
      ON site_network.network_parameter_id = network_parameter.network_parameter_id
      LEFT JOIN network
      ON network_parameter.network_id = network.network_id
      LEFT JOIN site
      ON site_network.site_id = site.site_id
      WHERE statistics.site_network_id = ", id, " AND 
      statistics.statistic_year BETWEEN '", start, "' AND '", end, "'
      AND statistics.statistic_type_id = ", statistic, "")
    
  } else {
    
    statement <- stringr::str_c(
      "SELECT statistics.site_network_id, 
      statistics.statistic_year AS year, 
      CASE WHEN statistics.statistic_month = 0 THEN 1 ELSE statistics.statistic_month END AS month,
      CASE WHEN statistics.statistic_day = 0 THEN 1 ELSE statistics.statistic_day END AS day,
      statistics.statistic_time AS time,
      CASE WHEN statistics.statistic = -9999 THEN NULL ELSE statistics.statistic END AS value,
      statistics.statistic_type_id,
      statistics.ratified_id, 
      statistics.unit_id,
      units.unit_name,
      LCASE(REPLACE(statistic_type.statistic_type, ' ', '_')) AS statistic_type,
      site_network.network_parameter_id,
      LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id,
      site_network.site_id AS site, 
      site.site_name,
      network_parameter.parameter_id AS variable
      FROM statistics
      LEFT JOIN units 
      ON statistics.unit_id = units.unit_id
      LEFT JOIN statistic_type
      ON statistic_type.statistic_type_id = statistics.statistic_type_id
      LEFT JOIN site_network
      ON statistics.site_network_id = site_network.site_network_id
      LEFT JOIN network_parameter
      ON site_network.network_parameter_id = network_parameter.network_parameter_id
      LEFT JOIN network
      ON network_parameter.network_id = network.network_id
      LEFT JOIN site
      ON site_network.site_id = site.site_id
      WHERE statistics.site_network_id = ", id, " AND 
      statistics.statistic_year BETWEEN '", start, "' AND '", end, "'
      AND statistics.statistic_type_id = ", statistic, "")
    
  }
  
  # Return
  statement
  
}


get_statistics_data <- function(database, con, df_look, start, end) {
  
  # Print a message
  message("Querying statistics tables...")
  
  # Get data for rows in lookup table, need to create a list so input is not 
  # returned too
  list_return <- plyr::alply(df_look, 1, query_statistics, database, con, start, end, 
                             .progress = "time")
  
  # Bind
  df <- dplyr::bind_rows(list_return)
  
  # Finished with database connection
  DBI::dbDisconnect(con)
  
  # Stop if no data are returned
  if (nrow(df) == 0) {
    
    stop("Database has been queried, but no data has been returned.", 
         call. = FALSE)
    
  }
  
  # Return
  df
  
}


# Function for querying
query_statistics <- function(df, database, con, start, end) {
  
  # Create SQL statements
  statement <- statistics_statement(database, df$site_network_id, start, end, 
                                    df$statistic_integer)
  
  # Query database
  suppressWarnings(
    df_query <- DBI::dbGetQuery(con, statement)
  )
  
  # Clean dates
  df_query <- format_statistic_dates(df_query, df$statistic, start, end)
  
  # Return
  df_query
  
}


# Function to format statistic dates post database query. 
# 
# I feel this function needs more work. 
# 
# No export
format_statistic_dates <- function(df, statistic, start, end) {
  
  # Clean data
  # Database uses 00:01:00 to indicate start time for years and months
  df$time <- stringr::str_replace(df$time, "00:01:00", "00:00:00")
  
  # Parse dates, this will deal with 24:00 dates
  df$date <- stringr::str_c(df$year, df$month, df$day, df$time, sep = " ")
  df$date <- lubridate::parse_date_time(df$date, c("ymd_hms", "ymd"), 
                                        tz = "UTC", quiet = TRUE)
  
  # Drop database times
  df$year <- NULL
  df$month <- NULL
  df$day <- NULL
  df$time <- NULL
  
  # Annual aggregations
  if (grepl("^annual_|winter|percentile", statistic)) {
    
    # To-do: fix this false match
    if (statistic != "annual_running_mean") {
      
      df$date_end <- df$date + lubridate::years(1)
      
    }
    
  }
  
  # Monthly, no such thing as adding a month, therefore round
  if (grepl("^monthly_", statistic)) {
    
    # Push forward 1 second 
    df$date_end <- df$date + lubridate::seconds(1)
    
    # Round
    df$date_end <- lubridate::ceiling_date(df$date_end, "month")
    
  }
  
  # Daily
  if (grepl("^daily_", statistic)) df$date_end <- df$date + lubridate::days(1)
  
  # More complicated statistics, to-do: confirm with Trevor that these pieces are 
  # correct
  # For co and o3
  if (grepl("8_hour_running_mean", statistic)) {
    
    # Date ending in database
    df$date_end <- df$date
    
    # Subtract 8 hours
    df$date <- df$date_end - lubridate::hours(8)
    
  }
  
  # For ozone, ge10, benzene
  if (grepl("hourly_max_ozone|24_hour_running_mean|annual_running_mean", statistic)) {
    
    # Date ending in database
    df$date_end <- df$date
    
    # Subtract an hour
    df$date <- df$date_end - lubridate::hours(1)
    
  }
  
  # Also another filter, bit of a poor piece of work but keeps things simple
  df <- dplyr::filter(df, date <= end & date >= start)
  
  # Return
  df
  
}

#' @export
import_statistics <- function(database = "archive", site, variable = "no2", 
                              start = NA, end = NA, 
                              statistic = c("annual_mean", "annual_data_capture"), 
                              extra = FALSE) {
  
  # Message
#  .Deprecated("import_stats", package = "importr")
   .Deprecated("import_stats")

  
  # Use function
  df <- import_stats(database, site, variable, start, end, statistic, extra)
  
  # Return
  df
}
