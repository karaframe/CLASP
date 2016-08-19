#' Function to import air quality measurement data from databases. 
#' 
#' \code{import_measures} is a user friendly wrapper for SQL statements which
#' can be used with various air quality measurement databases. 
#' 
#' \code{import_measures} forces uses to specify a database name because there
#' is potential for particular sites to be duplicated among the different 
#' databases. If you need to query more than one database, you will need to call
#' the function multiple times.
#' 
#' If many pollutants are selected along with many sites, a little patience is
#' needed as the databases are often large and are updated regularly.
#' 
#' \code{import_measures} makes as few manipulations to the data as possible,
#' however there are some exceptions:
#' \itemize{
#'   \item Dates are parsed and are always named \code{date} and \code{date_end}. 
#'   \item Wind speed and wind direction variable names are always forced to be
#'   \code{ws} and \code{wd}. This is done because \code{ws} and \code{wd} are
#'   reserved words for \strong{openair}'s functions such as \code{timeAverage} 
#'   and \code{windRose}.
#'   \item \code{nox} is used to represent \code{NOXasNO2} for the input and 
#'   output of the function. 
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
#' will floor-rounded. 
#' 
#' @param end What is the end date of data to be returned? Ideally, the 
#' date format should be \code{yyyy-mm-dd}, but the UK locale convention of 
#' \code{dd/mm/yyyy} will also work. Years as strings or integers work too and 
#' will be ceiling-rounded. 
#' 
#' @param extra Should the returned data frame contain things like unit, validity,
#' and ratification data? The default is \code{FALSE}. Set \code{extra} to 
#' \code{TRUE} to get these data. 
#'
#' @param period What measurement period should be returned? Default is 
#' \code{"hour"} for hourly data. \code{period} is only really useful when only
#' 15-minute SO2 data is desired. Use \code{"15_min"} for 15-minute data. 
#' Using \code{"15_min"} will also drop all multi-day variables. 
#' 
#' @param site_network_id Get data for a specific \code{site_network_id}. This 
#' is the indexed key in the databases which relate to a "process" (a 
#' site-pollutant) combination. This will override \code{site} and \code{variable}
#' arguments if used. 
#' 
#' @param pm_sum Should \code{pm10} and \code{pm25} variables be calculated if
#' volatile and non-volatile components of a particle size fraction are 
#' detected? This is a simple sum of the components and can be helpful when the 
#' summed masses are not stored in the database, usually found for TEOM-FDMS 
#' instruments. Default is \code{FALSE} and will only be applied when 
#' \code{extra} is \code{FALSE}. 
#'
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{import_stats}}, \code{\link{search_database}},
#' \code{\link{print_database_names}}
#' 
#' @examples
#' \dontrun{
#' # Get NO2 data for Reading New Town for 2014
#' data_reading <- import_measures(database = "archive", site = "rea1", 
#'                                 variable = "no2", start = "2014-01-01", 
#'                                 end = "2014-12-31")
#' 
#' 
#' # Get many variables for Rosia Road in Gibraltar since 2005
#' data_rosia <- import_measures(
#'  database = "gibraltar", site = "gib1", 
#'  variable = c("co", "no2", "nox", "no",  "gr25", "ge10"), start = 2005,
#'  extra = TRUE)
#' 
#' }
#' 
#' @export
import_measures <- function(database = "archive", site = NA, 
                            variable = c("no2", "ws", "wd"), 
                            start = NA, end = NA, extra = FALSE, 
                            period = "hour", site_network_id = NA, 
                            pm_sum = FALSE) {
  
  # Check inputs
  if (length(database) != 1) stop("Only one database can be used.", call. = FALSE)
  
  # Parse arguments
  # Site codes
  if (!is.na(site[1])) {
    
    site <- stringr::str_to_lower(site)
    site <- stringr::str_trim(site)
    
  }
  
  # Variables
  variable <- stringr::str_to_lower(variable)
  variable <- stringr::str_trim(variable)
  
  # Measurement period, a switch
  period <- stringr::str_trim(period)
  period <- stringr::str_replace_all(period, " |\\.", "_")
  period <- ifelse(period == "hour", 1, period)
  period <- ifelse(period == "15_min", 2, period)
  
  # Date arguments
  start <- parse_date_arguments(start, "start")
  end <- parse_date_arguments(end, "end")

  # Parse database name
  database <- parse_database_name(database)
  
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
    
    # If 15-minutes remove all multi-day variables too
    if (period == 2) df_look <- df_look[df_look$data_table != "md_measurement", ]
    
  }
  
  # Stop if no data is within look-up table
  if (nrow(df_look) == 0) stop("No data found in database tables.", call. = FALSE)
  
  # Rename friendly variables
  names(df_look) <- stringr::str_replace(names(df_look), "_friendly$", "")
  
  # Query database
  df <- get_measurement_data(database, con, df_look, start, end, period)
  
  # Clean variables
  df$variable <- clean_strings_post_query(df$variable, "variable")
  df$site <- clean_strings_post_query(df$site, "site")
  
  if (extra) {
    
    # Just arrange table
    df <- arrange_left(df, c("date", "date_end", "site", "site_name", "variable", 
                             "site_network_id", "value"))
    
  } else {
    
    # Reshaping
    # Select what is needed
    df <- dplyr::select(df, date, date_end, site, site_name, variable, value)
    
    # Reshape data
    df <- tryCatch({
      
      # Make data wider
      tidyr::spread(df, variable, value) 
      
    }, error = function (e) {
      
      # On error due to duplication, use distinct, but warn
      warning("Duplicate measurements ('site_network_id's) detected. Data has been removed.",
              call. = FALSE)
      
      # Distinct elements only
      df <- dplyr::distinct(df, date, site, variable)
      
      # Make data wider
      tidyr::spread(df, variable, value)
      
    })
    
    # Arrange
    df <- dplyr::arrange(df, site, date)
    
    # Sum fdms variables if appropriate
    if (pm_sum) df <- fdms_pm_sum(df)
    
    # Ensure names are unique
    if (any(duplicated(names(df)))) { 
      
      names(df) <- make.names(names(df), unique = TRUE)
      names(df) <- stringr::str_replace_all(names(df), "\\.", "_")
      
    }
    
    # Arrange variables
    df <- arrange_left(df, c("date", "date_end", "site", "site_name"))
    
  }
  
  # Return, drop dply's tbl_df
  data.frame(df)
  
}


query_measurements <- function(df, con, database, start, end, period) {
  
  # Create statement
  statement <- long_statement(database, df$site_network_id, df$data_table, start, 
                              end, period)
  
  # Query database
  suppressWarnings(
    df_query <- DBI::dbGetQuery(con, statement)
  )
  
  # Clean dates
  df_query <- format_measurement_dates(df_query, df$data_table, drop = FALSE)
  
  # Return
  df_query
  
}


get_measurement_data <- function(database, con, df_look, start, end, period) {
  
  # Print a message
  message("Querying measurement tables...")
  
  # Get data for rows in lookup table, need to create a list so input is not 
  # returned too
  list_return <- plyr::alply(df_look, 1, query_measurements, con, database, 
                             start, end, period, .progress = "time")
  
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


# I'm not happy with the duplication of statements here. It is asking for 
# trouble, but I do not know to work around things when variables do not exist 
# in the tables.
long_statement <- function(database, key, table, start, end, period) {
  
  # archive and gibraltar have a `md_measurement` table which have different dates
  if (database %in% c("archive", "gibraltar", "scotarc")) {
    
    if (database == "archive") {
      
      # archive's `measurement` table has many extensions
      if (table == "measurement") {
        
        statement <- stringr::str_c(
          "SELECT", table, ".site_network_id, 
          CONCAT(", table, ".measurement_date, ' ', measurement_time) AS date_end,
          CASE WHEN", table, ".measurement = -9999 THEN NULL ELSE", table, ".measurement END AS value,",
          table, ".ratified_id,",
          table, ".verify_id,",
          table, ".validity_id,",
          table, ".measurement_period_id AS period,",
          table, ".less_than,
          site_network.site_id AS site,
          site.site_name,
          network_parameter.parameter_id AS variable,
          unit_name,
          LCASE(REPLACE(measurement_period.measurement_period, ' ', '_')) AS period_name,
          site_network.network_parameter_id,
          LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id
          FROM", table,
          "LEFT JOIN site_network
          ON", table, ".site_network_id = site_network.site_network_id
          LEFT JOIN site
          ON site_network.site_id = site.site_id
          LEFT JOIN network_parameter 
          ON site_network.network_parameter_id = network_parameter.network_parameter_id
          LEFT JOIN units 
          ON", table, ".unit_id = units.unit_id
          LEFT JOIN measurement_period
          ON", table, ".measurement_period_id = measurement_period.measurement_period_id
          LEFT JOIN network
          ON network_parameter.network_id = network.network_id
          WHERE", table, ".site_network_id = ", key, " 
          AND measurement_date BETWEEN '", start, "'AND'", end, "'", 
          "AND", table, ".measurement_period_id =", period, sep = " ")
        
      }
      
      if (table == "md_measurement") {
        
        statement <- stringr::str_c(
          "SELECT", table, ".site_network_id, 
          CONCAT(", table, ".measurement_start_date, ' ',", table, ".measurement_start_time) AS date,
          CONCAT(", table, ".measurement_date, ' ',", table, ".measurement_time) AS date_end,
          CASE WHEN", table, ".measurement = -9999 THEN NULL ELSE", table, ".measurement END AS value,",
          table, ".data_capture,",
          table, ".ratified_id,",
          table, ".verify_id,",
          table, ".validity_id,",
          table, ".measurement_period_id AS period,",
          table, ".less_than,",
          table, ".liquid_volume,",
          table, ".funnel_diam AS funnel_diameter,",
          table, ".EMEP_code AS emep_code,
          site_network.site_id AS site,
          site.site_name,
          network_parameter.parameter_id AS variable,
          unit_name,
          LCASE(REPLACE(measurement_period.measurement_period, ' ', '_')) AS period_name,
          site_network.network_parameter_id,
          LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id
          FROM", table,
          "LEFT JOIN site_network
          ON", table, ".site_network_id = site_network.site_network_id
          LEFT JOIN site
          ON site_network.site_id = site.site_id
          LEFT JOIN network_parameter 
          ON site_network.network_parameter_id = network_parameter.network_parameter_id
          LEFT JOIN units 
          ON", table, ".unit_id = units.unit_id
          LEFT JOIN measurement_period
          ON", table, ".measurement_period_id = measurement_period.measurement_period_id
          LEFT JOIN network
          ON network_parameter.network_id = network.network_id
          WHERE", table, ".site_network_id = ", key, "
          AND (measurement_date BETWEEN '", start, "'AND'", end, "'
          OR measurement_start_date BETWEEN '", start, "'AND'", end, "')", sep = " ")
        
      }
      
    }
    
    # Gibraltar, scotland database also has a `md_measurement` table
    if (database %in% c("gibraltar", "scotarc")) {
      
      if (table == "measurement") {
        
        statement <- stringr::str_c(
          "SELECT", table, ".site_network_id, 
          CONCAT(", table, ".measurement_date, ' ', measurement_time) AS date_end,
          CASE WHEN", table, ".measurement = -9999 THEN NULL ELSE", table, ".measurement END AS value,",
          table, ".ratified_id,",
          table, ".measurement_period_id AS period,",
          table, ".less_than,
          site_network.site_id AS site,
          site.site_name,
          network_parameter.parameter_id AS variable,
          unit_name,
          LCASE(REPLACE(measurement_period.measurement_period, ' ', '_')) AS period_name,
          site_network.network_parameter_id,
          LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id
          FROM", table,
          "LEFT JOIN site_network
          ON", table, ".site_network_id = site_network.site_network_id
          LEFT JOIN site
          ON site_network.site_id = site.site_id
          LEFT JOIN network_parameter 
          ON site_network.network_parameter_id = network_parameter.network_parameter_id
          LEFT JOIN units 
          ON", table, ".unit_id = units.unit_id
          LEFT JOIN measurement_period
          ON", table, ".measurement_period_id = measurement_period.measurement_period_id
          LEFT JOIN network
          ON network_parameter.network_id = network.network_id
          WHERE", table, ".site_network_id = ", key, "
          AND measurement_date BETWEEN '", start, "'AND'", end, "'", 
          "AND", table, ".measurement_period_id =", period, sep = " ")
      
    }
    
    if (table == "md_measurement") {
      
      statement <- stringr::str_c(
          "SELECT", table, ".site_network_id, 
          CONCAT(", table, ".measurement_start_date, ' ',", table, ".measurement_start_time) AS date,
          CONCAT(", table, ".measurement_date, ' ',", table, ".measurement_time) AS date_end,
          CASE WHEN", table, ".measurement = -9999 THEN NULL ELSE", table, ".measurement END AS value,",
          table, ".data_capture,",
          table, ".ratified_id,",
          table, ".measurement_period_id AS period,",
          table, ".less_than,",
          table, ".liquid_volume,",
          table, ".funnel_diam AS funnel_diameter,",
          table, ".EMEP_code AS emep_code,
          site_network.site_id AS site,
          site.site_name,
          network_parameter.parameter_id AS variable,
          unit_name,
          LCASE(REPLACE(measurement_period.measurement_period, ' ', '_')) AS period_name,
          site_network.network_parameter_id,
          LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id
          FROM", table,
          "LEFT JOIN site_network
          ON", table, ".site_network_id = site_network.site_network_id
          LEFT JOIN site
          ON site_network.site_id = site.site_id
          LEFT JOIN network_parameter 
          ON site_network.network_parameter_id = network_parameter.network_parameter_id
          LEFT JOIN units 
          ON", table, ".unit_id = units.unit_id
          LEFT JOIN measurement_period
          ON", table, ".measurement_period_id = measurement_period.measurement_period_id
          LEFT JOIN network
          ON network_parameter.network_id = network.network_id
          WHERE", table, ".site_network_id = ", key, "
          AND (measurement_date BETWEEN '", start, "'AND'", end, "'
          OR measurement_start_date BETWEEN '", start, "'AND'", end, "')", sep = " ")
      
    }
    
  }
  
  } else {
    
    # All other databases, not the same complement of tables, no `md_measurement`, 
    # `verification` and `validity` tables. 
    statement <- stringr::str_c(
      "SELECT", table, ".site_network_id, 
      CONCAT(", table, ".measurement_date, ' ', measurement_time) AS date_end,
      CASE WHEN", table, ".measurement = -9999 THEN NULL ELSE", table, ".measurement END AS value,",
      table, ".ratified_id,",
      table, ".measurement_period_id AS period,",
      table, ".less_than,
      site_network.site_id AS site,
      site.site_name,
      network_parameter.parameter_id AS variable,
      unit_name,
      LCASE(REPLACE(measurement_period.measurement_period, ' ', '_')) AS period_name,
      site_network.network_parameter_id,
      LCASE(REPLACE(network_parameter.network_id, ' ', '_')) AS network_id
      FROM", table,
      "LEFT JOIN site_network
      ON", table, ".site_network_id = site_network.site_network_id
      LEFT JOIN site
      ON site_network.site_id = site.site_id
      LEFT JOIN network_parameter 
      ON site_network.network_parameter_id = network_parameter.network_parameter_id
      LEFT JOIN units 
      ON", table, ".unit_id = units.unit_id
      LEFT JOIN measurement_period
      ON", table, ".measurement_period_id = measurement_period.measurement_period_id
      LEFT JOIN network
      ON network_parameter.network_id = network.network_id
      WHERE", table, ".site_network_id = ", key, "
      AND measurement_date BETWEEN '", start, "'AND'", end, "'", 
      "AND", table, ".measurement_period_id =", period, sep = " ")
    
  }
    
  # Return
  statement
  
}


# Function to format dates after a database query. 
# 
# \code{format_measurement_dates} takes an input data frame after a query and 
# then does date manipulations. The databases store data in a time-ending 
# notation while I, and openair, work in time-beginning notation. To remove 
# ambiguity, a \code{date} and \code{date_end} variables are calculated. 
# 
# This process also becomes important when dealing with multi-day pollutants
# because they have variable start and end dates. 
# 
# No export
format_measurement_dates <- function(df, table, drop = TRUE) {
  
  # Parse dates
  df$date_end <- lubridate::parse_date_time(df$date_end, c("ymd_hms", "ymd"), 
                                            tz = "UTC", quiet = TRUE)
  
  # The measurement table does not contain start dates, only end times
  if (table == "measurement") {
    
    # Preallocate with date and manipulate this variable
    df$date <- df$date_end
    
    # For code control to avoid repeated ifelse calls when not needed 
    period_vector <- unique(df$period)
    
    # Only apply functions if needed
    if (1 %in% period_vector) {
      
      df$date <- ifelse(df$period == 1, df$date_end - lubridate::hours(1), df$date)
      
    }
    
    if (2 %in% period_vector) {
      
      df$date <- ifelse(df$period == 2, df$date_end - lubridate::minutes(15), df$date)
      
    }
    
    # Daily time is noon
    if (3 %in% period_vector) {
      
      df$date <- ifelse(df$period == 3, df$date_end - lubridate::hours(12), df$date)
      df$date_end <- ifelse(df$period == 3, df$date_end + lubridate::hours(12), df$date_end)
      
    }
    
    if (10 %in% period_vector) {
      
      df$date <- ifelse(df$period == 10, df$date_end - lubridate::minutes(30), df$date)
      
    }
    
    if (12 %in% period_vector) {
      
      df$date <- ifelse(df$period == 12, df$date_end - lubridate::hours(2), df$date)
      
    }
    
    if (13 %in% period_vector) {
      
      df$date <- ifelse(df$period == 13, df$date_end - lubridate::hours(3), df$date)
    }
    
    if (14 %in% period_vector) {
      df$date <- ifelse(df$period == 14, df$date_end - lubridate::hours(4), df$date)
      
    }
    
    # ifelse will coerce dates to unix time, format again
    df$date <- as.POSIXct(df$date, origin = "1970-01-01", tz = "UTC")
    
    # Only for daily
    if (class(df$date_end)[1] == "numeric") {
      
      df$date_end <- as.POSIXct(df$date_end, origin = "1970-01-01", tz = "UTC")  
      
    }
    
  }
  
  if (table == "md_measurement") {
    
    # Parse dates only
    df$date <- lubridate::parse_date_time(df$date, c("ymd_hms", "ymd"), 
                                          tz = "UTC", quiet = TRUE)
    
  }
  
  # Drop period
  if (drop) df$period <- NULL
  
  # Return
  df
  
}


# No export
fdms_pm_sum <- function(df) {
  
  # Calculate pm10 for fdmss, but only if components are present and pm10 is not
  if (all(c("nv10", "v10") %in% names(df) & !"pm10" %in% names(df))) {
    
    # Simple sum of non-volatile and volatile components
    df$pm10 <- rowSums(cbind(df$nv10, df$v10))
    
  }
  
  # Same for pm25
  if (all(c("nv25", "v25") %in% names(df) & !"pm25" %in% names(df))) {
    
    # Simple sum
    df$pm25 <- rowSums(cbind(df$nv25, df$v25))
    
  }
  
  # Return
  df
  
}


#' @export
import_measurements <- function(database = "archive", site, 
                                variable = c("no2", "ws", "wd"), 
                                start = NA, end = NA, extra = FALSE, 
                                period = "hour") {
  
  # Message
  .Deprecated("import_measures", package = "importr")

  
  # Use function
  df <- import_measures(database, site, variable, start, end, extra, period)
  
  # Return
  df
}
