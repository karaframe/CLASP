


function (database, string = "", extra = FALSE, site_network_id = NA) 
{
  if (database == "all") {
    database <- c("archive", "aqengland", "waq", "scotarc", 
                  "niarc", "gibraltar", "andorra", "kent", "nlincs", 
                  "ndarchive")
  }
  database <- sapply(database, parse_database_name, USE.NAMES = FALSE)
  string <- stringr::str_c(string, collapse = "|")
  df <- plyr::ldply(database, searcher, string = string)
  if (!is.na(site_network_id[1])) {
    df <- df[df$site_network_id %in% site_network_id, ]
  }
  else {
    if (!extra) {
      df <- subset(df, select = c(site_network_id, site_friendly, 
                                  site_name, variable_friendly, date_started, date_ended, 
                                  site_type, latitude, longitude, data_table, database_name))
      names(df) <- stringr::str_replace(names(df), "_friendly$", 
                                        "")
    }
  }
  df <- dplyr::arrange(df, site_name)
  df
}
