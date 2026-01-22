#' @title Get Station Parameters
#' @description `aq_get_lstation_parameters` obtains a list of all the parameters associated with a station or stations of interest.
#' @details This function obtains the list of parameters (IDs, names) and specific timeseries associated with queried stations
#' @param cdec_code three-letter code for station from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id numeric id for station; one option for querying
#' @param aq_location_id location id, as displayed in Aquarius database (combo of location_id and cdec_code); one option for querying
#' @returns returns data frame of parameters filtered to queried station(s)
#'
aq_get_station_parameters = function(cdec_code=NULL, location_id=NULL, aq_location_id=NULL) {

  # Get station info for all locations
  data <- aq_get_location_list()

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- data %>%
    {if (!is.null(cdec_code)) filter(., cdec_code %in% !!cdec_code) else .} %>%
    {if (!is.null(location_id)) filter(., location_id %in% !!location_id) else .} %>%
    {if (!is.null(aq_location_id)) filter(., aq_location_id %in% !!aq_id) else .}

  # Get all identifiers for entered stations
  Identifiers <- data_filtered$aq_location_id

  # Need to reconnect
  aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
             username = Sys.getenv("AQTS_USERNAME"),
             password = Sys.getenv("AQTS_PASSWORD"))

  # Ensure disconnect on exit
  on.exit(aq_disconnect())

  # Use purrr to loop through each identifier and get time series descriptions
  json_ts_params_df <- map_df(Identifiers, function(id) {
    cat("Fetching parameters for station:", id, "\n")

    ts_params <- timeseries$getTimeSeriesDescriptions(locationIdentifier = id)

    # Debug: check what we got
    cat("  Found", length(ts_params$Identifier), "time series\n")

    if (length(ts_params$Identifier) > 0) {
      data.frame(
        aq_location_id = ts_params$LocationIdentifier,
        parameter_id = ts_params$ParameterId,
        parameter_name = ts_params$Parameter,
        unit = ts_params$Unit,
        ts_id = ts_params$Identifier,
        stringsAsFactors = FALSE
      )
    } else {
      NULL  # map_df will skip NULL results
    }
  })

  # Check if we got any data
  if (nrow(json_ts_params_df) == 0) {
    cat("No data found.\n")
    stop("No data retrieved. Exiting.")
  }

  cat("\nRetrieved", nrow(json_ts_params_df), "parameters from",
      length(unique(json_ts_params_df$aq_location_id)), "station(s)\n\n")

  # Join with location metadata and select columns
  df <- json_ts_params_df %>%
    left_join(data_filtered, by = "aq_location_id") %>%
    select(cdec_code, location_id, aq_station_name, aq_location_id, parameter_id, parameter_name, unit, updated_at)

  # Return df
  return(df)

}
