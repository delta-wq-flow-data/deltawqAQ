#' @title Get Station Parameters
#' @description `aq_get_station_parameters` obtains a list of all the parameters associated with a station or stations of interest.
#' @details This function obtains the list of parameters (IDs, names) and specific timeseries associated with queried stations
#' @param cdec_code three-letter code for station from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id numeric id for station; one option for querying
#' @param aq_location_id location id, as displayed in Aquarius database (combo of location_id and cdec_code); one option for querying
#' @param connect TRUE/FALSE value indicates whether or not connection to Aquarius is needed. When this function is called within
#' other functions that have already connected to the database, this value should be FALSE to avoid errors.
#' @returns returns data frame of parameters filtered to queried station(s)
#'
aq_get_station_parameters = function(cdec_code=NULL, location_id=NULL, aq_location_id=NULL, connect = TRUE) {

  # Only connect if requested (for standalone use)
  if (connect) {
    aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
               username = Sys.getenv("AQTS_USERNAME"),
               password = Sys.getenv("AQTS_PASSWORD"))
    on.exit(aq_disconnect())
  }

  # Get station info for all locations
  data <- aq_get_location_list(connect = FALSE)

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- data

  if (!is.null(cdec_code)) {
    data_filtered <- data_filtered|> dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered|> dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered|> dplyr::filter(aq_location_id %in% .env$aq_location_id)
  }

  # Get all identifiers for entered stations
  Identifiers <- data_filtered$aq_location_id

  # Use purrr to loop through each identifier and get time series descriptions
  json_ts_params_df <- purrr::map_df(Identifiers, function(id) {
    cli::cli_alert_info("Fetching parameters for station: {id}")

    ts_params <- timeseries$getTimeSeriesDescriptions(locationIdentifier = id)

    # Debug: check what we got
    cli::cli_alert_info("Found {length(ts_params$Identifier)} time series")

    if (length(ts_params$Identifier) > 0) {
      data.frame(
        aq_location_id = ts_params$LocationIdentifier,
        parameter_id = ts_params$ParameterId,
        parameter_name = ts_params$Parameter,
        label = ts_params$Label,
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
    cli::cli_alert_warning("No data found")
    cli::cli_abort("No data retrieved")
  }

  cli::cli_alert_success("Retrieved {nrow(json_ts_params_df)} parameters from {length(unique(json_ts_params_df$aq_location_id))} station{?s}")

  # Join with location metadata and select columns
  df <- json_ts_params_df |>
    dplyr::left_join(data_filtered, by = "aq_location_id") |>
    dplyr::select(cdec_code, location_id, aq_station_name, aq_location_id, parameter_id, parameter_name, unit, label, updated_at)

  # Return df
  return(df)

}
