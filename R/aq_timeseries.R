#' @title Get Aquarius Timeseries
#' @description `aq_get_ts` obtains and optionally downloads timeseries.
#' @details This function obtains timeseries data for one station and one parameter.
#' The downloaded file can then be saved in your chosen filepath.
#' @param cdec_code three-letter code for station from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id numeric id for station; one option for querying
#' @param aq_location_id location id, as displayed in Aquarius database (combo of location_id and cdec_code); one option for querying
#' @param parameter parameter name
#' @param query_from start datetime for query
#' @param query_to end datetime for query
#' @param write TRUE/FALSE for whether to write the output timeseries
#' @param output_path folder path for output timeseries if write == TRUE
#' @returns returns a data frame of the time series as well as location codes

aq_get_ts <- function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL,
                      parameter, query_from, query_to,
                      write = FALSE, output_path = NULL) {

  # Connect once at the start
  aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
             username = Sys.getenv("AQTS_USERNAME"),
             password = Sys.getenv("AQTS_PASSWORD"))

  on.exit(aq_disconnect())

  # Get station info for all locations
  data <- aq_get_location_list(connect = FALSE)

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- data

  if (!is.null(cdec_code)) {
    data_filtered <- data_filtered %>% filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered %>% filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered %>% filter(aq_location_id %in% .env$aq_location_id)
  }

  # Right after data_filtered
  cli::cli_alert_info("After filtering: {nrow(data_filtered)} station{?s}")
  cli::cli_alert_info("Station codes: {paste(data_filtered$aq_location_id, collapse = ', ')}")

  # Check if any stations were found
  if (nrow(data_filtered) == 0) {
    cli::cli_alert_warning("No stations matched the specified criteria")
    return(NULL)
  }

  # Get identifier for the filtered station (should be just one now)
  station_code <- data_filtered$aq_location_id

  # Function to obtain time series data
  df <- aq_process_ts(station_code, parameter, query_from, query_to)

  ts_data <- df %>%
    dplyr::left_join(data_filtered, by = "aq_location_id") %>%
    dplyr::select(datetime,
                  cdec_code,
                  location_id,
                  aq_station_name,
                  aq_location_id,
                  parameter_name,
                  value,
                  unit,
                  approval)

  # Write to CSV if requested
  if (isTRUE(write)) {
    output_file <- if (!is.null(output_path)) {
      here::here(output_path, paste0(station_code, "_", parameter, "_data.csv"))
    } else {
      here::here(paste0(station_code, "_", parameter, "_data.csv"))
    }

    tryCatch({
      write.csv(ts_data, output_file, row.names = FALSE)
      cli::cli_alert_success("Data written to: {output_file}")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to write file: {e$message}")
    })
    return(invisible(ts_data))
  }

  # Return normally if not writing
  return(ts_data)
}

#' @title Process Aquarius Timeseries
#' @description `aq_process_ts` processes the timeseries once obtained from Aquarius.
#' @details This function cleans up the cleans up the json format data obtained by the Aquarius API request.
#' The function is called in the get_timeseries functions as an intermediate step.
#' @param station_code Aquarius station code of interest
#' @param parameter parameter name from Aquarius
#' @param query_from start datetime for query
#' @param query_to end datetime for query
#' @returns returns a data frame with timeseries information that will be further modified in get_ts functions

aq_process_ts = function(station_code, parameter, query_from, query_to) {

  filtered_params <- aq_get_station_parameters(aq_location_id = station_code, connect = FALSE)

  # Filter to the specific parameter requested
  param_info <- filtered_params %>%
    filter(parameter_name == parameter)

  if (nrow(param_info) == 0) {
    cli::cli_abort("Parameter '{parameter}' not found for station {station_code}")
  }

  # Use the first label if multiple exist
  label <- param_info$label[1]


  # Get the timeseries ID
  # AQUARIUS uses the format: Parameter.Label@LocationIdentifier
  timeseries_id <- paste0(parameter, ".", label, "@", station_code)
  cli::cli_alert_info(c(
    "Requesting time-series: {timeseries_id}",
    "Time range: {query_from} to {query_to}"
  ))

  ## Get the JSON form of the data
  cli::cli_alert_info("Fetching data...")
  json_data <- timeseries$getTimeSeriesData(
    timeSeriesIds = timeseries_id,
    queryFrom = query_from,
    queryTo = query_to
  )

  ## Process data
  # Extract points from the JSON response
  points <- json_data$Points

  # Check if we got any data
  if (nrow(points) == 0) {
    cli::cli_alert_warning("No data found for the specified time range")
    cli::cli_abort("No data retrieved.")
  }

  cli::cli_alert_success("Retrieved {nrow(points)} data points")

  # Convert timestamps to POSIXct format for easier handling
  # parseIso8601 automatically handles any timezone offset in the ISO 8601 string
  points$DateTime <- sapply(points$Timestamp, timeseries$parseIso8601)
  points$DateTime <- as.POSIXct(points$DateTime, origin = "1970-01-01")


  # get timeseries info
  ts_info <- json_data$TimeSeries

  # Create clean data frame
  df <- data.frame(
    datetime = points$DateTime,
    value = points$NumericValue1,
    aq_location_id = station_code[1],
    parameter_name = ts_info$Parameter[1],
    label = ts_info$Label[1],
    unit = ts_info$Unit[1],
    approval = points$ApprovalName1,
    stringsAsFactors = FALSE
  )

  return(df)
}

#' @title Get Aquarius Timeseries for Multiple Stations
#' @description `aq_get_ts_multi_station` obtains and optionally downloads timeseries for one parameter at multiple stations.
#' @details This function obtains timeseries data for one station and multiple parameters.
#' The downloaded file can then be saved in your chosen filepath.
#' @param cdec_code list of three-letter codes for station from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id list of numeric ids for station; one option for querying
#' @param aq_location_id list of location ids, as displayed in Aquarius database (combo of location_id and cdec_code); one option for querying
#' @param parameters parameter name
#' @param query_from start datetime for query
#' @param query_to end datetime for query
#' @param write TRUE/FALSE for whether to write the output timeseries
#' @param output_path folder path for output timeseries if write == TRUE
#' @returns returns a data frame of the combined time series
#'
aq_get_ts_multi_station <- function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL,
                                    parameter, query_from, query_to,
                                    write = FALSE, output_path = NULL) {

  # Connect once at the start
  aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
             username = Sys.getenv("AQTS_USERNAME"),
             password = Sys.getenv("AQTS_PASSWORD"))

  on.exit(aq_disconnect())

  # Get station info for all locations
  data <- aq_get_location_list(connect = FALSE)

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- data

  if (!is.null(cdec_code)) {
    data_filtered <- data_filtered %>% filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered %>% filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered %>% filter(aq_location_id %in% .env$aq_location_id)
  }

  # Check if any stations were found
  if (nrow(data_filtered) == 0) {
    cli::cli_alert_warning("No stations matched the specified criteria")
    return(NULL)
  }

  # Get all station codes
  station_codes <- data_filtered$aq_location_id

  # Show progress message
  cli::cli_alert_info("Processing {length(station_codes)} station{?s}...")

  # Track successful and failed attempts to get data
  successful_stations <- character()
  failed_stations <- character()

  # Process each station and collect results
  all_ts_data <- purrr::map_df(station_codes, function(station) {
    tryCatch({
      cli::cli_alert_info("Processing station: {station}")

      # Get time series data
      df <- aq_process_ts(station, parameter, query_from, query_to)

      # Join with location metadata
      ts_data <- df %>%
        dplyr::left_join(data_filtered, by = "aq_location_id") %>%
        dplyr::select(datetime,
                      cdec_code,
                      location_id,
                      aq_station_name,
                      aq_location_id,
                      parameter_name,
                      value,
                      unit,
                      approval)

      # Write individual files if requested
      if (isTRUE(write)) {
        # Get station identifier for filename
        station_name <- data_filtered %>%
          filter(aq_location_id == station) %>%
          pull(cdec_code)

        output_file <- if (!is.null(output_path)) {
          here::here(output_path, paste0(station_name, "_", parameter, "_data.csv"))
        } else {
          here::here(paste0(station_name, "_", parameter, "_data.csv"))
        }

        tryCatch({
          write.csv(ts_data, output_file, row.names = FALSE)
          cli::cli_alert_success("Data written to: {output_file}")
        }, error = function(e) {
          cli::cli_alert_danger("Failed to write file: {e$message}")
        })
      }

      successful_stations <<- c(successful_stations, station)
      return(ts_data)

    }, error = function(e) {
      cli::cli_alert_danger("Failed to process station {station}: {conditionMessage(e)}")
      failed_stations <<- c(failed_stations, station)
      return(NULL)
    })
  })

  # Summary message
  cli::cli_h2("Processing Summary")
  cli::cli_alert_success("Successfully processed: {length(successful_stations)} station{?s}")
  if (length(failed_stations) > 0) {
    cli::cli_alert_warning("Failed stations: {paste(failed_stations, collapse = ', ')}")
  }

  # Return data
  if (isTRUE(write)) {
    return(invisible(all_ts_data))
  } else {
    return(all_ts_data)
  }
}

#' @title Get Aquarius Timeseries for Multiple Parameters
#' @description `aq_get_ts_multi_params` obtains and optionally downloads timeseries for multiple parameters at one station.
#' @details This function obtains timeseries data for one station and multiple parameters.
#' The downloaded file can then be saved in your chosen filepath.
#' @param cdec_code three-letter code for station from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id numeric id for station; one option for querying
#' @param aq_location_id location id, as displayed in Aquarius database (combo of location_id and cdec_code); one option for querying
#' @param parameters list of parameter names to query
#' @param query_from start datetime for query
#' @param query_to end datetime for query
#' @param write TRUE/FALSE for whether to write the output timeseries
#' @param output_path folder path for output timeseries if write == TRUE
#' @returns returns a data frame of the combined time series
#'
aq_get_ts_multi_param <- function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL,
                                  parameters, query_from, query_to,
                                  write = FALSE, output_path = NULL){
  # Connect once at the start
  aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
             username = Sys.getenv("AQTS_USERNAME"),
             password = Sys.getenv("AQTS_PASSWORD"))

  on.exit(aq_disconnect())

  # Get station info for all locations
  data <- aq_get_location_list(connect = FALSE)

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- data

  if (!is.null(cdec_code)) {
    data_filtered <- data_filtered %>% filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered %>% filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered %>% filter(aq_location_id %in% .env$aq_location_id)
  }

  # Check if station was found
  if (nrow(data_filtered) == 0) {
    cli::cli_alert_warning("No stations matched the specified criteria")
    return(NULL)
  }

  # Should be only one station
  if (nrow(data_filtered) > 1) {
    cli::cli_alert_warning("Multiple stations found - using first match: {data_filtered$aq_location_id[1]}")
  }

  # Get station code
  station_code <- data_filtered$aq_location_id[1]

  # Show progress message
  cli::cli_alert_info("Processing {length(parameters)} parameter{?s} for station: {station_code}")

  # Track successes and failures
  successful_params <- character()
  failed_params <- character()

  # Process each parameter and collect results
  all_ts_data <- purrr::map_df(parameters, function(param) {
    tryCatch({
      cli::cli_alert_info("Processing parameter: {param}")

      # Get time series data
      df <- aq_process_ts(station_code, param, query_from, query_to)

      # Join with location metadata
      ts_data <- df %>%
        dplyr::left_join(data_filtered, by = "aq_location_id") %>%
        dplyr::select(datetime,
                      cdec_code,
                      location_id,
                      aq_station_name,
                      aq_location_id,
                      parameter_name,
                      value,
                      unit,
                      approval)

      # Write individual files if requested
      if (isTRUE(write)) {
        station_name <- data_filtered$cdec_code[1]

        output_file <- if (!is.null(output_path)) {
          here::here(output_path, paste0(station_name, "_", param, "_data.csv"))
        } else {
          here::here(paste0(station_name, "_", param, "_data.csv"))
        }

        tryCatch({
          write.csv(ts_data, output_file, row.names = FALSE)
          cli::cli_alert_success("Data written to: {output_file}")
        }, error = function(e) {
          cli::cli_alert_danger("Failed to write file: {e$message}")
        })
      }

      successful_params <<- c(successful_params, param)
      return(ts_data)

    }, error = function(e) {
      cli::cli_alert_danger("Failed to process parameter {param}: {conditionMessage(e)}")
      failed_params <<- c(failed_params, param)
      return(NULL)
    })
  })

  # Summary message
  cli::cli_h2("Processing Summary")
  cli::cli_alert_success("Successfully processed: {length(successful_params)} parameter{?s}")
  if (length(failed_params) > 0) {
    cli::cli_alert_warning("Failed parameters: {paste(failed_params, collapse = ', ')}")
  }

  # Return data
  if (isTRUE(write)) {
    return(invisible(all_ts_data))
  } else {
    return(all_ts_data)
  }


}
