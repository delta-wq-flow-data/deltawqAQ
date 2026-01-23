#' @title Get Aquarius Timeseries
#'
#' @description `aq_get_ts` retrieves time series data from an Aquarius database and optionally downloads and writes the timeseries.
#'
#' @details This function retrieves timeseries data for a single location and a single parameter.
#' Exactly one value of either `cdec_code`, `location_id`, or `aq_location_id` should be provided and identified by the location identifier type.
#' The downloaded file can then be saved in your chosen filepath.
#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id Numeric identifier for location; one option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one option for querying
#' @param parameter Parameter name
#' @param query_from Start datetime for query
#' @param query_to End datetime for query
#' @param write Logical; whether to write the output timeseries
#' @param output Folder path for output timeseries if `write` is `TRUE`.
#'
#' @return A data frame containing the time series values and associated metadata.
#' @examples
#' \dontrun{
#'ts_data <- aq_get_ts(
#'  cdec_code = "SJW",
#'  parameter = "Water Temp",
#'  query_from = "2025-12-01T00:00:00Z",
#'  query_to = "2026-01-01T00:00:00Z")
#'ts_data <- aq_get_ts(
#'  location_id = "11447903",
#'  parameter = "Sp Cond",
#'  query_from = "2025-12-01T00:00:00Z",
#'  query_to = lubridate::now(),
#'  write = TRUE,
#'  output = here::here()
#'  )
#'  }
#'
#' @export

aq_get_ts <- function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL,
                      parameter, query_from, query_to,
                      write = FALSE, output = NULL) {

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
    data_filtered <- data_filtered |>  dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(aq_location_id %in% .env$aq_location_id)
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

  ts_data <- df |>
    dplyr::left_join(data_filtered, by = "aq_location_id") |>
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
    output_file <- if (!is.null(output)) {
      here::here(output, paste0(station_code, "_", parameter, "_data.csv"))
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
#'
#' @description `aq_process_ts` Processes the timeseries into a clean data frame after Aquarius database retrieval.
#'
#' @details This function cleans up the JSON format data obtained by the Aquarius API request.
#' The function is called in the `get_ts` functions as an intermediate step.
#'
#' @param station_code Aquarius station/location code of interest
#' @param parameter Parameter name from Aquarius
#' @param query_from Start datetime for query
#' @param query_to End datetime for query
#'
#' @return A data frame with 7 columns that will be further modified in `get_ts` functions
aq_process_ts = function(station_code, parameter, query_from, query_to) {

  filtered_params <- aq_get_station_parameters(aq_location_id = station_code, connect = FALSE)

  # Filter to the specific parameter requested
  param_info <- filtered_params |>
    dplyr::filter(parameter_name == parameter)

  if (nrow(param_info) == 0) {
    cli::cli_abort("Parameter '{parameter}' not found for station {station_code}")
  }

  # Use the first label if multiple exist
  label <- param_info$label[1]

  cli::cli_alert_info("Found label: {label}")

  # Get the timeseries ID
  # AQUARIUS uses the format: Parameter.Label@LocationIdentifier
  timeseries_id <- paste0(parameter, ".", label, "@", station_code)
  cli::cli_alert_info(c(
    "Requesting time-series: {timeseries_id} ",
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
#'
#' @description `aq_get_ts_multi_station` retrieves and optionally downloads timeseries for one parameter at multiple locations
#'
#' @details This function retrieves timeseries data for multiple locations and a single parameter.
#' A list of values for either `cdec_code`, `location_id`, or `aq_location_id` should be provided and identified by the location identifier type.
#' The downloaded file can then be saved in your chosen filepath.
#'
#' @param cdec_code List of three-letter location codes matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id List of numeric identifiers for location; one option for querying
#' @param aq_location_id List of location identifiers as displayed in Aquarius database; one option for querying
#' @param parameter Parameter name
#' @param query_from Start datetime for query
#' @param query_to End datetime for query
#' @param write Logical indicating whether to write the output timeseries
#' @param output Folder path for output timeseries if `write` is TRUE
#'
#' @return A data frame of the combined time series for all stations
#' If `write` is TRUE, individual files will be written for each station.
#'
#' @examples
#' \dontrun{
#' multi_sta_ts <- aq_get_ts_multi_station(
#'   cdec_code = c("SJW", "MDM", "GSS"),
#'   parameter = "Turbidity, Form Neph",
#'   query_from = "2025-12-01T00:00:00Z",
#'   query_to = "2026-01-01T00:00:00Z")
#' multi_sta_ts <- aq_get_ts_multi_station(
#'   location_id = c("11447903", "11447905", "11447890"),
#'   parameter = "Sp Cond",
#'   query_from = "2025-12-01T00:00:00Z",
#'   query_to = lubridate::now(),
#'   write = TRUE,
#'   output = here::here()
#'   )
#'   }
#'
#'@export
aq_get_ts_multi_station <- function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL,
                                    parameter, query_from, query_to,
                                    write = FALSE, output = NULL) {

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
    data_filtered <- data_filtered |> dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(aq_location_id %in% .env$aq_location_id)
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
      ts_data <- df |>
        dplyr::left_join(data_filtered, by = "aq_location_id") |>
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
        station_name <- data_filtered |>
          dplyr::filter(aq_location_id == station) |>
          dplyr::pull(cdec_code)

        output_file <- if (!is.null(output)) {
          here::here(output, paste0(station_name, "_", parameter, "_data.csv"))
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
#'
#' @description `aq_get_ts_multi_param` retrieves and optionally downloads timeseries for multiple parameters at one location
#'
#' @details This function retrieves timeseries data for multiple parameters and a single location.
#' A single value for either `cdec_code`, `location_id`, or `aq_location_id` should be provided and identified by the location identifier type.
#' A list of parameters should be provided.
#' The downloaded file can then be saved in your chosen filepath.
#'
#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id Numeric identifier for location; one option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one option for querying
#' @param parameter List of parameter names
#' @param query_from Start datetime for query
#' @param query_to End datetime for query
#' @param write Logical indicating whether to write the output timeseries
#' @param output Folder path for output timeseries if `write` is TRUE
#'
#' @return A data frame of the combined time series for all parameters
#' If `write` is TRUE, individual files will be written for each parameter.
#'
#' @examples
#' \dontrun{
#' multi_param_ts <- aq_get_ts_multi_param(
#'   cdec_code = "SJW",
#'   parameter = c("Turbidity, Form Neph", "Water Temp", "Sp Cond"),
#'   query_from = "2025-12-01T00:00:00Z",
#'   query_to = "2026-01-01T00:00:00Z")
#' multi_param_ts <- aq_get_ts_multi_param(
#'   location_id = "11447903",
#'   parameter = c("Sp Cond", "Water Temp", "CHL RFU"),
#'   query_from = "2025-12-01T00:00:00Z",
#'   query_to = lubridate::now(),
#'   write = TRUE,
#'   output = here::here()
#'   )
#' }
#'
#' @export
aq_get_ts_multi_param <- function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL,
                                  parameter, query_from, query_to,
                                  write = FALSE, output = NULL){
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
    data_filtered <- data_filtered |> dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(aq_location_id %in% .env$aq_location_id)
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
  cli::cli_alert_info("Processing {length(parameter)} parameter{?s} for station: {station_code}")

  # Track successes and failures
  successful_params <- character()
  failed_params <- character()

  # Process each parameter and collect results
  all_ts_data <- purrr::map_df(parameter, function(param) {
    tryCatch({
      cli::cli_alert_info("Processing parameter: {param}")

      # Get time series data
      df <- aq_process_ts(station_code, param, query_from, query_to)

      # Join with location metadata
      ts_data <- df |>
        dplyr::left_join(data_filtered, by = "aq_location_id") |>
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

        output_file <- if (!is.null(output)) {
          here::here(output, paste0(station_name, "_", param, "_data.csv"))
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
