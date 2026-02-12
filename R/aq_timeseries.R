# Functions to get time series

#' @title Get Aquarius time series
#'
#' @description Retrieves time series data from an Aquarius database and optionally downloads and writes the time series.
#'
#' @details Retrieves time series data for a single location and a single parameter.
#' Exactly one value of either `cdec_code`, `location_id`, or `aq_location_id` should be provided and identified by the location identifier type.
#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id Numeric identifier for location; one option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one option for querying
#' @param parameter Parameter name
#' @param query_from Start datetime for query; also accepts date
#' @param query_to End datetime for query; also accepts date

#' @return A data frame containing the time series values and associated metadata.
#'
#' @examples
#' \dontrun{
#'ts_data <- aq_get_ts(
#'  cdec_code = "SJW",
#'  parameter = "Water Temp",
#'  query_from = "2025-12-01 00:00:00",
#'  query_to = "2026-01-01 00:00:00")
#'ts_data <- aq_get_ts(
#'  location_id = "11447903",
#'  parameter = "Sp Cond",
#'  query_from = "2025-12-01 00:00:00",
#'  query_to = lubridate::now()
#'  )
#'  }
#' @family Retrieve time series
#' @export

aq_get_ts <- function(cdec_code = NULL,
                      parameter,
                      query_from, query_to,
                      location_id = NULL,
                      aq_location_id = NULL) {

  # Check connection
  aq_ensure_connection()

  # Looks for either cdec code, location_id, or aq_location_id within all locations

  if (!is.null(cdec_code)) {
    data_filtered <- deltawqAQ::aq_all_locations |>
      dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations |>
      dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations |>
      dplyr::filter(aq_location_id %in% .env$aq_location_id)
  }

  # Right after data_filtered
  cli::cli_alert_info("After filtering: {nrow(data_filtered)} location{?s}")
  cli::cli_alert_info("Location codes: {paste(data_filtered$aq_location_id, collapse = ', ')}")

  # Check if any locations were found
  if (nrow(data_filtered) == 0) {
    cli::cli_alert_warning("No locations matched the specified criteria")
    return(NULL)
  }

  # Get identifier for the filtered location (should be just one now)
  location_code <- data_filtered$aq_location_id

  # Function to obtain time series data
  df <- aq_process_ts(location_code, parameter, query_from, query_to)

  ts_data <- df |>
    dplyr::left_join(data_filtered, by = "aq_location_id") |>
    dplyr::select(datetime,
                  cdec_code,
                  location_id,
                  aq_location_name,
                  aq_location_id,
                  parameter_name,
                  value,
                  unit,
                  approval)

  return(ts_data)
}

#' @title Process Aquarius time series
#'
#' @description Processes the time series into a clean data frame after Aquarius database retrieval.
#'
#' @details Cleans up the JSON format data obtained by the Aquarius API request.
#' The function is called in the get_ts functions as an intermediate step.
#'
#' @param location_code Aquarius location/location code of interest
#' @param parameter Parameter name from Aquarius
#' @param query_from Start datetime for query; also accepts date
#' @param query_to End datetime for query; also accepts date
#' @family Retrieve time series
#' @return A data frame with 7 columns that will be further modified in get_ts functions
aq_process_ts = function(location_code, parameter, query_from, query_to) {

  filtered_params <- aq_get_location_parameters(aq_location_id = location_code)

  if (nrow(filtered_params) == 0) {
    cli::cli_abort("Parameter '{parameter}' not found for location {location_code}")
  }

  # Get the time series ID using the crosswalk
  timeseries_id <- deltawqAQ::aq_parameter_location_crosswalk |>
    dplyr::filter(parameter_name %in% parameter,
                  aq_location_id %in% location_code) |>
    dplyr::pull(ts_unique_id)

  ## Get the data
  resp <- aq_request() |>
    httr2::req_url_path_append("GetTimeSeriesData") |>
    httr2::req_url_query(TimeSeriesUniqueIds = timeseries_id,
                         queryFrom = query_from,
                         queryTo = query_to) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  resp_points <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  points <- resp_points$Points

  resp_ts <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  ts_info <- resp_ts$TimeSeries

  # Check if we got any data
  if (httr2::resp_is_error(resp) || !is.null(points$ResponseStatus)) {
    msg <- points$ResponseStatus$Message %||% paste("HTTP", httr2::resp_status(resp))
    cli::cli_abort(c(
      msg,
      "i" = "Check {.code deltawqAQ::aq_all_parameters} and {.code deltawqAQ::aq_all_locations}
      for valid parameter and location names"
    ))
  }

  cli::cli_alert_success("Retrieved {nrow(points)} data points")

  # Convert timestamps to work in ggplot
  points$Datetime <- lubridate::ymd_hms(points$Timestamp)

  # Create clean data frame
  df <- data.frame(
    datetime = points$Datetime,
    value = points$NumericValue1,
    aq_location_id = ts_info$LocationIdentifier[1],
    parameter_name = ts_info$Parameter[1],
    label = ts_info$Label[1],
    unit = ts_info$Unit[1],
    approval = points$ApprovalName1,
    stringsAsFactors = FALSE
  )

  return(df)
}

#' @title Get Aquarius time series for multiple locations
#'
#' @description Retrieves and optionally downloads time series for one parameter at multiple locations
#'
#' @details Retrieves time series data for one to multiple locations and a single parameter.
#' A list of values for either cdec_code, location_id, or aq_location_id should be provided and identified by the location identifier type.
#'
#' @param cdec_code List of three-letter location codes matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id List of numeric identifiers for location; one option for querying
#' @param aq_location_id List of location identifiers as displayed in Aquarius database; one option for querying
#' @param parameter Parameter name
#' @param query_from Start datetime for query; also accepts date
#' @param query_to End datetime for query; also accepts date
#'
#' @return A data frame of the combined time series for all locations
#'
#' @examples
#' \dontrun{
#' multi_sta_ts <- aq_get_ts_multi_location(
#'   cdec_code = c("SJW", "MDM", "GSS"),
#'   parameter = "Turbidity, Form Neph",
#'   query_from = "2025-12-01 00:00:00",
#'   query_to = "2026-01-01 00:00:00")
#' multi_sta_ts <- aq_get_ts_multi_location(
#'   location_id = c("11447903", "11447905", "11447890"),
#'   parameter = "Sp Cond",
#'   query_from = "2025-12-01 00:00:00",
#'   query_to = lubridate::now()
#'   )
#'   }
#' @family Retrieve time series
#' @export
aq_get_ts_multi_location <- function(cdec_code = NULL,
                                    parameter,
                                    query_from, query_to,
                                    location_id = NULL,
                                    aq_location_id = NULL) {

  # Check connection
  aq_ensure_connection()

  # Looks for either cdec code, location_id, or aq_location_id within all locations

  if (!is.null(cdec_code)) {
    data_filtered <- deltawqAQ::aq_all_locations |>
      dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations |>
      dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations |>
      dplyr::filter(aq_location_id %in% .env$aq_location_id)
  }

  # Check if any locations were found
  if (nrow(data_filtered) == 0) {
    cli::cli_alert_warning("No locations matched the specified criteria")
    return(NULL)
  }

  # Get all location codes
  location_codes <- data_filtered$aq_location_id

  # Show progress message
  cli::cli_alert_info("Processing {length(location_codes)} location{?s}...")

  # Track successful and failed attempts to get data
  successful_locations <- character()
  failed_locations <- character()

  # Process each location and collect results
  all_ts_data <- purrr::map_df(location_codes, function(location) {
    tryCatch({
      cli::cli_alert_info("Processing location: {location}")

      # Get time series data
      df <- aq_process_ts(location, parameter, query_from, query_to)

      # Join with location metadata
      ts_data <- df |>
        dplyr::left_join(data_filtered, by = "aq_location_id") |>
        dplyr::select(datetime,
                      cdec_code,
                      location_id,
                      aq_location_name,
                      aq_location_id,
                      parameter_name,
                      value,
                      unit,
                      approval)

      # Track which locations successful
      successful_locations <<- c(successful_locations, location)
      return(ts_data)

    }, error = function(e) {
      cli::cli_alert_danger("Failed to process location {location}: {conditionMessage(e)}")
      failed_locations <<- c(failed_locations, location)
      return(NULL)
    })
  })

  # Summary message
  cli::cli_h2("Processing Summary")
  cli::cli_alert_success("Successfully processed: {length(successful_locations)} location{?s}")
  if (length(failed_locations) > 0) {
    cli::cli_alert_warning("Failed locations: {paste(failed_locations, collapse = ', ')}")
  }

  # Return data
  return(all_ts_data)
}


#' @title Get Aquarius time series for multiple parameters
#'
#' @description Retrieves and optionally downloads time series for multiple parameters at one location
#'
#' @details Retrieves time series data for one to multiple parameters and a single location.
#' A single value for either cdec_code, location_id, or aq_location_id should be provided and identified by the location identifier type.
#' A list of parameters should be provided.
#'
#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id Numeric identifier for location; one option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one option for querying
#' @param parameter List of parameter names
#' @param query_from Start datetime for query; also accepts date
#' @param query_to End datetime for query; also accepts date
#'
#' @return A data frame of the combined time series for all parameters
#'
#' @examples
#' \dontrun{
#' multi_param_ts <- aq_get_ts_multi_param(
#'   cdec_code = "SJW",
#'   parameter = c("Turbidity, Form Neph", "Water Temp", "Sp Cond"),
#'   query_from = "2025-12-01 00:00:00",
#'   query_to = "2026-01-01 00:00:00")
#' multi_param_ts <- aq_get_ts_multi_param(
#'   location_id = "11447903",
#'   parameter = c("Sp Cond", "Water Temp", "CHL RFU"),
#'   query_from = "2025-12-01 00:00:00",
#'   query_to = lubridate::now()
#'   )
#' }
#' @family Retrieve time series
#' @export
aq_get_ts_multi_param <- function(cdec_code = NULL,
                                  parameter,
                                  query_from, query_to,
                                  location_id = NULL,
                                  aq_location_id = NULL){

   # Check connection
  aq_ensure_connection()

  # Load list of locations to filter from
  data <- deltawqAQ::aq_all_locations

  # Looks for either cdec code, location_id, or aq_location_id within all locations

  if (!is.null(cdec_code)) {
    data_filtered <- deltawqAQ::aq_all_locations |> dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations |> dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations |> dplyr::filter(aq_location_id %in% .env$aq_location_id)
  }

  # Check if location was found
  if (nrow(data_filtered) == 0) {
    cli::cli_alert_warning("No locations matched the specified criteria")
    return(NULL)
  }

  # Should be only one location
  if (nrow(data_filtered) > 1) {
    cli::cli_alert_warning("Multiple locations found - using first match: {data_filtered$aq_location_id[1]}")
  }

  # Get location code
  location_code <- data_filtered$aq_location_id[1]

  # Show progress message
  cli::cli_alert_info("Processing {length(parameter)} parameter{?s} for location: {location_code}")

  # Track successes and failures
  successful_params <- character()
  failed_params <- character()

  # Process each parameter and collect results
  all_ts_data <- purrr::map_df(parameter, function(param) {
    tryCatch({
      cli::cli_alert_info("Processing parameter: {param}")

      # Get time series data
      df <- aq_process_ts(location_code, param, query_from, query_to)

      # Join with location metadata
      ts_data <- df |>
        dplyr::left_join(data_filtered, by = "aq_location_id") |>
        dplyr::select(datetime,
                      cdec_code,
                      location_id,
                      aq_location_name,
                      aq_location_id,
                      parameter_name,
                      value,
                      unit,
                      approval)

      # Track which parameters successful
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
    return(all_ts_data)

}
