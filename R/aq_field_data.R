#' @title Get field visit readings
#' @description Retrieves field visit readings from one or multiple locations
#'
#' @details Retrieves all discrete field readings from one or multiple locations for different types of site visits.
#'
#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one mutually exclusive option for querying
#' @param location_id Numeric identifier for location; one mutually exclusive option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one mutually exclusive option for querying
#' other functions that have already connected to the database, this value should be `FALSE` to avoid errors.
#'
#' @return Field values
#'
#' @examples
#' aq_get_field_readings(cdec_code = "GSS")
#' @family Field visits
#' @export

aq_get_field_readings = function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL) {

  aq_ensure_connection()

  # Looks for either cdec code, location_id, or aq_location_id within all locations
  if (!is.null(cdec_code)) {
    data_filtered <- deltawqAQ::aq_all_locations|>
      dplyr::filter(cdec_code %in% .env$cdec_code)
    not_found <- cdec_code[!cdec_code %in% data_filtered$cdec_code]
    if (length(not_found) > 0) {
      cli::cli_alert_warning("The following cdec_codes were not found in deltawqAQ::aq_all_locations: {paste(not_found, collapse = ', ')}")
    }
  }

  if (!is.null(location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations|>
      dplyr::filter(location_id %in% .env$location_id)
    not_found <- location_id[!location_id %in% data_filtered$location_id]
    if (length(not_found) > 0) {
      cli::cli_alert_warning("The following location_id were not found in deltawqAQ::aq_all_locations: {paste(not_found, collapse = ', ')}")
    }
  }

  if (!is.null(aq_location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations|>
      dplyr::filter(aq_location_id %in% .env$aq_location_id)
    not_found <- aq_location_id[!aq_location_id %in% data_filtered$aq_location_id]
    if (length(not_found) > 0) {
      cli::cli_alert_warning("The following aq_location_ids were not found in deltawqAQ::aq_all_locations: {paste(not_found, collapse = ', ')}")
    }
  }

  # Check if location was found
  if (nrow(data_filtered) == 0) {
    cli::cli_alert_warning("No locations matched the specified criteria. Check deltawqAQ::aq_all_locations for location names.")
    return(invisible(NULL))
  }

  # Get all identifiers for entered locations
  identifiers <- data_filtered$aq_location_id

  # Track successes and failures
  successful_locations <- character()
  failed_locations <- character()

  # Use purrr to loop through each identifier to handle multiple locations
  json_ts_readings <- purrr::map_df(identifiers, function(id) {
     tryCatch({
      resp <- aq_request() |>
        httr2::req_url_path_append("GetFieldVisitReadingsByLocation") |>
        httr2::req_url_query(LocationIdentifier = id) |>
        httr2::req_error(is_error = ~ FALSE) |> # prevent the request from error when 400 is returned
        httr2::req_perform()

      body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      if (is.null(body$FieldVisitReadings) || !is.data.frame(body$FieldVisitReadings) || nrow(body$FieldVisitReadings) == 0) {
        cli::cli_alert_warning("No readings found for location {id}.")
        failed_locations <<- c(failed_locations, id)
        return(invisible(NULL))
      }

      field_readings <- jsonlite::flatten(body$FieldVisitReadings)|>
        dplyr::select(datetime = Time,
                      parameter_id = ParameterId,
                      parameter_name = Parameter,
                      value = Value.Numeric,
                      unit = Value.Unit,
                      monitoring_method = MonitoringMethod,
                      field_visit_id = FieldVisitIdentifier) |>
        dplyr::mutate(aq_location_id = id,
                      datetime = lubridate::ymd_hms(datetime, tz = "Etc/GMT+8"))

      successful_locations <<- c(successful_locations, id)
      return(field_readings)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to process location {id}: {conditionMessage(e)}")
      failed_locations <<- c(failed_locations, id)
      return(invisible(NULL))
    })
  })

  # Report successes and failures
  if (length(failed_locations) > 0) {
    cli::cli_alert_warning("The following locations failed: {paste(failed_locations, collapse = ', ')}")
  }
  if (length(successful_locations) == 0) {
    cli::cli_alert_danger("No locations returned data.")
    return(invisible(NULL))
  }

  # Combine with additional location information for df product
  df <- json_ts_readings |>
    dplyr::left_join(data_filtered, by = "aq_location_id") |>
    dplyr::select(cdec_code, aq_location_name, aq_location_id, datetime, parameter_name, value, unit, monitoring_method, field_visit_id)
  return(df)

}
