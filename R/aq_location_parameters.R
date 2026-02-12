#' @title Get location parameters
#' @description Retrieves a list of all the parameters associated with one or multiple locations.
#'
#' @details Retrieves the parameters (IDs, names) and specific time series identifiers associated with queried locations.
#'
#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id Numeric identifier for location; one option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one option for querying
#' other functions that have already connected to the database, this value should be `FALSE` to avoid errors.
#'
#' @return A data frame of parameters filtered to queried location(s)
#'
#' @examples
#' aq_get_location_parameters(cdec_code = "SJW")
#' @family Retrieve metadata
#' @export
aq_get_location_parameters = function(cdec_code=NULL, location_id=NULL, aq_location_id=NULL) {

  # Check connection
  aq_ensure_connection()

  # Looks for either cdec code, location_id, or aq_location_id within all locations

  if (!is.null(cdec_code)) {
    data_filtered <- deltawqAQ::aq_all_locations|>
      dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations|>
      dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations|>
      dplyr::filter(aq_location_id %in% .env$aq_location_id)
  }

  # Get all identifiers for entered locations
  Identifiers <- data_filtered$aq_location_id

  # Use purrr to loop through each identifier and get time series descriptions
  json_ts_params_df <- purrr::map_df(Identifiers, function(id) {

      resp <- aq_request() |>
        httr2::req_url_path_append("GetTimeSeriesDescriptionList") |>
        httr2::req_url_query(LocationIdentifier = id, Publish = "true") |>
        httr2::req_error(is_error = ~ FALSE) |> # prevent the request from error when 400 is returned
        httr2::req_perform()

      body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      ts_params <- body$TimeSeriesDescriptions

      if (is.null(ts_params) || length(ts_params$Identifier) == 0) {
        return(NULL)
      }

    if (length(ts_params$Identifier) > 0) {
      ts_params %>%
        dplyr::select(aq_location_id = LocationIdentifier,
                      parameter_id = ParameterId,
                      parameter_name = Parameter,
                      label = Label,
                      unit = Unit,
                      ts_id = Identifier,
                      ts_unique_id = UniqueId)
    } else {
      NULL  # map_df will skip NULL results
    }
  })

  # Combine with additional location information
  df <- json_ts_params_df %>%
    dplyr::left_join(data_filtered, by = "aq_location_id") |>
    dplyr::select(cdec_code, location_id, aq_location_name, aq_location_id, parameter_name, unit, label, updated_at, ts_unique_id)
  return(df)

}
