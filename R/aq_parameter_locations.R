#' @title Get locations associated with a parameter
#'
#' @description Provides a list of locations associated with a queried parameter.
#'
#' @details Provides a list of locations and dates associated with a queried parameter.
#' Exactly one parameter name should be provided.
#'
#' @param param Parameter of interest

#' @return A data frame containing columns for location information and start and end times of timeseries
#'
#' @examples
#' aq_get_parameter_locations("Water Temp")
#'
#' @export
aq_get_parameter_locations <- function(param) {

  aq_ensure_connection()

  resp <- aq_request() |>
    httr2::req_url_path_append("GetTimeSeriesDescriptionList") |>
    httr2::req_url_query(Publish = "true") |>
    httr2::req_perform()

  json_ts_des <- httr2::resp_body_json(resp, simplifyVector = TRUE)$TimeSeriesDescriptions |>
    dplyr::select(aq_location_id = LocationIdentifier, parameter_name = Parameter, start_datetime = CorrectedStartTime,
                  end_datetime = CorrectedEndTime) |>
    dplyr::right_join(deltawqAQ::aq_all_locations |>
                        dplyr::select(aq_location_id, cdec_code, location_id, location_name), by = "aq_location_id") |>
    dplyr::filter(!is.na(start_datetime),
                  !is.na(end_datetime),
                  parameter_name == param)

  # Filter aq_all_parameter_locations data object to the parameter selected and add additional details
  params_filtered <- deltawqAQ::aq_all_parameters |>
    dplyr::filter(parameter_name == param) |>
    # add start and end date times
    dplyr::left_join(json_ts_des) |>
    # final selection of display columns
    dplyr::select(parameter_name,
                  cdec_code,
                  location_id,
                  location_name,
                  aq_location_id,
                  start_datetime,
                  end_datetime)

  # Return df
  return(params_filtered)

}
