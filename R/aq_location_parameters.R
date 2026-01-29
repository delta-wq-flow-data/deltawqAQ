#' @title Get Location Parameters
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
#' params_SJW <- aq_get_location_parameters(cdec_code = "SJW")
#' params_SJW_MDM_GES <- aq_get_location_parameters(cdec_code = c("SJW", "MDM", "GES"))
#' param_info <- aq_get_location_parameters(location_id = c("11447903", "11447905", "11447890"))
#'
#' @export
aq_get_location_parameters = function(cdec_code=NULL, location_id=NULL, aq_location_id=NULL) {

  # Check connection
  aq_ensure_connection()

  # Looks for either cdec code, location_id, or aq_location_id within all locations

  if (!is.null(cdec_code)) {
    data_filtered <- deltawqAQ::aq_all_locations|> dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations|> dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- deltawqAQ::aq_all_locations|> dplyr::filter(aq_location_id %in% .env$aq_location_id)
  }

  # Get all identifiers for entered locations
  Identifiers <- data_filtered$aq_location_id

  # Use purrr to loop through each identifier and get time series descriptions
  json_ts_params_df <- purrr::map_df(Identifiers, function(id) {

    ts_params <- timeseries$getTimeSeriesDescriptions(locationIdentifier = id)

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

  cli::cli_alert_success("Retrieved {nrow(json_ts_params_df)} parameters from {length(unique(json_ts_params_df$aq_location_id))} location{?s}")

  # Join with location metadata and select columns
  df <- json_ts_params_df |>
    dplyr::left_join(data_filtered, by = "aq_location_id") |>
    # implement the next row once we have all working time series set up
    # dplyr::filter(grepl(".working", label, fixed = TRUE)) |>
    dplyr::select(cdec_code, location_id, aq_location_name, aq_location_id, parameter_id, parameter_name, unit, label, updated_at)

  # Return df
  return(df)

}
