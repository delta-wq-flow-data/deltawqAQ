# Functions for getting location metadata, including list of all locations or filtering locations

#' @title Get location metadata

#' @description Retrieves metadata from Aquarius database for a selection of locations.

#' @details This function retrieves metadata for all the locations, then filters to locations of interest.

#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id Numeric identifier for location; one option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one option for querying
#' @family Retrieve metadata
#' @return A data frame of 9 columns, with each row displaying metadata for one location, based on queried locations.
#'
#' @examples
#' SJW_metadata <- aq_get_location_metadata(cdec_code = "SJW")
#' aq_get_location_metadata(location_id = "11447903")


#' @export
aq_get_location_metadata = function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL) {

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- deltawqAQ::aq_all_locations

  if (!is.null(cdec_code)) {
    data_filtered <- data_filtered |> dplyr::filter(cdec_code %in% .env$cdec_code)
  }
  if (!is.null(location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(location_id %in% .env$location_id)
  }
  if (!is.null(aq_location_id)) {
    data_filtered <- data_filtered |> dplyr::filter(aq_location_id %in% .env$aq_location_id)
  }

  # Return df
  return(data_filtered)

}



#' @title Get location metadata for all locations

#' @description Retrieves the full list of locations, including location metadata

#' @details This function retrieves metadata for all the locations and can be used with aq_get_location_metadata to filter information
#' to locations of interest.

#' @return A data frame of all locations and location metadata in Aquarius database

#' @examples
#' all_locations <- aq_get_location_list()

#' @family Retrieve metadata
#' @export
aq_get_location_list  <- function() {

  aq_ensure_connection()

  resp <- aq_request() |>
    httr2::req_url_path_append("GetLocationDescriptionList") |>
    httr2::req_url_query() |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

  body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  json_locations <- body$LocationDescriptions

  # Check if we got any data
  if (length(json_locations$Name) == 0) {
    cli::cli_alert_warning("No data found.")
    cli::cli_abort("No data retrieved.")
  }

  df <- json_locations |>
    dplyr::select(
      aq_location_id = Identifier,
      aq_location_name = Name,
      aq_unique_id = UniqueId,
      updated_at = LastModified) |>
    dplyr::filter(grepl("_", aq_location_id)) |>
    tidyr::separate(col = aq_location_id, into = c("location_id", "cdec_code"), sep = "_", remove = FALSE) |>
    tidyr::separate(col = aq_location_name, into = c("cdec", "location_name"), sep = " - ", remove = FALSE ) |>
    dplyr::mutate(cdec = dplyr::if_else(grepl("CM", cdec), gsub("^CM", "C", cdec), cdec),
                  cdec_code = toupper(substr(cdec, 1, 3)))

  json_location_data <- purrr::map_df(df$aq_location_id, function(id)  {
    # get lat/lon
    resp_latlon <- aq_request() |>
    httr2::req_url_path_append("GetLocationData") |>
    httr2::req_url_query(LocationIdentifier = id) |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_perform()

    body_latlon <- httr2::resp_body_json(resp_latlon, simplifyVector = TRUE)
    data.frame(
      aq_location_id = body_latlon$Identifier,
      latitude       = body_latlon$Latitude,
      longitude      = body_latlon$Longitude
    )

  }) |>
    # combine with rest of location information
    dplyr::left_join(df, by = "aq_location_id") |>
    dplyr::select(cdec_code, location_id, location_name, latitude, longitude, aq_location_id, aq_location_name, updated_at)

  # Return data frame
  return(json_location_data)

}
