#' @title Get Location Metadata

#' @description `aq_get_location_metadata` retrieves metadata from Aquarius database for a selection of locations.

#' @details This function retrieves metadata for all the locations, then filters to locations of interest.

#' @param cdec_code Three-letter location code matching identifiers from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id Numeric identifier for location; one option for querying
#' @param aq_location_id Location identifier as displayed in Aquarius database; one option for querying

#' @return A data frame of filtered stations and station metadata in Aquarius database

#' @examples
#' \dontrun{
#' SJW_metadata <- aq_get_location_metadata(cdec_code = "SJW")
#' aq_get_location_metadata(location_id = "11447903")
#' }

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



#' @title Get Location Metadata for all Locations

#' @description `aq_get_location_list` retrieves the full list of stations, including metadata

#' @details This function retrieves metadata for all the locations and can be used with aq_get_location_metadata to filter information
#' to stations of interest.

#' @return A data frame of all stations and station metadata in Aquarius database

#' @examples
#' \dontrun{
#' all_locations <- aq_get_location_list(connect = TRUE)
#' }

#' @export
aq_get_location_list  <- function() {

  # Check connection
  aq_ensure_connection()

  # Location description call provides list of all locations if no parameters are called;
  # otherwise filters to selected stations(s)
  json_locations <- timeseries$getLocationsDescriptions()

  # Check if we got any data
  if (length(json_locations$Name) == 0) {
    cli::cli_alert_warning("No data found.")
    cli::cli_abort("No data retrieved.")
  }

  # Create data frame for useful location information
  df <- data.frame(
    aq_location_id = json_locations$Identifier,
    aq_station_name = json_locations$Name,
    aq_unique_id = json_locations$UniqueId,
    updated_at = json_locations$LastModified,
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(grepl("_", aq_location_id)) |>
    tidyr::separate(col = aq_location_id, into = c("location_id", "cdec_code"), sep = "_", remove = FALSE) |>
    tidyr::separate(col = aq_station_name, into = c("cdec", "location_name"), sep = " - ", remove = FALSE ) |>
    dplyr::mutate(cdec_code = toupper(substr(cdec_code, 1, 3)))

  # Get latitude and longitude for stations
  json_location_data <- lapply(df$aq_location_id, timeseries$getLocationData)

  # Pull out relevant information
  location_df <- purrr::map_df(
    json_location_data,
    ~ data.frame(
      aq_location_id = .x$Identifier,
      latitude = .x$Latitude,
      longitude = .x$Longitude)) |>
    dplyr::left_join(df, by = "aq_location_id") |>
    dplyr::select(cdec_code, location_id, location_name, latitude, longitude, aq_location_id, aq_station_name, aq_unique_id, updated_at)

  # Return data frame
  return(location_df)

}
