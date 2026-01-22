#' @title Get Location Metadata
#' @description `aq_get_location_metadata` obtains metadata for a selection of stations.
#' @details This function obtains metadata for all the locations, then filters to stations of interest.
#' @param cdec_code three-letter code for station from \href{cdec.ca.gov}{CDEC}; one option for querying
#' @param location_id numeric id for station; one option for querying
#' @param aq_location_id location id, as displayed in Aquarius database (combo of location_id and cdec_code); one option for querying
#' @returns data frame of filtered stations and station metadata in Aquarius database
#' @examples
#' SJW_metadata <- aq_get_location_metadata(cdec_code = "SJW")
#' aq_get_location_metadata(location_id = "11447903")
#'
#'
aq_get_location_metadata = function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL) {

  # run all locations metadata and get list of all stations
  data <- aq_get_location_list()

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

  # Return df
  return(data_filtered)

}


#' @title Get Location Metadata for all Locations
#' @description `aq_get_location_list` obtains the full list of stations, including metadata
#' @details This function obtains metadata for all the locations and can be used with aq_get_location_metadata to filter information
#' to stations of interest.
#' @param connect TRUE/FALSE value indicates whether or not connection to Aquarius is needed. When this function is called within
#' other functions that have already connected to the database, this value should be FALSE to avoid errors.
#' @returns data frame of all stations and station metadata in Aquarius database
#' @examples
#' all_locations <- aq_get_location_list(connect = TRUE)

aq_get_location_list  <- function(connect = TRUE) {

  # Only connect if requested
  if (connect) {
    aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
               username = Sys.getenv("AQTS_USERNAME"),
               password = Sys.getenv("AQTS_PASSWORD"))
    on.exit(aq_disconnect())
  }

  # Location description call provides list of all locations if no parameters are called;
  # otherwise filters to selected stations(s)
  json_locations <- timeseries$getLocationsDescriptions()

  # Check if we got any data
  if (length(json_locations$Name) == 0) {
    cli::cli_alert_warning("No data found.")
    cli::cli_abort("No data retrieved.")
  }

  cli::cli_alert_success("Retrieved {length(json_locations$Name)} locations")

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
    dplyr::left_join(df) |>
    dplyr::select(cdec_code, location_id, location_name, latitude, longitude, aq_location_id, aq_station_name, aq_unique_id, updated_at)

  # Return data frame
  return(location_df)

}
