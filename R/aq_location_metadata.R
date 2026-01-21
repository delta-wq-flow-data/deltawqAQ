## Get location metadata, including latitude and longitude. we may want to add more things like county in the future.
aq_get_location_metadata = function(cdec_code = NULL, location_id = NULL, aq_location_id = NULL) {

  # run all locations metadata and get list of all stations
  data <- aq_get_location_list()

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- data %>%
    {if (!is.null(cdec_code)) filter(., cdec_code %in% cdec_code) else .} %>%
    {if (!is.null(location_id)) filter(., location_id %in% location_id) else .} %>%
    {if (!is.null(aq_location_id)) filter(., aq_location_id %in% aq_location_id) else .}

  # Return df
  return(data_filtered)

}


### Function to get metadata for all locations
aq_get_location_list  <- function() {

  # Function to connect to aquarius
  aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
             username = Sys.getenv("AQTS_USERNAME"),
             password = Sys.getenv("AQTS_PASSWORD"))

  # Ensure disconnect on exit
  on.exit(aq_disconnect())

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
  ) %>%
    filter(grepl("_", aq_location_id)) %>%
    separate(col = aq_location_id, into = c("location_id", "cdec_code"), sep = "_", remove = FALSE) %>%
    separate(col = aq_station_name, into = c("cdec", "location_name"), sep = " - ", remove = FALSE ) %>%
    mutate(cdec_code = toupper(substr(cdec_code, 1, 3)))

  # Get latitude and longitude for stations
  json_location_data <- lapply(df$aq_location_id, timeseries$getLocationData)

  # Pull out relevant information
  location_df <- purrr::map_df(
    json_location_data,
    ~ data.frame(
      aq_location_id = .x$Identifier,
      latitude = .x$Latitude,
      longitude = .x$Longitude)) %>%
    left_join(df) %>%
    select(cdec_code, location_id, location_name, latitude, longitude, aq_location_id, aq_station_name, aq_unique_id, updated_at)

  # Return data frame
  return(location_df)

}
