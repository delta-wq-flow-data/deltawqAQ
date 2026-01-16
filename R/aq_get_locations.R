### Function to get location metadata
aq_get_location_metadata = function(locationName=NULL, locationIdentifier=NULL) {

  # Location description call provides list of all locations if no parameters are called;
  # otherwise filters to selected stations(s)
  json_locations <- timeseries$getLocationsDescriptions()

  # Check if we got any data
  if (length(json_locations$Name) == 0) {
    cat("No data found.\n")
    timeseries$disconnect()
    stop("No data retrieved. Exiting.")
  }

  cat("Retrieved", length(json_locations$Name), "data points\n\n")

  # Create data frame for useful location information
  df <- data.frame(
    aquarius_id = json_locations$Identifier,
    aquarius_name = json_locations$Name,
    aquarius_unique_id = json_locations$UniqueId,
    updated_at = json_locations$LastModified,
    stringsAsFactors = FALSE
  ) %>%
    filter(grepl("_", aquarius_id)) %>%
    separate(col = aquarius_id, into = c("location_id", "cdec_code"), sep = "_", remove = FALSE) %>%
    separate(col = aquarius_name, into = c("cdec", "location_name"), sep = " - ", remove = FALSE ) %>%
    mutate(cdec_code = toupper(substr(cdec_code, 1, 3)))

  # Get latitude and longitude for stations
  json_location_data <- lapply(df$aquarius_id, timeseries$getLocationData)

  # Pull out relevant information
  location_df <- purrr::map_df(
    json_location_data,
    ~ data.frame(
      aquarius_id = .x$Identifier,
      latitude = .x$Latitude,
      longitude = .x$Longitude)) %>%
    left_join(df) %>%
    select(cdec_code, location_id, location_name, latitude, longitude, aquarius_id, aquarius_name, aquarius_unique_id, updated_at)

  return(location_df)

}
