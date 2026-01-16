## Get location metadata, including latitude and longitude. we may want to add more things like county in the future.
aq_get_location_metadata = function(cdec_code=NULL, location_id=NULL, aq_location_id=NULL) {

  # run all locations metadata and get list of all stations
  data <- aq_get_location_list()

  # Looks for either cdec code, location_id, or aq_location_id within all stations
  data_filtered <- data %>%
    {if (!is.null(cdec_code)) filter(., cdec_code %in% !!cdec_code) else .} %>%
    {if (!is.null(location_id)) filter(., location_id %in% !!location_id) else .} %>%
    {if (!is.null(aq_location_id)) filter(., aq_location_id %in% !!aq_location_id) else .}

  # Return df
  return(data_filtered)

}
