# Extract all unique CDEC codes from the location list
all_cdec_codes <- as.list(unique(deltawqAQ::aq_all_locations$cdec_code))

# Get all parameters associated with locations
aq_parameter_location_crosswalk <- deltawqAQ::aq_get_location_parameters(cdec_code = all_cdec_codes) |>
  dplyr::left_join(deltawqAQ::aq_all_locations |> dplyr::select(cdec_code, aq_location_id)) |>
  dplyr::select(cdec_code, everything())

# Overwrite
usethis::use_data(aq_parameter_location_crosswalk, overwrite = TRUE)

