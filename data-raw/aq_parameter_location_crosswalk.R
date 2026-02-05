# Extract all unique CDEC codes from the location list
all_cdec_codes <- as.list(unique(aq_all_locations$cdec_code))

# Get all parameters associated with locations
aq_parameter_location_crosswalk <- deltawqAQ::aq_get_location_parameters(cdec_code = all_cdec_codes)

# Overwrite
usethis::use_data(aq_parameter_location_crosswalk, overwrite = TRUE)

