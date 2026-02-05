
# Call aq_get_location_list to get list. Write as data object.
aq_all_locations <- aq_get_location_list()
usethis::use_data(aq_all_locations, overwrite=TRUE)

