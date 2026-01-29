## code to prepare `aq_all_locations` dataset goes here

aq_all_locations <- aq_get_location_list()
usethis::use_data(aq_all_locations, overwrite=TRUE)

