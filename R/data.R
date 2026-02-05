# Documentation of datasets

#' Aquarius location metadata dataset
#' @title Aquarius Location Metadata
#' @description Metadata dataset for all locations in Aquarius database. Updated when changes to locations occur.
#' @format Data frame of 9 columns x Number of Aquarius locations
#' @source Aquarius API, as called through the aq_get_location_list function
"aq_all_locations"

#' Aquarius locations and associated parameters
#' @title Aquarius Parameter-Location crosswalk
#' @description Dataset for all which locations are associated with each parameter in the Aquarius database. Updated when changes to publishing locations or parameters occur.
#' @format Data frame of 9 columns x Number of associated locations
#' @source Pairs aq_all_locations object with aq_get_location_parameters
"aq_parameter_location_crosswalk"

#' Aquarius parameter list
#' @title Aquarius parameters
#' @description List of parameter names for querying
#' @format Data frame of 2 columns (parameter name, unit) x number of parameters available in the database
#' @source Filters aq_parameter_location_crosswalk for unique parameters
"aq_all_parameters"
