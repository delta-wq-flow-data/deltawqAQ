#' @title Get locations associated with a parameter
#'
#' @description Provides a list of locations associated with a queried parameter.
#'
#' @details Provides a list of locations and dates associated with a queried parameter.
#' Exactly one parameter name should be provided.
#'
#' @param param Parameter of interest

#' @return A data frame containing columns for location information
#'
#' @examples
#' aq_get_parameter_locations("Water Temp")
#'
#' @export
aq_get_parameter_locations <- function(param) {



  # Filter aq_all_parameter_locations data object to the parameter selected
  params_filtered <- deltawqAQ::aq_all_parameter_locations %>%
    dplyr::filter(parameter_name == param) %>%
    dplyr::select(parameter_name, cdec_code, location_id, aq_location_name, aq_location_id)

  # Return df
  return(params_filtered)

}
