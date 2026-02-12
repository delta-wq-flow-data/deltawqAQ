aq_all_parameters <- deltawqAQ::aq_parameter_location_crosswalk |>
  dplyr::select(parameter_name, unit) |>
  dplyr::distinct()

usethis::use_data(aq_all_parameters, overwrite = TRUE)
