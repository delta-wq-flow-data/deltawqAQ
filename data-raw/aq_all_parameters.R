aq_all_parameters <- deltawqAQ::aq_all_parameter_locations %>%
  select(parameter_name, unit) %>%
  distinct()

usethis::use_data(aq_all_parameters, overwrite = TRUE)
