# deltawqAQ

This package supports Bureau of Reclamation's Delta Water Quality monitoring. 
Scripts pull the most recently available time series data from the Aquarius database.


## Installation
To install package, use the `remotes::install_github()` function.

```
remotes::install_github("delta-wq-flow-data/deltawqAQ")
```

## Usage
See the Articles tab for examples of how to use this package. 

**Data**

* `aq_all_locations` provides the full list of locations in the Aquarius database along with associated metadata
* `aq_all_parameters` provides the full list of parameters in the current Aquarius database 
* `aq_parameter_location_crosswalk` provides the full crosswalk of locations and associated parameters

**Key functions include:**

* `aq_get_location_parameters()` for viewing the parameters associated with a location
* `aq_get_parameter_locations()` for viewing the locations associated with a parameter
* `aq_get_ts()` for obtaining time series for one location and parameter, for several parameters at one location, or for obtaining time series for one parameter at several locations

To view user-friendly package information and vignette with workflow example run pkgdown::preview_site(). 


