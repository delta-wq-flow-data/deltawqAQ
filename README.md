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

**Key functions include:**

* `aq_get_location_parameters()` for viewing the parameters associated with a location
* `aq_get_ts()` for obtaining time series for one location and parameter
* `aq_get_ts_multi_param()` for obtaining time series for several parameters at one location
* `aq_get_ts_multi_location()` for obtaining time series for one parameter at several locations




