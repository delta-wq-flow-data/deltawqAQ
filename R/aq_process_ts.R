### Function to process data into data frame
aq_process_ts = function(station_code, parameter, label, query_from, query_to) {

  ## Get the timeseries ID -------------------------
  ## AQUARIUS uses the format: Parameter.Label@LocationIdentifier
  timeseries_id <- paste0(parameter, ".", label, "@", station_code)
  cat("Requesting time-series:", timeseries_id, "\n")
  cat("Time range:", query_from, "to", query_to, "\n\n")

  ## Get the JSON form of the data ------------------
  cat("Fetching data...\n")
  json_data <- timeseries$getTimeSeriesData(
    timeSeriesIds = timeseries_id,
    queryFrom = query_from,
    queryTo = query_to
  )

  ## Process data -----------------------
  # Extract points from the JSON response
  points <- json_data$Points

  # Check if we got any data
  if (nrow(points) == 0) {
    cat("No data found for the specified time range.\n")
    timeseries$disconnect()
    stop("No data retrieved. Exiting.")
  }

  cat("Retrieved", nrow(points), "data points\n\n")

  # Convert timestamps to POSIXct format for easier handling
  # parseIso8601 automatically handles any timezone offset in the ISO 8601 string
  points$DateTime <- sapply(points$Timestamp, timeseries$parseIso8601)
  points$DateTime <- as.POSIXct(points$DateTime, origin = "1970-01-01")

  # Get the actual values (column name depends on number of time-series requested)
  # For single time-series, use NumericValue1
  points$Value <- points$NumericValue1

  # get timeseries info
  ts_info <- json_data$TimeSeries

  # Create clean data frame --------------
  df <- data.frame(
    DateTime = points$DateTime,
    Value = points$Value,
    Station = station_code[1],
    Parameter = ts_info$Parameter[1],
    Label = ts_info$Label[1],
    Unit = ts_info$Unit[1],
    Approval = points$ApprovalName1,
    stringsAsFactors = FALSE
  )
}
