# Function to get a timeseries
aq_get_ts = function(station_code, parameter, label, query_from, query_to,
                     write = FALSE, output_path = NULL) {

  # Function to connect to aquarius
  aq_connect(server_hostname = Sys.getenv("AQTS_SERVER"),
             username = Sys.getenv("AQTS_USERNAME"),
             password = Sys.getenv("AQTS_PASSWORD"))

  # Ensure disconnect on exit
  on.exit(aq_disconnect())

  # Function to obtain time series data and return or write df based on inputs
  df <- aq_process_ts(station_code, parameter, label, query_from, query_to)

  # Write to CSV if requested
  if (isTRUE(write)) {
    output_file <- if (!is.null(output_path)) {
      here::here(output_path, paste0(station_code, "_", parameter, "_data.csv"))
    } else {
      here::here(paste0(station_code, "_", parameter, "_data.csv"))
    }

    tryCatch({
      write.csv(df, output_file, row.names = FALSE)
      cli::cli_alert_success("Data written to: {output_file}")
    }, error = function(e) {
      cli::cli_alert_danger("Failed to write file: {e$message}")
    })
    return(invisible(df))
  }

  # Return normally if not writing
  return(df)
}


### Function to process data into data frame
aq_process_ts = function(station_code, parameter, label, query_from, query_to) {

  # Ensure disconnection happens no matter how function exits
  on.exit(timeseries$disconnect(), add = TRUE)

  # Get the timeseries ID
  # AQUARIUS uses the format: Parameter.Label@LocationIdentifier
  timeseries_id <- paste0(parameter, ".", label, "@", station_code)
  cli::cli_alert_info(c(
    "Requesting time-series: {timeseries_id}",
    "Time range: {query_from} to {query_to}"
  ))

  ## Get the JSON form of the data ------------------
  cli::cli_alert_info("Fetching data...")
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
    cli::cli_alert_warning("No data found for the specified time range")
    cli::cli_abort("No data retrieved.")
  }

  cli::cli_alert_success("Retrieved {nrow(points)} data points")

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

  return(df)
}
