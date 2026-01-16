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
  if (write==TRUE) {
    output_file <- if (!is.null(output_path)) {
      here::here(output_path, paste0(station_code, "_", parameter, "_data.csv"))
    } else {
      here::here(paste0(station_code, "_", parameter, "_data.csv"))
    }

    write.csv(df, output_file, row.names = FALSE)
    cat("Data written to:", output_file, "\n")
    # return(invisible(df))
  }

  # Return normally if not writing
  return(df)
}
