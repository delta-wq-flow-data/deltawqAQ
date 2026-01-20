aq_connect <- function(server_hostname, username, password, timeout_seconds=30) {
  # Attempt connection with error handling
  tryCatch({
    # Set HTTP timeout if supported by httr
    if (timeout_seconds > 0) {
      httr::set_config(httr::timeout(timeout_seconds))
    }

    timeseries$connect(server_hostname,username,password)
    cat("Connected successfully! Server version:", timeseries$version, "\n\n")

  }, error = function(e) {
    cat_bullet("\nERROR: Failed to connect to AQUARIUS server.\n")
    cli_bullets("Server:", server_hostname, "\n")
    cat("Error message:", conditionMessage(e), "\n\n")
    cat("Troubleshooting tips:\n")
    cat("  1. Verify server URL is correct and accessible\n")
    cat("  2. Check username and password in .Renviron\n")
    cat("  3. Ensure you have network access to the server\n")
    cat("  4. Try accessing the server in a web browser\n")
    stop("Connection failed. Please fix the issues above and try again.")
  })

}


# Function to disconnect from Aquarius
aq_disconnect <- function() {
  # Ensure disconnection happens even if there are errors
  on.exit({
    cat("\nDisconnecting from AQUARIUS...\n")
    tryCatch({
      timeseries$disconnect()
      cat("Disconnected successfully.\n")
    }, error = function(e) {
      cat("Warning: Error during disconnect:", conditionMessage(e), "\n")
    })
  })
}
