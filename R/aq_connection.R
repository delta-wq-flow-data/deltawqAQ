#' @title Connect to Aquarius
#' @description `aq_connect` connects to the Aquarius database.
#' @details This function uses stored username and password to access the Aquarius database.
#' The function also provides error messages if the connection is not successful.
#' This function is often called in other functions in this package.
#' @param server_hostname url for database
#' @param username username for database
#' @param password password for database
#' @param timeout_seconds number of seconds hanging before connection times out


aq_connect <- function(server_hostname, username, password, timeout_seconds=30) {
  # Attempt connection with error handling
  tryCatch({
    # Set HTTP timeout if supported by httr
    if (timeout_seconds > 0) {
      httr::set_config(httr::timeout(timeout_seconds))
    }

    timeseries$connect(server_hostname,username,password)
    cli::cli_alert_success("Connected successfully! Server version: {timeseries$version}")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to connect to AQUARIUS server.")
    cli::cli_alert_info("Server: {server_hostname}")
    cli::cli_alert_warning("Error message: {conditionMessage(e)}")
    cli::cli_h2("Troubleshooting tips:")
    cli::cli_ol(c(
      "Verify server URL is correct and accessible",
      "Check username and password in .Renviron",
      "Ensure you have network access to the server",
      "Try accessing the server in a web browser"
    ))
    cli::cli_abort("Connection failed. Please fix the issues above and try again.")
  })

}


#' @title Disconnect from Aquarius database
#' @description `aq_disconnect` disconnects from the Aquarius database.
#' @details This function does not require any inputs and is often called in other functions in this package.

# Function to disconnect from Aquarius
aq_disconnect <- function() {
  # Ensure disconnection happens even if there are errors
  cli::cli_alert_info("Disconnecting from AQUARIUS...")
  tryCatch({
    timeseries$disconnect()
    cli::cli_alert_success("Disconnected successfully")
  }, error = function(e) {
    cli::cli_alert_warning("Warning: Error during disconnect: {conditionMessage(e)}")
  })
}
