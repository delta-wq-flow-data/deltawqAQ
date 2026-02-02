# Functions for connecting and disconnecting from Aquarius database

# Create private environment for connection token
.aq_env <- new.env(parent = emptyenv())

#' @title Connect to Aquarius
#' @description Establishes and stores a connection to the Aquarius database.
#' @details Creates a connection that can be reused across
#' function calls. The connection is stored in a package environment and
#' remains active until explicitly disconnected or the R session ends.
#'
#' @param server_hostname URL for database. If NULL, uses AQTS_SERVER from environment variables.
#' @param username Username for database. If NULL, uses AQTS_USERNAME from environment variables.
#' @param password Password for database. If NULL, uses AQTS_PASSWORD from environment variables.
#' @param timeout_seconds Number of seconds before connection times out (default: 30)
#' @param force_reconnect Logical. If TRUE, forces a new connection even if one exists (default: FALSE)
#'
#' @return Invisible NULL (connection stored in package environment)
#' @export
#'
#' @examples
#' # Using environment variables
#' aq_connect()
#'
#' # Explicit credentials
#' aq_connect(
#'   server_hostname = "https://aquarius.example.com",
#'   username = "user",
#'   password = "pw"
#' )
aq_connect <- function(server_hostname = NULL,
                       username = NULL,
                       password = NULL,
                       timeout_seconds = 30,
                       force_reconnect = FALSE) {

  # Check if connection already exists
  if (!force_reconnect && aq_is_connected()) {
    cli::cli_alert_info("Already connected to AQUARIUS server")
    cli::cli_alert_success("Server version: {(.aq_env$version)}")
    return(invisible(NULL))
  }

  # Get credentials from environment if not provided
  server_hostname <- server_hostname %||% Sys.getenv("AQTS_SERVER")
  username <- username %||% Sys.getenv("AQTS_USERNAME")
  password <- password %||% Sys.getenv("AQTS_PASSWORD")

  # Validate credentials
  if (server_hostname == "" || username == "" || password == "") {
    cli::cli_abort(c(
      "Missing connection credentials",
      "i" = "Provide server_hostname, username, and password as arguments",
      "i" = "Or set AQTS_SERVER, AQTS_USERNAME, and AQTS_PASSWORD environment variables"
    ))
  }

  # Attempt connection with error handling
  tryCatch({
    # Set HTTP timeout if supported by httr
    if (timeout_seconds > 0) {
      httr::set_config(httr::timeout(timeout_seconds))
    }

    # Connect to Aquarius
    timeseries$connect(server_hostname, username, password)

    # Store connection info in package environment
    .aq_env$connected <- TRUE
    .aq_env$server <- server_hostname
    .aq_env$version <- timeseries$version
    .aq_env$connected_at <- Sys.time()

    cli::cli_alert_success("Connected successfully! Server version: {timeseries$version}")

  }, error = function(e) {
    cli::cli_alert_danger("Failed to connect to AQUARIUS server")
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

  invisible(NULL)
}

#' @title Disconnect from Aquarius database
#' @description Disconnects from the Aquarius database and clears stored connection.
#' @details Closes the active connection and clears the connection
#' from the package environment. After disconnecting, you'll need to call aq_connect()
#' again before using other package functions.
#'
#' @return Invisible NULL
#' @export
#'
#' @examples
#' aq_disconnect()
aq_disconnect <- function() {
  # Check if there's a connection to disconnect
  if (!aq_is_connected()) {
    cli::cli_alert_info("No active AQUARIUS connection to disconnect")
    return(invisible(NULL))
  }

  # Ensure disconnection happens even if there are errors
  cli::cli_alert_info("Disconnecting from AQUARIUS...")

  tryCatch({
    timeseries$disconnect()

    # Clear connection state
    .aq_env$connected <- FALSE
    .aq_env$server <- NULL
    .aq_env$version <- NULL
    .aq_env$connected_at <- NULL

    cli::cli_alert_success("Disconnected successfully")

  }, error = function(e) {
    # Still clear the state even if disconnect fails
    .aq_env$connected <- FALSE
    .aq_env$server <- NULL
    .aq_env$version <- NULL
    .aq_env$connected_at <- NULL

    cli::cli_alert_warning("Warning: Error during disconnect: {conditionMessage(e)}")
  })

  invisible(NULL)
}

#' @title Ensure Aquarius connection
#' @description Internal helper function to ensure connection exists before operations.
#' @details Checks if a connection exists and creates one if needed.
#' It's designed to be called by other package functions that need a connection.
#'
#' @return Invisible NULL (throws error if connection cannot be established)
#' @keywords internal
aq_ensure_connection <- function() {
  if (!aq_is_connected()) {
    cli::cli_alert_info("No active connection. Connecting to AQUARIUS...")
    aq_connect()
  }
  invisible(NULL)
}

# Helper function for NULL coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @title Check if connected to Aquarius
#' @description Checks whether there is an active Aquarius connection.
#' @details This function returns TRUE if a connection has been established and not
#' yet disconnected. Note that this checks the stored state and does not verify
#' the connection is still active on the server side.
#'
#' @return Logical indicating whether there is an active connection
#' @export
#'
#' @examples
#' if (aq_is_connected()) {
#'   # Do something with the connection
#' }
aq_is_connected <- function() {
  isTRUE(.aq_env$connected)
}
