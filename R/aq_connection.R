.aq_env <- new.env(parent = emptyenv())

.proxy_url <- "https://ydq2zyq24pdbyptkrm3kjtxuqm0mfwai.lambda-url.us-west-2.on.aws"

#' @title Check proxy authenticator server's health
#' @description Check proxy server health
#' @details Provides message if proxy server is not healthy
#'
#' @family Aquarius database connections
#' @keywords internal
aq_check_proxy_health <- function() {
  resp <- httr2::request(W.proxy_url) |> httr2::req_perform()
  status_code <- httr2::resp_status(resp)
  if (status_code == 200) {
    cli::cli_alert_success("Proxy server is up and healthy")
    TRUE
  } else {
    cli::cli_alert_danger("Proxy server not healthy")
    FALSE
  }
}

#' @title Connect to Aquarius
#' @description Establishes and stores a connection to the Aquarius database.
#' @details By default, connects via a secure proxy that manages credentials
#' server-side — no username or password needed. If `username` and `password`
#' are provided (or set via environment variables), a direct connection to
#' the Aquarius server is used instead.
#'
#' @param server_hostname URL for database. Only needed for direct connections.
#'   If NULL, uses AQTS_SERVER from environment variables.
#' @param username Username for direct database connection (admin use).
#'   If NULL, uses AQTS_USERNAME from environment variables.
#' @param password Password for direct database connection (admin use).
#'   If NULL, uses AQTS_PASSWORD from environment variables.
#' @param timeout_seconds Number of seconds before connection times out (default: 30)
#' @param force_reconnect Logical. If TRUE, forces a new connection even if one exists (default: FALSE)
#'
#' @return Invisible NULL (connection stored in package environment)
#' @family Aquarius database connections
#' @export
#'
#' @examples
#' # Default: connect via proxy (no credentials needed)
#' aq_connect()
#'
#' # Direct connection with explicit credentials (admin use)
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

  if (!force_reconnect && aq_is_connected()) {
    cli::cli_alert_info("Already connected to {(.aq_env$server)}")
    return(invisible(NULL))
  }

  username <- username %||% Sys.getenv("AQTS_USERNAME", unset = "")
  password <- password %||% Sys.getenv("AQTS_PASSWORD", unset = "")

  if (username != "" && password != "") {
    aq_connect_direct(
      server_hostname = server_hostname %||% Sys.getenv("AQTS_SERVER"),
      username = username,
      password = password,
      timeout_seconds = timeout_seconds
    )
  } else {
    aq_connect_proxy(timeout_seconds = timeout_seconds)
  }

  invisible(NULL)
}

#' @title Connect via proxy
#' @description Connects to Aquarius via the secure proxy (no credentials needed).
#' @param timeout_seconds Number of seconds before connection times out
#' @return Invisible NULL
#' @family Aquarius database connections
#' @keywords internal
aq_connect_proxy <- function(timeout_seconds = 30) {
  req <- httr2::request(.proxy_url) |>
    httr2::req_url_path_append("token") |>
    httr2::req_timeout(timeout_seconds)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to connect via proxy",
        "x" = conditionMessage(e)
      ))
    }
  )

  body <- httr2::resp_body_json(resp)

  .aq_env$connected <- TRUE
  .aq_env$server <- body$server
  .aq_env$publish_uri <- body$server
  .aq_env$token <- body$token
  .aq_env$connected_at <- Sys.time()

  cli::cli_alert_success("Connected to AQTS Server")
}

#' @title Connect directly to Aquarius
#' @description Connects directly to Aquarius with explicit credentials (admin use).
#' @param server_hostname URL for database
#' @param username Username for database
#' @param password Password for database
#' @param timeout_seconds Number of seconds before connection times out
#' @return Invisible NULL
#' @family Aquarius database connections
#' @keywords internal
aq_connect_direct <- function(server_hostname, username, password, timeout_seconds = 30) {
  if (server_hostname == "") {
    cli::cli_abort(c(
      "Missing server hostname for direct connection",
      "i" = "Provide server_hostname or set AQTS_SERVER environment variable"
    ))
  }

  req <- httr2::request(server_hostname) |>
    httr2::req_url_path_append("session") |>
    httr2::req_body_json(list(Username = username, EncryptedPassword = password)) |>
    httr2::req_timeout(timeout_seconds)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to connect to AQUARIUS server",
        "i" = "Server: {server_hostname}",
        "x" = conditionMessage(e)
      ))
    }
  )

  token <- httr2::resp_body_string(resp)

  .aq_env$connected <- TRUE
  .aq_env$server <- server_hostname
  .aq_env$publish_uri <- server_hostname
  .aq_env$token <- token
  .aq_env$connected_at <- Sys.time()

  cli::cli_alert_success("Connected to AQTS Server")
}

#' @title Disconnect from Aquarius database
#' @description Disconnects from the Aquarius database and clears stored connection.
#' @details Closes the active connection and clears the connection
#' from the package environment. After disconnecting, you'll need to call aq_connect()
#' again before using other package functions.
#'
#' @return Invisible NULL
#' @family Aquarius database connections
#' @export
#'
#' @examples
#' aq_disconnect()
aq_disconnect <- function() {
  if (!aq_is_connected()) {
    cli::cli_alert_info("No active AQUARIUS connection to disconnect")
    return(invisible(NULL))
  }

  cli::cli_alert_info("Disconnecting from AQUARIUS...")

  if (!is.null(.aq_env$publish_uri) && !is.null(.aq_env$token)) {
    req <- httr2::request(.aq_env$publish_uri) |>
      httr2::req_url_path_append("session") |>
      httr2::req_method("DELETE") |>
      httr2::req_headers("X-Authentication-Token" = .aq_env$token)

    tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        cli::cli_alert_warning("Error during disconnect: {conditionMessage(e)}")
      }
    )
  }

  .aq_env$connected <- FALSE
  .aq_env$server <- NULL
  .aq_env$publish_uri <- NULL
  .aq_env$token <- NULL
  .aq_env$connected_at <- NULL

  cli::cli_alert_success("Disconnected")
  invisible(NULL)
}

#' @title Ensure Aquarius connection
#' @description Internal helper function to ensure connection exists before operations.
#' @details Checks if a connection exists and creates one if needed.
#' It's designed to be called by other package functions that need a connection.
#'
#' @return Invisible NULL (throws error if connection cannot be established)
#' @family Aquarius database connections
#' @keywords internal
aq_ensure_connection <- function() {
  if (!aq_is_connected()) {
    cli::cli_alert_info("Connecting to AQUARIUS...")
    aq_connect()
  }
  invisible(NULL)
}

#' @title Create authenticated request
#' @description Creates an httr2 request with the session token header.
#' @param url The URL for the request (defaults to publish URI)
#' @return An httr2 request object with X-Authentication-Token header
#' @family Aquarius database connections
#' @keywords internal
aq_request <- function(url = NULL) {
  aq_ensure_connection()

  base_url <- url %||% .aq_env$publish_uri

 httr2::request(base_url) |>
    httr2::req_headers("X-Authentication-Token" = .aq_env$token)
}


#' @title Check if connected to Aquarius
#' @description Checks whether there is an active Aquarius connection.
#' @details This function returns TRUE if a connection has been established and not
#' yet disconnected. Note that this checks the stored state and does not verify
#' the connection is still active on the server side.
#'
#' @return Logical indicating whether there is an active connection
#' @family Aquarius database connections
#' @keywords internal
#'
#' @examples
#' if (aq_is_connected()) {
#'   # Do something with the connection
#' }
aq_is_connected <- function() {
  isTRUE(.aq_env$connected)
}
