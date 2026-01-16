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
