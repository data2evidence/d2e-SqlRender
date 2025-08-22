#' Switch SQL dialect for Trex connection
#'
#' This function conditionally switches the SQL dialect to DuckDB for
#' when the 'trex_connection' environment variable is set to "true".
#'
#' @param dialect A character string specifying the original SQL dialect
#'   (e.g., "postgresql", "mysql", "sqlite", etc.)
#'
#' @return A character string representing the SQL dialect to use:
#'   \itemize{
#'     \item Returns "duckdb" if trex_connection environment variable is "true" and dialect is not "hana"
#'     \item Returns the original dialect otherwise
#'   }
#'
#' @details
#' The function checks the 'trex_connection' environment variable:
#' \itemize{
#'   \item If the environment variable is not set (NA), returns the original dialect
#'   \item If set to "true" and the dialect is not "hana", returns "duckdb"
#'   \item Otherwise, returns the original dialect unchanged
#' }
#'
#' @examples
#' # Set environment variable for testing
#' Sys.setenv(trex_connection = "true")
#' trexDialect("postgresql")  # Returns "duckdb"
#' trexDialect("hana")        # Returns "hana" (unchanged)
#'
#' # Unset environment variable
#' Sys.unsetenv("trex_connection")
#' trexDialect("postgresql")  # Returns "postgresql"
#'
#' @seealso \code{\link{Sys.getenv}} for environment variable handling
#'
#' @export
trexDialect <- function(dialect) {
  trex_env <- Sys.getenv("trex_connection", unset = NA)
  if (is.na(trex_env)) {
    return(dialect)
  }
  if (trex_env == "true" && dialect != "hana") {
    return("duckdb")
  } else {
    return(dialect)
  }
}