#' Switch SQL dialect for Trex connection
#'
#' This function conditionally switches the SQL dialect based on the
#' value of the 'trex_connection' environment variable. Supported values:
#' "trex", "trex_hana" (or unset).
#'
#' - If 'trex_connection' is not set, returns the original dialect.
#' - If 'trex_connection' == "trex" and dialect != "hana", returns "duckdb".
#' - If 'trex_connection' == "trex_hana", returns "hana".
#' - For any other value, returns the original dialect.
#'
#' @param dialect A character string specifying the original SQL dialect
#'   (e.g., "postgresql", "mysql", "sqlite", etc.)
#'
#' @return A character string representing the SQL dialect to use:
#'   \itemize{
#'     \item If 'trex_connection' is not set, returns the original dialect
#'     \item If 'trex_connection' == "trex" and dialect != "hana", returns "duckdb"
#'     \item If 'trex_connection' == "trex_hana", returns "hana"
#'     \item For any other value, returns the original dialect
#'   }
#'
#' @details
#' The function checks the 'trex_connection' environment variable:
#' \itemize{
#'   \item If the environment variable is not set (NA), returns the original dialect
#'   \item If set to "trex" and the dialect is not "hana", returns "duckdb"
#'   \item If set to "trex_hana", returns "hana"
#'   \item Otherwise, returns the original dialect unchanged
#' }
#'
#' @examples
#' # Set environment variable for testing
#' Sys.setenv(trex_connection = "trex")
#' trexDialect("postgresql")  # Returns "duckdb"
#' trexDialect("hana")        # Returns "hana" (unchanged)
#'
#' Sys.setenv(trex_connection = "trex_hana")
#' trexDialect("postgresql")  # Returns "hana"
#' trexDialect("hana")        # Returns "hana"
#'
#' Sys.setenv(trex_connection = "other_value")
#' trexDialect("postgresql")  # Returns "postgresql"
#'
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
    if (trex_env == "trex" && dialect != "hana") {
      return("duckdb")
    } else if (trex_env == "trex_hana") {
      return("hana")
    } else {
      return(dialect)
    }
}