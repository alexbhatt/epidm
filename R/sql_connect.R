#' Connect to a SQL database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' An internal function to help setup connections to SQL databases
#'   acting as a wrapper for the odbc and DBI packages
#'
#' @importFrom odbc dbConnect odbc odbcListDrivers
#'
#' @param server a string containing the server connection
#' @param database a string containing the database name within the data store
#'
#' @return a SQL connection object
#' @keywords internal
#'

sql_connect <- function(server,
                        database){

  # get correct driver for ODBC connection
  if('SQL Server' %in% unique(odbc::odbcListDrivers()$name)) {
    # windows
    SQLdriver <- 'SQL Server'
  } else if('SQL Server' %in% unique(odbc::odbcListDrivers()$name)) {
    # linux [Debian-flavours]
    SQLdriver <- 'ODBC Driver 17 for SQL Server'
  } else {
    stop("SQL server drivers require installation")
  }

  conString <- paste0('driver={',SQLdriver,'};',
                      'server=',server,';',
                      'database=',database,';',
                      'trusted_connection=true',
                      'timeout=120')

  # connect to the database
  odbcConnect <- odbc::dbConnect(
    odbc::odbc(),
    .connection_string = conString
  )

  return(odbcConnect)
}
