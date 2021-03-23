#' Connect to a SQL database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' An internal function to help setup connections to SQL databases
#'   acting as a wrapper for the odbc and DBI packages
#'
#' @importFrom odbc odbcListDrivers dbConnect odbc
#' @importFrom DBI dbIsValid
#'
#' @param server a string containing the server connection
#' @param database a string containing the database name within the data store
#'
#' @seealso sql_clean sql_read sql_write
#'
#' @return a SQL connection object
#' @keywords internal
#'

sql_connect <- function(server,
                        database){

  # # get correct driver for ODBC connection
  # if('SQL Server' %in% unique(odbc::odbcListDrivers()$name)) {
  #   # windows
  #   SQLdriver <- 'SQL Server'
  # } else if('SQL Server' %in% unique(odbc::odbcListDrivers()$name)) {
  #   # linux [Debian-flavours]
  #   SQLdriver <- 'ODBC Driver 17 for SQL Server'
  # } else {
  #   stop("SQL server drivers require installation")
  # }

  ## cycle through the available drivers on the machine to find the right one
  for(driver in unique(odbc::odbcListDrivers()$name)){

    ## uses Active Directory credentials
    conString <- paste0('driver={',driver,'};',
                        'server=',server,';',
                        'database=',database,';',
                        'trusted_connection=true',
                        'timeout=120'
                        )

    # connect to the database
    odbcConnect <- odbc::dbConnect(
      odbc::odbc(),
      .connection_string = conString
    )

    if(DBI::dbIsValid(odbcConnect)) {
      break
    }
  }

  return(odbcConnect)
}
