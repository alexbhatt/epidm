#' Connect to a SQL database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' An internal function to help setup connections to SQL databases
#'   acting as a wrapper for the odbc and DBI packages
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


#' Read a table from a SQL database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' Read a table object to a SQL database. Acts a wrapper for odbc and DBI
#'   packages.
#'
#' @param server a string containing the server connection
#' @param database a string containing the database name within the data store
#' @param sql a string containing a SQL query or to a .sql/.txt SQL query
#'
#' @return a table from a SQL database
#' @export
#'
sql_read <- function(server,
                     database,
                     sql){

  odbcConnect <- sql_connect(server = server, database = database)

  # test the connection is valid
  if(DBI::dbIsValid(odbcConnect)){

    sqlQuery <- epidm::sql_clean(sql)

    # query te database
    tableResult <- DBI::dbGetQuery(conn = odbcConnect,
                                   statement = sqlQuery)
  }

  return(tableResult)

  # close the connection
  DBI::dbDisconnect(odbcConnect)

}



#' Write a table to a SQL database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' Write a table object to a SQL database. Acts a wrapper for odbc and DBI
#'   packages with additional checks to ensure upload completes.
#'
#' @param x a data.frame/data.table/tibble object
#' @param server a string containing the server connection
#' @param database a string containing the database name within the data store
#' @param tablename a string containing the chosen SQL database table name
#'
#' @return writes a data.frame/data.table/tibble to a SQL database
#' @export
#'
sql_write <- function(x,
                      server,
                      database,
                      tablename){

  ## connect to the database
  odbcConnect <- sql_connect(server = server, database = database)

  # does the table already exist
  if(DBI::dbExistsTable(odbcConnect,tablename)){
    check_SQL <- paste0('SELECT COUNT(*) FROM ',
                        database,
                        '.dbo.',
                        tablename)
  }

  # check the object exists
  if(exists(quote(x))){
    if(nrow(x)>0){
      if(DBI::dbIsValid(odbcConnect)){
        print('connection established')
      } else{
        odbcConnect <- sql_connect(server = server, database = database)
      }

      ## upload check to ensure the full dataset is uploaded
      DBrows <- DBI::dbGetQuery(odbcConnect,check_SQL)[1,1]
      print(paste(DBrows,'in',database,'currently'))

      ## this will ensure that object matches the upload
      while(DBrows!=nrow(x)){

        upstart <- Sys.time()
        print(paste('Start data upload',upstart))

        dbWriteTable(conn = odbcConnect,
                     name = DBI::Id(schema = 'dbo',
                                    table   = tablename),
                     value = x,
                     encoding = 'latin1',
                     row.names = FALSE,
                     overwrite = TRUE
        )

        DBrows <- DBI::dbGetQuery(odbcConnect,check_SQL)[1,1]

      }

      upend <- Sys.time()
      print(paste0(nrow(x),
                   ' records written to ',
                   database,
                   '.dbo.',tablename,' in ',
                   round(difftime(upend,upstart,units = 'mins')),'min'))
    }else{
      print('data empty')
    }
  }else{
    print('data does not exists')
  }
}
