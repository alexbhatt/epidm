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
#' @return
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
#' @return
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
#' @return
#' @export
#'
sql_write <- function(x,
                      server,
                      database,
                      tablename){



  check_SQL <- paste0('SELECT COUNT(*) FROM ',DLdatabase,'.dbo.HospitalOnset_COVID')

  if(exists('x')){
    if(nrow(x)>0){
      if(DBI::dbIsValid(odbc_datalake)){
        print('connection established')
      } else{
        source('./R/utilities.R')
      }

      ## upload check to ensure the full dataset is uploaded
      DBrows <- dbGetQuery(odbc_datalake,check_SQL)[1,1]
      print(paste0(DBrows,'in DataLake currently'))

      while(DBrows!=nrow(hcai)){

        upstart <- Sys.time()
        print(paste('Start data upload',upstart))

        dbWriteTable(conn = odbc_datalake,
                     name = DBI::Id(schema  = 'dbo',
                                    table   = tablename),
                     value = hcai,
                     encoding = 'latin1',
                     row.names = FALSE,

                     overwrite = TRUE,
        )

        DBrows <- dbGetQuery(odbc_datalake,check_SQL)[1,1]

      }


      upend <- Sys.time()
      print(paste0(nrow(hcai),' records written to ',
                   DLdatabase,'.dbo.HospitalOnset_COVID in ',
                   round(difftime(upend,upstart,units = 'mins')),'min'))
    }else{print('data empty')}
  }else{print('data does not exists')}
}
