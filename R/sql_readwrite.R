#' Read a table from a SQL database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' Read a table object to a SQL database. Acts a wrapper for odbc and DBI
#'   packages.
#'
#' @importFrom DBI dbIsValid dbGetQuery dbDisconnect
#' @importFrom odbc dbConnect odbc
#'
#' @param server a string containing the server connection
#' @param database a string containing the database name within the data store
#' @param sql a string containing a SQL query or to a .sql/.txt SQL query
#'
#' @seealso sql_clean sql_connect
#'
#' @return a table from a SQL database
#' @export
#'
sql_read <- function(server,
                     database,
                     sql){

  odbcConnect <- epidm::sql_connect(server = server, database = database)

  timeStart <- Sys.time()

  # test the connection is valid
  if(DBI::dbIsValid(odbcConnect)){

    sqlQuery <- epidm::sql_clean(sql)

    # query te database
    tableResult <- DBI::dbGetQuery(conn = odbcConnect,
                                   statement = sqlQuery)
  }

  ## success!
  message(paste0('Data imported in ',
               round(difftime(Sys.time(),timeStart,units = 'mins')),'min'))

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
#' @importFrom DBI dbExistsTable dbGetQuery dbWriteTable dbDisconnect
#' @importFrom odbc dbConnect odbc
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
  odbcConnect <- epidm::sql_connect(server = server, database = database)

  # used to check if the table outputs upload
  checkSQL <- paste0('SELECT COUNT(*) FROM ',database,'.dbo.',tablename)

  # check the object exists
  if(exists('x')){
    if(nrow(x)>0){
      if(DBI::dbIsValid(odbcConnect)){
        message('connection established')
      } else{
        odbcConnect <- epidm::sql_connect(server = server, database = database)
      }

      ## upload check to ensure the full dataset is uploaded
      if(DBI::dbExistsTable(odbcConnect,tablename)){
        DBrows <- DBI::dbGetQuery(odbcConnect,checkSQL)[1,1]
        message(paste0(DBrows,' records in [',
                     database,'].[dbo].[',tablename,'] currently'))
      } else {
        DBrows <- 0
        message(paste0('[',database,'].[dbo].[',tablename,
                     '] does not exist; creating table.'))
      }

      timeStart <- Sys.time()

      ## this will ensure that object matches the upload
      while(DBrows!=nrow(x)){

        message(paste('Start data upload',timeStart))

        DBI::dbWriteTable(conn = odbcConnect,
                          name = DBI::Id(schema = 'dbo',
                                         table   = tablename),
                          value = x,
                          encoding = 'latin1',
                          row.names = FALSE,
                          overwrite = TRUE
        )

        ## perform the check after the upload for the while loop
        DBrows <- DBI::dbGetQuery(odbcConnect,checkSQL)[1,1]

      }

      ## success!
      message(paste0(nrow(x),
                   ' records written to [',
                   database,'].[dbo].[',tablename,'] in ',
                   round(difftime(Sys.time(),timeStart,units = 'mins')),'min')
            )
    }else{
      message('data empty')
    }
  }else{
    message('data does not exists')
  }

  # close the connection
  DBI::dbDisconnect(odbcConnect)
}
