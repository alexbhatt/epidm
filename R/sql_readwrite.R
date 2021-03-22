sql_read <- function(server,
                     database,
                     sql){

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

  # connect to the database
  ODBCConnect <- odbc::dbConnect(
    odbc(),
    .connection_string = paste0('driver={',SQLdriver,'};',
                                'server=',server,';',
                                'database=',database,';',
                                'trusted_connection=true',
                                'timeout=120')
  )

  # test the connection is valid
  if(DBI::dbIsValid(ODBCConnect)){
    tableResult <- dbGetQuery(ODBCConnect,
                              epidm::clean_sql(sql))
  }

  return(tableResult)

  # close the connection
  DBI::dbDisconnect(ODBCConnect)

}


sql_write <- function(x,
                      server,
                      database,
                      tablename){

  check_SQL <- paste0('SELECT COUNT(*) FROM ',DLdatabase,'.dbo.HospitalOnset_COVID')

  if(exists('hcai')){
    if(nrow(hcai)>0){
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
                                    table   = 'HospitalOnset_COVID'),
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
