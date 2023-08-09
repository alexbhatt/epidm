#' Function to execute SQL files
#' @param con DBI connection object
#' @param query_path file path to a sql query, this should be saved in a .sql file
#' for readability and maintainability
#' @returns number of rows affected by query provided
#' @description
#' Executes upsert command in SQL database, this will execute any SQL query provided in
#' a .sql file, so use sensibly. Do not execute SQL files without knowing the expected output.
#' Users will have to have relevant permissions in the database e.g. if user wants to execute
#' an upsert (INSERT and UPDATE) user must have SQL insert and update permission in database
#'
#' This function just provides a simplified way to execute a .sql file handling the reading
#' into R for the user.
#' This is similar to [dbExecute()] if you have a string query use [dbExecute()]
#' @export
#'
execute_sql_file <- function(con, query_path) {
  # Get query from .sql file
  query <- readr::read_file(query_path)
  # Execute the UPSERT query
  res <- DBI::dbSendStatement(con, query)
  DBI::dbHasCompleted(res)
  rows_affected <- DBI::dbGetRowsAffected(res)
  DBI::dbClearResult(res)
  return(rows_affected)
}
