#' @title callback function for readr read_delim_chunked in [sql_upload_txt_chunked()]
#' @author Owen Pullen
#' @param chunk chunk of txt file loaded by [readr::read_delim_chunked()]
#' @param pos position of chunk loaded by [readr::read_delim_chunked()]
#' @note [getOption()] provides additional params set by sql_upload_txt_chunked/csv
#' @examples
#' f <- function(x,pos) print(pos)
#' readr::read_csv_chunked(readr::readr_example("mtcars.csv"),
#' callback = readr::SideEffectChunkCallback$new(f),
#' chunk_size = 10
#' )
#' @returns TRUE invisibly also writes a table in a SQL database and saves out a copy of chunk in parquet
#' @description
#' `r lifecycle::badge('experimental')`
#' \itemize{
#' \item Not usually called do not run unless re-developing
#' \item A function to be run inside of [sql_upload_txt_chunked()] or
#' [sql_upload_txt_chunked()]
#' \item This function writes the chunk of the file
#' read by readr to a SQL table specified in [sql_upload_txt_chunked()]
#' and produces a parquet file backup of each chunk read
#' \item Called in [sql_upload_txt_chunked()] and [sql_upload_csv_chunked()]
#' \item Parquet writing can disabled by specifying write_parquet = FALSE
#' in above functions
#' \item Upload txt/ csv file chunk file to staging table and create parquet file of the chunk
#' }
#' @keywords internal
#'
callback <- function(chunk, pos) {
  # Remove spaces from colnames
  colnames(chunk) <- gsub("[^[:alnum:]]", "_", colnames(chunk))
  # write to SQL staging table
  DBI::dbWriteTable(
    con = getOption("con"),
    DBI::Id(schema = getOption("schema"), table = getOption("table_name")),
    append = TRUE,
    value = chunk
  )
  # Parquet backup pos used to prevent overwriting
  if (getOption("write_parquet")) {
    arrow::write_parquet(chunk, glue::glue(
      getOption("backup_filepath"),
      "{pos}_",
      getOption("backup_name"),
      getOption("pattern"),
      ".parquet")
    )
  }
  return(invisible(TRUE))
}
