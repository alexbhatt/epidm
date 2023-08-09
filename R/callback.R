#' @title callback function for readr read_delim_chunked in sql_upload_from_txt()
#' @author Owen Pullen
#' @param chunk chunk of txt file loaded by readr::read_delim_chunked
#' @param pos position of chunk loaded by readr::read_delim_chunked
#' @note getOption() provides additional params set by sql_upload_from_txt/csv
#' @examples
#' f <- function(x,pos) print(pos)
#' readr::read_csv_chunked(readr::readr_example("mtcars.csv"),
#' callback = readr::SideEffectChunkCallback$new(f),
#' chunk_size = 10
#' )
#' @returns Writes a table in a SQL database and saves out a copy of chunk in parquet
#' @description
#' A function to be run by readr inside of sql_upload_from_txt() or
#' sql_upload_from_csv() (referred to collectively) this function writes the chunk of the file
#' read by readr to a SQL table specified in sql_upload_from_txt()
#' and produces a parquet file backup of each chunk read by readr, combined by combine_parquet_backup
#' called in sql_upload_parquet_backup_from_txt. Parquet writing can disabled by
#' specifying write_parquet = FALSE in sql_upload_from_txt
#' #' Upload txt file to staging table and create parquet backup
#' @export
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
}
