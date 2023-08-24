#' @title Function to read csv file in chunks and upload to SQL & convert to parquet
#' @author Owen Pullen
#' @param input_filename file path to a .csv file
#' @param col_list list of columns a vroom or [readr] readr:::as.col_spec() object, default NULL
#' when [NULL] [readr] will auto detect column names
#' e.g. col_list <- cols_only(column = col_character()), see [readr::cols_only()]
#' Please see [readr::read_delim_chunked()] argument col_types to read up on this
#' To read in all cols as character use col_list = readr::cols(.default = col_character()) see [readr::cols()]
#' readr will guess data types by the [readr::read_delim_chunked()]'s guess_max parameter which is equal to the chunk size,
#' this is fast but unreliable particularly around integers, dates and float and may cause
#' inserts to fail
#' @param con a DBI con connection object [DBI::dbConnect()]
#' @param schema the schema of the SQL table you are uploading to default "dbo"
#' @param table_name SQL destination table name, if does not exist will be created
#' by [DBI::dbWriteTable()] in callback function
#' @param truncate_table boolean [TRUE]/ [FALSE], [TRUE] will truncate the SQL table name supplied, default FALSE
#' @param date_stamp date stamp to stamp date file was loaded, suggest [Sys.Date()]
#' @param write_parquet TRUE/ FALSE only, TRUE will write parquet files in chunks and combine them
#' without loading into local memory, default FALSE, FALSE will just preform SQL upload in callback
#' @param backup_filepath file path to folder or location where you want parquet backups to be written
#' do not include file name or .parquet extension as file names are created using pattern and name,
#' default NULL in case of write_parquet = FALSE
#' @param backup_name string name that the file name will be not including file path filepath is defined
#' by argument backup_file path
#' default [NULL] in case of write_parquet = FALSE
#' @param pattern a pattern to write temporary chunk parquet files with and to search for them when
#'  combining the parquet files e.g. pattern = "temp" - looks for files with temp in the file name
#' or including file extension e.g. backup_name = "example_file" is for "blah/example_file.parquet"
#' default NULL in case of write_parquet = FALSE
#' @param file_remove removes parquet chunk files written in callback function, if argument
#' write_parquet = TRUE
#' @param chunk_size chunk size for [readr] to read .csv file and for callback to send to SQL,
#'  default 50,000
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#' t <- tempdir(check = TRUE)
#' t1 <- paste0(t, "\\")
#' # TRUNCATE TABLE does not exist in SQLite so cannot be shown in examples
#' sql_upload_csv_chunked(readr::readr_example("mtcars.csv"),
#'   col_list = NULL,
#'   con = con,
#'   schema = NULL,
#'   table_name = "example",
#'   truncate_table = FALSE,
#'   write_parquet = TRUE,
#'   backup_filepath = t1,
#'   backup_name = "test_txt",
#'   pattern = "temp_chunk_parquet_file",
#'   file_remove = TRUE,
#'   chunk_size = 10
#' )
#'
#' res <- DBI::dbReadTable(con, "example")
#' res <- dplyr::as_tibble(res) # Otherwise prints all 32 rows as df
#' res
#' arrow::read_parquet(paste0(t1, "test_txt.parquet"))
#' DBI::dbDisconnect(con)
#'
#' @returns TRUE invisibly and a SQL table filled with values from csv file and if write_parquet = TRUE
#' a parquet file backup of the .csv file
#' @description
#' `r lifecycle::badge('experimental')`
#' \itemize{
#' \item Function to read .csv files in chunks, upload to a destination SQL table, defined by
#' a DBI connection object. Can also write parquet backups of the .csv file which are a
#' more efficient way to store data than the .csv files. These are written in chunks by
#' the callback function used by [readr::read_csv_chunked()] these can be combined and removed
#' if the file_remove argument is set to TRUE
#'
#' \item This function uses [readr::read_csv_chunked()] to read a csv file in chunks
#' without retaining the whole file in local memory to conserve RAM.
#' [DBI::dbWriteTable()] with option append = TRUE is used to write each chunk
#' to a destination SQL table. For R based SQL inserts, it is recommended
#' that you use a staging table as R and SQL data types do not always align.
#' Parquet backup or file conversion functionality
#' This function has optional arguments to allow for the writing of chunked
#' parquet backups using [arrow::write_parquet()] these are then combined using
#' [arrow::open_dataset()] and [arrow::write_parquet()] this is recommended because
#' parquet is a more efficient file format
#' than csv
#' }
#' @details
#' Some internal parameters are fed to internal callback function. This function
#' should not need to be used by end users but this is a note for developers
#' see epidm:::callback()
#'
#' @export
#' @seealso [sql_upload_csv_chunked()] [combine_parquet_files()] [callback()]
#' [arrow::write_parquet()] [arrow::read_parquet()] [DBI::dbWriteTable()]
#' [readr::read_csv_chunked()] [options()]
#'
sql_upload_csv_chunked <- function(input_filename,
                                   col_list = NULL,
                                   con,
                                   schema = "dbo",
                                   table_name,
                                   truncate_table = FALSE,
                                   date_stamp = NULL,
                                   write_parquet = FALSE,
                                   backup_filepath = NULL,
                                   backup_name = NULL,
                                   pattern = NULL,
                                   file_remove = FALSE,
                                   chunk_size = 50000) {
  # Error handling
  if (write_parquet) {
    assertthat::assert_that(
      assertthat::not_empty(pattern),
      msg = "argument pattern is NULL and write_parquet is TRUE: add argument pattern"
    )
    assertthat::assert_that(
      assertthat::not_empty(backup_name),
      msg = "argument backup_name is NULL and write_parquet is TRUE: add argument backup_name"
    )
    assertthat::assert_that(
      !is.null(backup_filepath),
      msg = "argument backup_filepath is NULL and write_parquet is TRUE: add argument backup_filepath"
    )
    assertthat::assert_that(
      stringi::stri_length(pattern) >= 10,
      msg = "arguement pattern is less than 10 characters may lead to unintended files being combined
      or removed and may cause errors or corruption of data. Suggest use of a pattern of longer than
      15 characters with a unique name"
    )
    assertthat::assert_that(
      !(stringi::stri_detect_fixed(str = backup_name, pattern = ".parquet") |
        stringi::stri_detect_fixed(str = backup_name, pattern = ".pqt")),
      msg = "parquet file extension added to backup name, .parquet extension
      is automatically added in combine_parquet_files()"
    )
  }
  # Set options for callback function: Schema, staging table name and backup file path
  op <- options(
    "con" = con,
    "schema" = schema,
    "table_name" = table_name,
    "date_stamp" = date_stamp,
    "backup_filepath" = backup_filepath,
    "pattern" = pattern,
    "backup_name" = backup_name,
    "write_parquet" = write_parquet
  )
  # Truncates staging table
  if (truncate_table) {
    DBI::dbExecute(con, glue::glue("TRUNCATE TABLE {schema}.{table_name}"))
  }

  readr::read_csv_chunked(
    file = input_filename,
    callback = readr::SideEffectChunkCallback$new(callback),
    chunk_size = chunk_size, # Can decide on optimum chunk size
    col_types = col_list,
    na = c("", "NA", "-", " ", "\t"), # values to convert to NA
    trim_ws = TRUE
  )
  if (write_parquet) {
    combine_parquet_files(
      path = backup_filepath,
      pattern = pattern,
      backup_name = backup_name,
      remove_old = file_remove
    )
  }
  on.exit(options(op))
  return(invisible(TRUE))
}
