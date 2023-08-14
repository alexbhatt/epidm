#' @title Combines parquet file chunk datasets saved across multiple parquet files
#' @author Owen Pullen
#' @param path Reads parquet files, combines them into one single parquet file and removes old files
#' @param pattern pattern to look for in parquet files to combine
#' e.g. pattern = "temp" - looks for files with temp in the file name
#' @param backup_name name of file this is a string name that the file name will be not including file path
#' or including file extension e.g. backup_name = "example_file" is for "blah/example_file.parquet"
#' @param remove_old removes chunked parquet files written by callback function
#' @returns invisibly character vector of parquet files that were merged and a single parquet file (saved/written)
#' @description
#' Combine parquet files into larger parquet files - useful for when writing parquet files in chunks
#' then later combining chunks into single file
#' The ordering may fail with a linux OS
#' @examples
#' t <- tempdir(check = TRUE)
#' t1 <- paste0(t, "\\")
#' iris_1 <- dplyr::filter(iris, Species == "setosa")
#' iris_2 <- dplyr::filter(iris, Species == "virginica")
#' arrow::write_parquet(iris_1, paste0(t1, "input_1.parquet"))
#' arrow::write_parquet(iris_2, paste0(t1, "input_2.parquet"))
#' combine_parquet_files(path = t1,
#'                       pattern = "input",
#'                       backup_name = "sample_of_iris",
#'                       remove_old = TRUE
#'                       )
#'
#' @export
#'
combine_parquet_files <- function(path, pattern, backup_name, remove_old = FALSE) {
  sources <- list.files(path = path, pattern = pattern, full.names = TRUE)
  sources <- sources[order(file.info(sources)$ctime, grep("[0-9]", sources))]
  arrow::write_parquet(arrow::open_dataset(sources, format = "parquet"),
                       sink = paste0(path, backup_name, ".parquet")
  )
  if (remove_old) {
    file.remove(sources)
  }
  return(invisible(sources))
}
