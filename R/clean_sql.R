#'
#' @title Clean and Read a SQL query
#'
#' A utility function to read in a SQL query from a character object, clipboard
#' or text file and remove all comments for use with database query packages
#'
#' @param sql a SQL file or text string
#' @importFrom readr read_lines
#'
#' @examples
#' testSQL <- c(
#' "/*********",
#' "INTRO HEADER COMMENTS",
#' "*********/","  SELECT ",
#' "  [VAR 1]  -- with comments",
#' ",[VAR 2]",",[VAR 3]","FROM DATASET ",
#' "-- output here")
#' clean_sql(testSQL)
#'
#' @return a cleaned SQL query without comments as a character string
#' @export


clean_sql <- function(sql) {
  # read in
  x <- readr::read_lines(sql)

  # note these gsubs could be combined with the next line separated with |
  # remove comments
  x <- gsub("--.*","",x)

  # breaks, tabs and carriage returns; separated with | in regex
  x <- gsub("\\r|\\t|\\n","",x)

  # single string
  x <- paste(x,collapse=" ")

  # remove multiline /**/ comments
  x <- gsub("/\\*(.|\n)*?\\*/","",x)

  # whitespace cleanup
  while(grepl("  ",x)){
    x <- gsub("  "," ",x)
  }
  x <- trimws(x)

  return(x)
}

