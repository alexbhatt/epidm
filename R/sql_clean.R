#'
#' @title Clean and Read a SQL query
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A utility function to read in a SQL query from a character object, clipboard
#' or text file and remove all comments for use with database query packages
#'
#' @importFrom readr read_lines
#'
#' @param sql a SQL file or text string
#'
#' @examples
#' testSQL <- c(
#' "/*********",
#' "INTRO HEADER COMMENTS",
#' "*********/","  SELECT ",
#' "  [VAR 1]  -- with comments",
#' ",[VAR 2]",",[VAR 3]","FROM DATASET ",
#' "-- output here")
#' sql_clean(testSQL)
#'
#' @return a cleaned SQL query without comments as a character string
#' @export
#'

sql_clean <- function(sql) {

  ## a character string
  if(length(sql)==1){
    ## thats a SQL query stored in a .sql or .txt file
    if(grepl(".(sql|txt)$",sql,ignore.case=TRUE)){
      x <- readr::read_lines(sql)
    } else {
      ## or just a SQL query as text eg. 'SELECT * FROM TABLE'
      x <- sql
    }
  } else {
    ## or a list/vector of character strings that need to be put together
    x <- readr::read_lines(sql)
  }

  # note these gsubs could be combined with the next line separated with |
  # remove comments
  x <- gsub("--.*","",x)

  # breaks, tabs and carriage returns; separated with | in regex
  x <- gsub("\\r|\\t|\\n"," ",x)

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
