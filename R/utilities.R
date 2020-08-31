testSQL <- c(
  "/*********",
  "INTRO HEADER COMMENTS",
  "*********/",
  "SELECT ",
  "[VAR 1]  -- with comments",
  ",[VAR 2]",
  ",[VAR 3]",
  "FROM DATASET ",
  "-- output here"
)

## SQL reader; no comments
read_sql <- function(x) {
  if(grepl(".sql",x)){
    sql <- readLines(x)
    sql <- gsub("--.*","",sql)
    sql <- gsub("--.*","",sql)
    sql <- gsub("\\r","",sql)
    sql <- gsub("\\t","",sql)
    sql <- gsub("\\n","",sql)
    sql <- paste(sql,collapse=" ")
    sql <- gsub("/\\*(.|\n)*?\\*/","",sql)
    sql <- trimws(sql)
    return(sql)
  } else {
    stop("input must be .sql file")
  }
}

read_sql("../covid/sgss-sus-linkage/sql/ho_covid_extract.sql")

## reading a csv/xlsx from zip
## requires read_csv or read_xlsx to be used with output.
csv_from_zip <- function(x) {
  loc.url <- x
  td <- tempdir()
  tf <- tempfile(tmpdir=td, fileext=".zip")
  download.file(loc.url, tf)
  fname <- unzip(tf, list=TRUE)$Name[1]
  unzip(tf, files=fname, exdir=td,overwrite=TRUE)
  fpath <- file.path(td, fname)
  return(fpath)
}
