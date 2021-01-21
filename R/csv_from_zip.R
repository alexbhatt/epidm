#' @title Download a csv from a zip
#' @param x a zip file from the web
#'
#' @return a zip file for ingestion into your chosen readr
#' @importFrom utils download.file unzip
#'
#' @examples head(read.csv(csv_from_zip("https://files.digital.nhs.uk/assets/ods/current/succarc.zip")))
#'
#' @export

csv_from_zip <- function(x) {
  loc.url <- x
  td <- tempdir()
  tf <- tempfile(tmpdir=td, fileext=".zip")
  utils::download.file(loc.url, tf)
  fname <- unzip(tf, list=TRUE)$Name[grep("csv|xls",unzip(tf,list=T)$Name)]
  utils::unzip(tf, files=fname, exdir=td,overwrite=TRUE)
  fpath <- file.path(td, fname)
  return(fpath)
}

