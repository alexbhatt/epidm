% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_connect.R
\name{sql_connect}
\alias{sql_connect}
\title{Connect to a SQL database}
\usage{
sql_connect(server, database)
}
\arguments{
\item{server}{a string containing the server connection;
note that servers may require the use of double backslash \verb{\\\\}}

\item{database}{a string containing the database name within the data store}
}
\value{
a SQL connection object
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

An function to help setup connections to SQL databases
acting as a wrapper for the odbc and DBI packages. Used by other sql_*
tools within epidm. This uses the credential manager within the system and
assumes you are using a trusted connection.
}
\examples{
\dontrun{
sql <- list(
  dsn = list(ser = 'covid.ukhsa.gov.uk',
             dbn = 'infections')
)

sgss_con = sql_connect(server = sql$dsn$ser, database = sql$dsn$dbn)
}

}
\seealso{
sql_clean sql_read sql_write
}
