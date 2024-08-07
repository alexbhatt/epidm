% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_readwrite.R
\name{sql_read}
\alias{sql_read}
\title{Read a table from a SQL database}
\usage{
sql_read(server, database, sql)
}
\arguments{
\item{server}{a string containing the server connection}

\item{database}{a string containing the database name within the data store}

\item{sql}{a string containing a SQL query or to a .sql/.txt SQL query}
}
\value{
a table from a SQL database
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Read a table object to a SQL database. Acts a wrapper for odbc and DBI
packages.
}
\seealso{
sql_clean sql_connect
}
