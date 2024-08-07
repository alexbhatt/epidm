% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_readwrite.R
\name{sql_write}
\alias{sql_write}
\title{Write a table to a SQL database}
\usage{
sql_write(x, server, database, tablename)
}
\arguments{
\item{x}{a data.frame/data.table/tibble object}

\item{server}{a string containing the server connection}

\item{database}{a string containing the database name within the data store}

\item{tablename}{a string containing the chosen SQL database table name}
}
\value{
writes a data.frame/data.table/tibble to a SQL database
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Write a table object to a SQL database. Acts a wrapper for odbc and DBI
packages with additional checks to ensure upload completes.
}
