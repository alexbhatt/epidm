% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{group_ecds_discharge_destination}
\alias{group_ecds_discharge_destination}
\title{A&E attendance discharge destination}
\format{
\describe{
\item{code}{the ECDS destination_code}
\item{destination_code}{the destination grouping as a human readable string}
}
}
\usage{
group_ecds_discharge_destination
}
\description{
In order to group A&E discharge destination from SNOWMED into human
readable groups, a lookup table has been created. These work with
Emergency Care Dataset (ECDS)
data with the destination_code field to show where a patient goes after
discharge from A&E.
}
\keyword{datasets}
