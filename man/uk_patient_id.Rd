% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/patient_id.R
\name{uk_patient_id}
\alias{uk_patient_id}
\title{Patient ID record grouping}
\usage{
uk_patient_id(
  x,
  nhs_number,
  hospital_number,
  date_of_birth,
  sex_mfu,
  forename = "NONAME",
  surname = "NONAME",
  .sortOrder,
  .keepValidNHS = FALSE,
  .forceCopy = FALSE,
  .experimental = FALSE
)
}
\arguments{
\item{x}{a data.frame or data.table containing the cleaned line list}

\item{nhs_number}{a column as a character containing the patient NHS numbers}

\item{hospital_number}{a column as a character containing the patient
Hospital numbers}

\item{date_of_birth}{a column as a date variable containing the patient
date of birth in date format}

\item{sex_mfu}{column as a character containing the patient sex;
NOTE only works if coded only as character versions of Male/Female/Unknown;
does not currently work with additional options \verb{#future update}}

\item{forename}{a column as a character containing the patient forename;
leave as NONAME if unavailable}

\item{surname}{a column as a character containing the patient surname;
leave as NONAME if unavailable}

\item{.sortOrder}{optional; a column as a character to allow a sorting
order on the id generation}

\item{.keepValidNHS}{optional, default FALSE; set TRUE if you wish to retain
the column with the NHS checksum result stored as a BOOLEAN}

\item{.forceCopy}{optional, default FALSE; TRUE will force data.table to take a copy
instead of editing the data without reference}

\item{.experimental}{optional, default FALSE; TRUE will enable the
experimental features for recoding NA values based on the mode}
}
\value{
A dataframe with one new variable:
\describe{
\item{\code{id}}{a unique patient id}
\item{\code{valid_nhs}}{if retained using argument \code{.keepValidNHS=TRUE}, a
BOOLEAN containing the result of the NHS checksum validation}
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Groups patient records from multiple isolates with a single integer patientID
by grouping patient identifiers.

Grouping is based on five stages:
\enumerate{
\item matching nhs number and date of birth
\item Hospital number &  Date of Birth
\item NHS number & Hospital Number
\item Date of Birth & Surname IF nhs unknown
\item Sex & Date of Birth & Fuzzy Name
}

Identifiers are copied over where they are missing or invalid to the grouped
records.
}
\examples{
id_test <- data.frame(
  nhs_n = c(
    9434765919,9434765919,9434765919,NA,NA,
    3367170666,5185293519,5185293519,5185293519,8082318562,NA,NA,NA
  ),
  hosp_n = c(
    '13','13','13','UNKNOWN','13','13','13','31','31','96','96',NA,'96'),
  sex = c(rep('F',6),rep('Male',4), 'U', 'U', 'M'),
  dateofbirth = as.Date(
    c(
      '1988-10-06','1988-10-06','1900-01-01','1988-10-06','1988-10-06',
      '1988-10-06','1988-10-06','1988-10-06','1988-10-06','2020-01-28',
      '2020-01-28','2020-01-28','2020-01-28'
    )
  ),
  firstname = c(
    'Danger','Danger','Denger','Danger','Danger','DANGER','Danger',
    'Danger','Danger','Crazy','Crazy','Krazy','C'
  ),
  lastname = c(
    'Mouse','Mause','Mouse','Moose','Moose','Mouse','MOUSe',
    'Mouse','Mouse','Frog','FROG','Frug','Frog'
  ),
  testdate = sample(seq.Date(Sys.Date()-21,Sys.Date(),"day"),13,replace = TRUE)
)
uk_patient_id(x = id_test,
              nhs_number = 'nhs_n',
              hospital_number = 'hosp_n',
              forename = 'firstname',
              surname = 'lastname',
              sex_mfu = 'sex',
              date_of_birth = 'dateofbirth',
              .sortOrder = 'testdate')[]

}
