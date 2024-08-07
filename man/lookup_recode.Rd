% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lookup_recode.R
\name{lookup_recode}
\alias{lookup_recode}
\title{Lookup table switch handler}
\usage{
lookup_recode(
  src,
  type = c("species", "specimen", "inpatient_admission_method",
    "inpatient_discharge_destination", "ecds_destination_code", "manual"),
  .import = NULL
)
}
\arguments{
\item{src}{a character, vector or column containing the value(s) to be referenced}

\item{type}{a character value to denote the lookup table used}

\item{.import}{a list  in the order list(new,old) containing the
values for another lookup table existing in the environment}
}
\value{
a list object of the recoded field
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
A function to call an epidm lookup table and
recode  where we are aware of a new value.

Built in are  the organism re-classifications and specimen_type groupings
and a manual mode.
}
\examples{
df <- data.frame(
  spec = c(
    sample(grep(")",
                respeciate_organism$previous_organism_name,
                value=TRUE,
                invert = TRUE),
           9),
    "ESCHERICHIA COLI","SARS-COV-2","CANDIDA AUREUS"),
  type = sample(specimen_type_grouping$specimen_type,12),
  date = sample(seq.Date(from = Sys.Date()-365,
                         to = Sys.Date(),
                         by = "day"),12)
)
df <- df[order(df$date),]

# show the data before the changes
df

# check the lookup tables
# observe the changes
head(respeciate_organism[1:2])
df$species <- lookup_recode(df$spec,'species')
df[,c('spec','species','date')]

head(specimen_type_grouping)
df$grp <- lookup_recode(df$type,'specimen')
df[,c('species','type','grp','date')]

# for a tidyverse use
# df \%>\% mutate(spec=lookup_recode(spec,'species))

# manual input of your own lookup
# .import=list(new,old)
lookup_recode(
  "ALCALIGENES DENITRIFICANS",
  type = 'manual',
  .import=list(respeciate_organism$organism_species_name,
               respeciate_organism$previous_organism_name)
  )

}
