## code to prepare `respeciate_organism` dataset goes here

## allow file to be maintained as csv
file.respeciate_organism <- system.file(
  "extdata",
  "respeciate_organism.csv",
  package = "epidm"
)

respeciate_organism <- read.csv(
  file.respeciate_organism,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

head(respeciate_organism)

## clean up text
respeciate_organism$previous_organism_name <- toupper(respeciate_organism$previous_organism_name)
respeciate_organism$organism_species_name <- toupper(respeciate_organism$organism_species_name)

## order it
respeciate_organism <- with(respeciate_organism,
                            respeciate_organism[order(organism_species_name,
                                                      previous_organism_name),]
                            )

## work with the genus now.
respeciate_organism$organism_genus_name <- stringr::word(respeciate_organism$organism_species_name)
respeciate_organism$genus_old <- stringr::word(respeciate_organism$previous_organism_name)

## has the genus changed?
respeciate_organism$genus_change <- with(
  respeciate_organism,
  ifelse(genus_old == organism_genus_name,
         0,
         1)
)

respeciate_organism$genus_old <- NULL

## do we want to recode all species within the genus?
respeciate_organism$genus_all_species <- with(
  respeciate_organism,
  ifelse(grepl("SP$",previous_organism_name),
         1,
         0
         )
  )

## printout some stats
dim(respeciate_organism)

respeciate_organism

usethis::use_data(respeciate_organism, overwrite = TRUE)
