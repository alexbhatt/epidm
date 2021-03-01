## code to prepare `genus_gram_stain` dataset goes here

## allow file to be maintained as csv
file.genus_gram_stain <- system.file(
  "extdata",
  "genus_gram_stain.csv",
  package = "epidm"
)

genus_gram_stain <- read.csv(
  file.genus_gram_stain,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

head(genus_gram_stain)

## cleanup the strings
genus_gram_stain$organism_genus <- toupper(genus_gram_stain$organism_genus)

genus_gram_stain$gram_stain <- ifelse(
  grepl("^p",genus_gram_stain$gram_stain,ignore.case = T),
  "POSITIVE",
  "NEGATIVE"
)

## create some flags
genus_gram_stain$gram_positive <- ifelse(genus_gram_stain$gram_stain=="POSITIVE",1,0)
genus_gram_stain$gram_negative <- ifelse(genus_gram_stain$gram_stain=="NEGATIVE",1,0)

genus_gram_stain <- with(genus_gram_stain,
                         genus_gram_stain[order(organism_genus),])

usethis::use_data(genus_gram_stain, overwrite = TRUE)
