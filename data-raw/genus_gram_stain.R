## code to prepare `genus_gram_stain` dataset goes here

## allow file to be maintained as csv
ggs <- system.file(
  "extdata",
  "genus_gram_stain.csv",
  package = "epidm"
)

genus_gram_stain <- read.csv(
  ggs,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

genus_gram_stain$gram_pos <- ifelse(genus_gram_stain$gram_stain=="positive",1,0)
genus_gram_stain$gram_neg <- ifelse(genus_gram_stain$gram_stain=="negative",1,0)
genus_gram_stain$gram_stain <- toupper(genus_gram_stain$gram_stain)

genus_gram_stain <- genus_gram_stain[order(organism_genus),]

usethis::use_data(genus_gram_stain, overwrite = TRUE)
