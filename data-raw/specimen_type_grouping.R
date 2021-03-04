## code to prepare `specimen_type_grouping` dataset goes here


## allow file to be maintained as csv
file.specimen_type_grouping <- system.file(
  "extdata",
  "specimen_type_group.csv",
  package = "epidm"
)

specimen_type_grouping <- read.csv(
  file.specimen_type_grouping,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

specimen_type_grouping$specimen_type <- trimws(specimen_type_grouping$specimen_type)
specimen_type_grouping$specimen_group <- trimws(specimen_type_grouping$specimen_group)

head(specimen_type_grouping)

usethis::use_data(specimen_type_grouping, overwrite = TRUE)
