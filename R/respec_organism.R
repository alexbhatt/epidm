#' Respeciate organism
#'
#' A function to call the respeciated_organism lookup table and
#' recode organisms where we are aware of a new classification
#'
#' @param species a vector containing the organism species name
#'
#' @return a vector containing the updated scientific name for the organism
#' @export
#'
#' @examples
#' respec_example <- data.frame(
#'   sp=respeciate_organism$previous_organism_name,
#'   ge=gsub("([A-Za-z]+).*", "\\1",respeciate_organism$previous_organism_name)
#'   )
#' respec_example$new <- respec_org(respec_example$sp)
#' head(respec_example)


respec_org <- function(species) {

  ## calls upon the lookup table stored in the epidm package
  ## data(respeciate_organism)
  lk <- as.list(setNames(
    respeciate_organism$organism_species_name,
    respeciate_organism$previous_organism_name
  ))

  x <- unname(lk[species])

  return(x)

}
