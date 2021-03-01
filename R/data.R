#' Bacterial Genus Gram Stain Lookup Table
#'
#' A reference table of bacterial gram stain results by genus
#' to allow faster filtering of bacterial results.
#' This dataset has been maintained manually against the PHE SGSS database.
#' If there are organisms missing, please raise and issue or push request on the
#' \href{https://github.com/alexbhatt/epidm}{epidm GitHub}
#'
#' @format A data frame with four columns
#' \describe{
#' \item{organism_genus}{The bacterial genus}
#' \item{gram_stain}{A character string to indicate POSITIVE or NEGATIVE type}
#' \item{gram_positive}{A 0/1 flag to indicate if the genus is gram positive}
#' \item{gram_negative}{A 0/1 flag to indicate if the genus is gram negative}
#' }
#'
"genus_gram_stain"

#' Respeciated organisms
#'
#' Occasionally, research shows that two organisms, previously thought to be
#' different are in fact one and the same. The reverse is also true.
#' This is  a manually updated list.
#' If there are organisms missing, or new respeciates to be added,
#' please raise and issue or push request on the
#' \href{https://github.com/alexbhatt/epidm}{epidm GitHub}
#'
#' @format
#' \describe{
#' \item{previous_organism_name}{What the organism used to be known as, in the form GENUS SPECIES}
#' \item{organism_species_name}{What the organism is known as now, in the form GENUS SPECIES}
#' \item{organism_genus_name}{The genus of the recoded organism}
#' \item{genus_change}{A 0/1 flag to indicate if the genus has changed}
#' \item{genu_all_species}{A 0/1 flag to indicate if all species under that genus should change}
#' }
#'
"respeciate_organism"
