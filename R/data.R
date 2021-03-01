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