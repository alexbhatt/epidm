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

#' Specimen type grouping
#'
#' In order to help clean up an analysis based on a group of specimen types,
#' a lookup table has been created to help group sampling sites.
#' This is  a manually updated list.
#' If there are organisms missing, or new respeciates to be added,
#' please raise and issue or push request on the
#' \href{https://github.com/alexbhatt/epidm}{epidm GitHub}
#' @format
#' \describe{
#' \item{specimen_type}{The primary specimen type with detail}
#' \item{specimen_group}{A simple grouping of like specimen sites}
#' }
"specimen_type_grouping"

#' Inpatient admission methods
#'
#' In order to group hospital inpatient admissions into human readable groups,
#' a lookup table has been created. These work with Hospital Episode Statistics
#' (HES) and Secondary Use Services (SUS) data with the admission_method fields.
#'
#' @format
#' \describe{
#' \item{code}{the admission_method code}
#' \item{admission_method}{the admission_method grouping as a human readable string}
#' }
"group_inpatient_admission_method"

#' Inpatient discharge destination
#'
#' In order to group hospital inpatient discharge destination into human
#' readable groups, a lookup table has been created. These work with
#' Hospital Episode Statistics (HES) and Secondary Use Services (SUS)
#' data with the discharge_destination fields.
#'
#' @format
#' \describe{
#' \item{code}{the discharge_destination code}
#' \item{discharge_destination}{the discharge_destination grouping as a human readable string}
#' }
"group_inpatient_discharge_destination"

#' A&E attendance discharge destination
#'
#' In order to group A&E discharge destination from SNOWMED into human
#' readable groups, a lookup table has been created. These work with
#' Emergency Care Dataset (ECDS)
#' data with the destination_code field to show where a patient goes after
#' discharge from A&E.
#'
#' @format
#' \describe{
#' \item{code}{the ECDS destination_code}
#' \item{destination_code}{the destination grouping as a human readable string}
#' }
"group_inpatient_discharge_destination"
