#' @title Synthetic Lab Data for epidm
#'
#' @description
#' A dataset containing synthetic lab data for testing epidemiological data
#' transformation functions.
#'
#' @name lab_data
#'
#' @docType data
#'
#' @usage data(lab_data)
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{nhs_number}{NHS number}
#'   \item{local_patient_identifier}{Patient identifier such as hospital number}
#'   \item{patient_birth_date}{Date of birth of the patients.}
#'   \item{sex}{Gender of the patients (Factor with levels: "Female", "Male").}
#'   \item{surname}{Patient surname}
#'   \item{forename}{Patient forename}
#'   \item{organism_species_name}{Organism species name (Factor with levels: "KLEBSIELLA PNEUMONIAE").}
#'   \item{specimen_date}{Date of specimen collection.}
#'   \item{specimen_type}{Type of specimen: BLOOD or URINE.}
#'   \item{lab_code}{Laboratory codes (Factor with unique levels).}
#'   \item{local_authority_name}{Name of the local authority.}
#'   \item{local_authority_code}{Code of the local authority.}
#' }
#'
#' @examples
#' data(lab_data)
#' head(lab_data)
#'
#' @keywords datasets
#'
"lab_data"
