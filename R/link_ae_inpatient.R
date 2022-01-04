#' Link A&E to Inpatient records
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' Link together ECDS A&E records to HES/SUS inpatient records on
#'   NHS number, Hospital Number and Date of Birth.
#'
#' @import data.table
#'
#' @param ae_data the ECDS A&E dataset
#' @param ae_in the ECDS arrival date
#' @param ae_out the ECDS discharge date
#' @param inpatient_data the HES/SUS inpatient dataset
#' @param spell_id the HES/SUS spell id
#' @param nhs_number a vector containing the columns for the NHS numbers
#' @param hospital_number a vector containing the columns for the Hospital numbers
#' @param patient_dob a vector containing the columns for the date of birth
#' @param organisation_code a vector containing the columns for the organisation codes
#' @param .forceCopy a boolean to control if you want to copy the dataset before
#'   linking together
#'
#' @seealso group_time continuous_inpatient_spells
#'
#' @return a patient level linked hospital record
#' @export
#'
#'
link_ae_inpatient <- function(
  ae_data,
  ae_in,
  ae_out,
  inpatient_data,
  admission_date,
  spell_id,
  nhs_number=c('nhs_number','nhs_number'),
  hospital_number=c('local_patient_identifier','local_patient_identifier'),
  patient_dob=c('patient_birth_date','date_birth'),
  org_code=c('organisation_code_of_provider','organisation_code_code_of_provider'),
  .forceCopy=FALSE
){

  if(.forceCopy){
    inpatient_data <- data.table::copy(inpatient_data)
    ae_data <- data.table::copy(ae_data)
  } else {
    data.table::setDT(inpatient_data)
    data.table::setDT(ae_data)
  }

  ## allow people to match on either A&E admisison or discharge date
  ae_data <- data.table::rbindlist(
    list(ae_data[arrival_date != departure_date,link_date := arrival_date],
         ae_data[,link_date := departure_date])
  )

  ## dont want to ovewrite the admission date; so create a new one for linking
  inpatient_data[, link_date := spell_start_date]

  ## valid nhs links
  aeNHS <- ae_data[!is.na(nhs_number),]
  inNHS <- inpatient_data[!is.na(nhs_number),]

  ## valid hospital number links
  aeHOS <- ae_data[is.na(nhs_number) & !is.na(hospital_number),]
  inHOS <- inpatient_data[is.na(nhs_number) & !is.na(hospital_number),]

  link <- data.table::merge.data.table(
    x = aeNHS,
    y = inNHS,
    by.x = c("nhs_number",
             "patient_birth_date",
             "organisation_code_of_provider",
             "link_date"),
    by.y = c("nhs_number",
             "birth_date",
             "organisation_code_code_of_provider",
             "link_date"),
    suffixes = c("_ecds","_sus"),
    all = TRUE,
    allow.cartesian = TRUE
  )

  link <- data.table::rbindlist(
    list(
      aeNHS[inNHS,
           on=.(nhs_number,
                patient_birth_date = birth_date,
                organisation_code_of_provider = organisation_code_code_of_provider,
                link_date),
           allow.cartesian = TRUE,
           nomatch = NA,
           mult = "all"
      ],
      aeHOS[inHOS,
           on=.(hospital_number,
                patient_birth_date = birth_date,
                organisation_code_of_provider = organisation_code_code_of_provider,
                link_date),
           allow.cartesian = TRUE,
           nomatch = NA,
           mult = "all"
      ]
    )
  )

  return(link)
}
