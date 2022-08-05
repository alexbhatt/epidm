#' Link A&E to Inpatient records
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' Link together ECDS A&E records to HES/SUS inpatient records on
#'   NHS number, Hospital Number and Date of Birth. To note that the inpatient
#'   records should already be aggregated into spells at the desired level.
#'
#' @import data.table
#'
#' @param ae_data the ECDS A&E dataset
#' @param ae_arrival_date the ECDS arrival date
#' @param ae_ae_departure_date the ECDS discharge date
#' @param inp_data the HES/SUS inpatient dataset
#' @param inp_spell_id the HES/SUS spell id
#' @param nhs_number a vector containing the column names for the NHS numbers in the order `c('ae','inpatient')`
#' @param hospital_number a vector containing the column names for the Hospital numbers in the order `c('ae','inpatient')`
#' @param patient_dob a vector containing the column names for the date of birth in the order `c('ae','inpatient')`
#' @param org_code a vector containing the column names for the organisation codes in the order `c('ae','inpatient')`
#' @param admission_date a string containing the inpatient (HES/SUS) admission date column name
#' @param .forceCopy a boolean to control if you want to copy the dataset before
#'   linking together
#'
#' @seealso group_time continuous_inpatient_spells
#'
#' @return a patient level linked hospital record
#'

link_ae_inpatient <- function(
  ae_data,
  ae_arrival_date,
  ae_departure_date,
  inp_data,
  inp_inp_spell_start_date,
  inp_spell_id,
  nhs_number=c('nhs_number','nhs_number'),
  hospital_number=c('local_patient_identifier','local_patient_identifier'),
  patient_dob=c('date_birth','date_birth'),
  org_code=c('organisation_code_of_provider','organisation_code_code_of_provider'),
  .forceCopy=FALSE
){

  if(.forceCopy){
    inp_data <- data.table::copy(inp_data)
    ae_data <- data.table::copy(ae_data)
  } else {
    data.table::setDT(inp_data)
    data.table::setDT(ae_data)
  }

  ## allow people to match on either A&E admisison or discharge date
  ae_data <- data.table::rbindlist(
    list(ae_data[ae_arrival_date != ae_departure_date,
                 link_date := ae_arrival_date],
         ae_data[, link_date := ae_departure_date])
  )

  ## dont want to ovewrite the admission date; so create a new one for linking
  inp_data[, link_date := inp_spell_start_date]

  ## valid nhs links
  aeNHS <- ae_data[!is.na(nhs_number[1]),]
  inNHS <- inp_data[!is.na(nhs_number[2]),]

  ## valid hospital number links
  aeHOS <- ae_data[is.na(nhs_number[1]) & !is.na(hospital_number[1]),]
  inHOS <- inp_data[is.na(nhs_number[2]) & !is.na(hospital_number[2]),]

  link <- data.table::merge.data.table(
    x = aeNHS,
    y = inNHS,
    by.x = c(nhs_number[1],
             patient_dob[1],
             org_code[1],
             "link_date"),
    by.y = c(nhs_number[2],
             patient_dob[2],
             org_code[2],
             "link_date"),
    suffixes = c("_ae","_inp"),
    all = TRUE,
    allow.cartesian = TRUE
  )

  link <- data.table::rbindlist(
    list(
      data.table::merge.data.table(
        x = aeNHS,
        y = inNHS,
        by.x = c(nhs_number[1],
                 patient_dob[1],
                 org_code[1],
                 link_date),
        by.y = c(nhs_number[2],
                 patient_dob[2],
                 org_code[2],
                 link_date),
        allow.cartesian = TRUE,
        nomatch = NA,
        mult = "all"
      ),
      data.table::merge.data.table(
        x = aeNHS,
        y = inNHS,
        by.x = c(hospital_number[1],
                 patient_dob[1],
                 org_code[1],
                 link_date),
        by.y = c(hospital_number[2],
                 patient_dob[2],
                 org_code[2],
                 link_date),
        allow.cartesian = TRUE,
        nomatch = NA,
        mult = "all"
      )
      # aeNHS[inNHS,
      #      on=.(nhs_number[1] = nhs_number[2],
      #           patient_dob[1] = patient_dob[2],
      #           org_code[1] = org_code[2],
      #           link_date),
      #      allow.cartesian = TRUE,
      #      nomatch = NA,
      #      mult = "all"
      # ],
      # aeHOS[inHOS,
      #      on=.(hospital_number[1] = hospital_number[2],
      #           patient_dob[1] = patient_dob[2],
      #           org_code[1] = org_code[2],
      #           link_date),
      #      allow.cartesian = TRUE,
      #      nomatch = NA,
      #      mult = "all"
      # ]
    )
  )

  return(link)
}
