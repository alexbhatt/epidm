#' @title Hospital IN/OUT dates
#'
#' @description This function helps to determine when a patient has been in
#' hospital across  spell aggregation.
#' When retaining the final record the following criteria is used:
#'  \describe{
#'    \item{"1"}{Current admissions take priority}
#'    \item{"2"}{When conflicting on the same day, inpatient admissions take priority over A&E emergency care data}
#'    \item{"3"}{Where a patient has a linked A&E admission to a hospital inpatient stay, the A&E admission date is used}
#'    \item{"4"}{Where a patient has a positive test between two hospital stays the most recent completed hospital stay prior to the test is retained except if the time between these events is greater than 14 days, then the first admission following the test is retained}
#'  }
#'
#' @param data the linked asset holding A&E and Inpatient data
#' @param person_id the column containing the unique patient ID
#' @param hospital a list containing the following items
#' \describe{
#'    \item{`org_code`}{the NHS trust organisation codes}
#'    \item{`event_date`}{the comparison date used; often `specimen_date`}
#'    \item{`ae_arrive`}{the ECDS arrival date}
#'    \item{`ae_depart`}{the ECDS discharge date}
#'    \item{`ae_discharge`}{the ECDS discharge status; recommend grouping from `epidm::lookup_recode`}
#'    \item{`in_spell_start`}{the HES/SUS spell start date; recommend after `epidm::group_time`}
#'    \item{`in_spell_end`}{the HES/SUS spell end date; recommend after `epidm::group_time`}
#'    \item{`in_discharge`}{the HES/SUS discharge destination code; recommend grouping from `epidm::lookup_recode`}
#'   }
#'
#' @seealso epidm::lookup_recode()
#' @seealso epidm::group_time()
#' @seealso epidm::cip_spells()
#'
#' @importFrom lubridate `%within%` interval
#'
#' @return  new date columns on the data.table for `hospital_in` and `hospital_out` and `hospital_event_rank`
#'
#' @examples
#' hospital_in_out_dates(link,
#' person_id = 'id',
#' hospital = list(
#'   org_code = 'organisation_code_of_provider',
#'   event_date = 'ev_date',
#'   ae_arrive = 'arrival_date',
#'   ae_depart = 'departure_date',
#'   ae_discharge = 'ecds_discharge',
#'   in_spell_start = 'spell_start_date',
#'   in_spell_end = 'spell_end_date',
#'   in_discharge = 'discharge_destination'
#' )[]
#'

hospital_in_out_dates <- function(data,
                                  person_id = 'id',
                                  hospital = list(
                                    org_code = 'organisation_code_of_provider',
                                    event_date = 'ev_date',
                                    ae_arrive = 'arrival_date',
                                    ae_depart = 'departure_date',
                                    ae_discharge = 'ecds_discharge',
                                    in_spell_start = 'spell_start_date',
                                    in_spell_end = 'spell_end_date',
                                    in_discharge = 'discharge_destination'
                                    )
                                  ){

  ## A&E discharge events resulting in an inpatient stay
  ae_admission <- c("Admitted","Transfer")

  ## ensure dates are in date format
  for(i in c(hospital$event_date,
             hospital$ae_arrive,
             hospital$ae_depart,
             hospital$in_spell_start,
             hospital$in_spell_end)) {

    # check if its a date class
    if(inherits(i,"Date")){
      link[,
           date := as.Date(date),
           env = list(date = i)
      ]
    }

  }

  ## source flag, this lets you determine which data its linked to
  link[, source := data.table::fcase(
    !is.na(ae_in) & !is.na(in_in), "ECDS:SUS",
    !is.na(ae_in) &  is.na(in_in), "ECDS",
    is.na(ae_in)  & !is.na(in_in), "SUS",
    default = NA
  ),
  env = list(ae_in = hospital$ae_arrive,
             in_in = hospital$in_spell_start)
  ]


  link[!is.na(ae_in) | !is.na(in_in),
       `:=`(
         hospital_in=fifelse(!is.na(ae_in),ae_in,in_in),
         hospital_out=fifelse(!is.na(in_out),in_out,ae_out)
       ),
       env = list(ae_in = hospital$ae_arrive,
                  ae_out = hospital$ae_depart,
                  in_in = hospital$in_spell_start,
                  in_out = hospital$in_spell_end)
  ]

  data.table::setorder(link, get(person_id), hospital_in, hospital_out)

  ## ECDS episodes where the SUS record has not yet come in
  ## if the patient was admitted as an inpatient, and its less than 60 days
  ## code them as still being an inpatient
  link[source=="ECDS",
       ecds_proxy_date := ae_dis == "Admitted" & difftime(Sys.Date(),
                                                          ev_date,
                                                          units="days") < 90,
       env = list(ae_dis = hospital$ae_discharge,
                  ev_date = hospital$event_date)
  ]

  link[ecds_proxy_date==TRUE,
       hospital_out := Sys.Date()]

  ## group togerher ECDS and SUS records where they overlap
  ## this allows for transfers across hospitals
  if(exists('org_code', where = hospital)){
    grp <- c(person_id, hospital$org_code)
  } else {
    grp <- c(person_id)
  }

  link <- epidm::group_time(x = link,
                            date_start = 'hospital_in',
                            date_end = 'hospital_out',
                            group_vars= grp,
                            indx_varname = "indx",
                            min_varname = "hospital_in2",
                            max_varname = "hospital_out2")

  data.table::setorder(link, get(patient_id), hospital_in, hospital$event_date)

  link[,
       c('hospital_in',
         'hospital_out'
       ) := NULL
  ]

  setnames(link,
           c("hospital_in2","hospital_out2"),
           c("hospital_in","hospital_out")
  )

  ## APPLY HOSPITAL EVENT ORDER #################################################
  ## create some key indicators

  link[!is.na(hospital_in),
       `:=`(
         diff_pos_admit = as.integer(difftime(
           hospital_in,
           ev_date,
           units = "days"
         )),
         diff_pos_discharge = as.integer(difftime(
           hospital_out,
           ev_date,
           units = "days"
         )),
         length_of_stay = as.integer(difftime(
           hospital_out,
           hospital_in,
           units = "days"
         ))
       ),
       env = list(
         ev_date = hospital$event_date
       )
  ]

  link[!is.na(hospital_in),
       `:=`(
         inpatient_stay = ecds_discharge %in% ae_admission | grepl("SUS",source),
         pos_in_hospital = ev_date %within% interval(hospital_in,hospital_out),
         still_in_hospital= fcase(
           grepl("SUS",source) & discharge_destination=="98",TRUE,
           source=="ECDS" & ecds_discharge %in% ae_admission,TRUE,
           default=FALSE),
         test_pre_admit = diff_pos_admit > 0,
         pos14_order = fifelse(
           is.na(hospital_out),
           FALSE,
           diff_pos_discharge %in% c(1:14)
         ),
         abs_discharge = abs(diff_pos_discharge),
         abs_admit = abs(diff_pos_admit)
       ),
       env = list(ae_dis = hospital$ae_discharge,
                  ev_date = hospital$event_date)
  ]

  ## order and mark valid entries
  data.table::setorderv(
    link,
    cols = c(
      new_unique_identifier,
      -valid_hospital_link,
      abs_admit,
      abs_discharge,
      -pos_in_hospital,
      -pos14_order,
      -hospital_in,
      -hospital_out,
      ev_date
    )
  )

  link <- unique(link,
                 by = c('new_unique_identifier',
                        'organisation_code_of_provider',
                        'ev_date',
                        'indx',
                        'hospital_in',
                        'hospital_out'))

  link[,N := .N, by = 'new_unique_identifier']

  link[,drop := N>1 & !is.na(source) & is.na(hospital_in)]

  link <- link[drop==FALSE,]

  link[valid_hospital_link==TRUE,
       hospital_event_rank := seq_len(.N),
       by = new_unique_identifier]

  link[, source := fifelse(is.na(source), "CASE",
                           paste0("CASE:",source))
  ]

  ## cleanup and remove temporary vars
  tmpcols <- grep("^tmp.",names(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(link)
}
