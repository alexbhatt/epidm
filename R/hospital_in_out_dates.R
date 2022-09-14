## STAGE 4.5: Hospital in/out dates ###############################################
# When retaining the final record the following criteria is used:
#  ALL RULES ARE WITHIN A SINGLE PATIENT
#  1 Current admissions take priority
#  2 When conflicting on the same day, SUS admissions take priority over ECDS
#    emergency care data
#  3 Where a patient has a linked A&E admission to a hospital inpatient stay,
#    the A&E admission date is used
#  4 Where a patient has a positive test between two hospital stays the most
#    recent completed hospital stay prior to the test is retained except if
#    the time between these events is greater than 14 days, then the first
#    admission following the test is retained
hospital_in_out_dates <- function(data,
                                  person_id = 'id',
                                  date = list(
                                    event = 'spec_date',
                                    ae_arrive = 'arrival_date',
                                    ae_depart = 'departure_date',
                                    in_spell_start = 'spell_start_date',
                                    in_spell_end = 'spell_end_date'
                                    )
                                  ){

  ## these are A&E attendances which result in inpatient stays
  ae_admission <- c("Admitted","Transfer")

  ## create the base values
  link[!is.na(aeIn) | !is.na(inIn),
       `:=`(
         hospital_in = data.table::fifelse(!is.na(aeIn),aeIn,inIn),
         hospital_out = data.table::fifelse(!is.na(inOut),inOut,aeOut)
       ),
       env = list(aeIn = date$ae_arrive,
                  aeOut = date$ae_depart,
                  inIn = date$in_spell_start,
                  inOut = date$in_spell_end)
  ]

  # sort the data
  setorder(link, get(person_id), hospital_in, hospital_out)

  # time to get funky with the hospital out dates to make sure they make sense
  # where we see people having more than one event `.N > 1, by id`
  link[!is.na(hospital_in),
       `:=`(
         ## if they have more than one hospital record AND the
         hospital_out = data.table::fifelse(.N > 1 &
                                  data.table::shift(hospital_in,type="lead") < hospital_out &
                                  !spec_date %within% lubridate::interval(hospital_in,hospital_out) &
                                  data.table::shift(source,type="lead")!="ECDS" & grepl("SUS",source),
                                data.table::shift(hospital_in,type="lead"),
                                hospital_out),
         hospital_out = data.table::fifelse(is.na(hospital_out),
                                data.table::fifelse(is.na(spell_end_date),
                                        departure_date,
                                        spell_end_date),
                                hospital_out)
       ),
       by = pid,
       env = list(pid = person_id)
  ]

  ## ECDS episodes where the SUS record has not yet come in
  ## if the patient was admitted as an inpatient, and its less than 60 days
  ## code them as still being an inpatient
  link[source=="ECDS",
       ecds_proxy_date := ecds_discharge == "Admitted" & difftime(Sys.Date(),
                                                                  spec_date,
                                                                  units="days") < 90
  ]

  link[ecds_proxy_date==TRUE,
       hospital_out := Sys.Date()]

  link[!is.na(hospital_in),
       `:=`(
         diff_pos_admit = as.integer(difftime(
           spec_date,
           hospital_in,
           units = "days"
         )),
         diff_pos_discharge = as.integer(difftime(
           spec_date,
           hospital_out,
           units = "days"
         )),
         length_of_stay = as.integer(difftime(
           data.table::fifelse(source=="ECDS" & ecds_discharge %in% ae_admission,
                   Sys.Date(),
                   hospital_out),
           hospital_in,
           units = "days"
         ))
       )
  ]

  link[!is.na(hospital_in),
       `:=`(
         pos_in_hospital = spec_date %within% lubridate::interval(hospital_in,hospital_out),
         still_in_hospital = data.table::fcase(
           grepl("SUS",source) & discharge_destination=="98",TRUE,
           source=="ECDS" & ecds_discharge %in% ae_admission,TRUE,
           default=FALSE),
         test_pre_admit = diff_pos_admit < 0,
         inpatient_stay = ecds_discharge %in% ae_admission | !is.na(discharge_destination)
       )
  ]
}
