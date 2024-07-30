## Prepare Inpatient lookups
## These are internal to the package, and are used by the lookup_recode function
## HES and SUS compatible

group_inpatient_discharge_destination <- rbind(
  data.frame(
    code = c("19", "29"),
    discharge_destination = "Patient returned home"
  ),
  data.frame(
    code = c("50", "51", "52", "53", "30", "48", "49"),
    discharge_destination = "Patient discharged to another NHS Provider"
  ),
  data.frame(
    code = c("54", "85"),
    discharge_destination = "Patient discharged to Care Home"
  ),
  data.frame(
    code = c("65", "66"),
    discharge_destination = "Patient discharged to Local Authority Care"
  ),
  data.frame(
    code = c("84", "87", "88"),
    discharge_destination = "Patient discharged to non-NHS Hospital"
  ),
  data.frame(
    code = c("37", "38"),
    discharge_destination = "Patient in justice system"
  ),
  data.frame(code = c("79"),
             discharge_destination = "Patient died"),
  data.frame(code = c("98"),
             discharge_destination = "Patient still in Hospital"),
  data.frame(code = c("99", NA),
             discharge_destination = "Not known")
)




group_inpatient_admission_method <- rbind(
  data.frame(
    code = c("11", "12", "13"),
    admission_method = "Elective admission"
  ),
  data.frame(
    code = c("21", "22", "23", "24", "28", "2A", "2B", "2C", "2D"),
    admission_method = "Emergency admission"
  ),
  data.frame(code = c("31", "32"),
             admission_method = "Maternity admission"),
  data.frame(code = c("82", "83"),
             admission_method = "Baby born in hospital"),
  data.frame(code = c("81"),
             admission_method = "Patient transfer into hospital"),
  data.frame(code = c(NA),
             admission_method = "Not known")
)

## These are for Emergency Care Data Set (ECDS) A&E attendance discharge codes
## These use the SNOWMED system

group_ecds_discharge_destination <-
  rbind(
    data.frame(
      code = c(
        "306689006", # to home
        "306691003", # to home
        "306694006", # to nursing home
        "306705005", # to policy
        "50861005", # to legal custody
        "989501000000106", # discharged for follow up by GP
        "1066111000000103", # referred for outpatient procedure
        "301791000000104", # referred for fracture clinic
        NA
      ),
      destination_code = "Discharged"
    ),
    data.frame(
      code = c("1066331000000109", # ED short stay
               "1066341000000100", # ambulance care
               "1066351000000102" # hospital at home service
               ),
      destination_code = "Ambulatory/Short stay"
    ),
    data.frame(
      code = c(
        "306706006", # admission to ward
        "1066361000000104", # admission to HDU
        "1066391000000105", # admission to ICU
        "1066371000000106", # admission to coronary care
        "1066381000000108", # special baby care unit
        "1066401000000108"  # neonate ICU
      ),
      destination_code = "Admitted"
    ),
    data.frame(code = c("19712007"),
               destination_code = "Transfer"),
    data.frame(code = c("305398007"),
               destination_code = "Died")
  )

usethis::use_data(
  group_inpatient_admission_method,
  group_inpatient_discharge_destination,
  group_ecds_discharge_destination,
  overwrite = TRUE
)
