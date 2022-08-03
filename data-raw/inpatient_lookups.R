## Prepare Inpatient lookups
## These are internal to the package, and are used by the lookup_recode function
## HES and SUS compatible

group_inpatient_discharge_destination <- data.frame(
  value = c(
    "19","29",
    "50","51","52","53","30","48","49",
    "54","85",
    "65","66",
    "84","87","88",
    "37","38",
    "79",
    "98",
    "99",NA
  ),
  discharge_destination = c(
    rep("Patient returned home",2),
    rep("Patient discharged to another NHS Provider",7),
    rep("Patient discharged to Care Home",2),
    rep("Patient discharged to Local Authority Care",2),
    rep("Patient discharged to non-NHS Hospital",3),
    rep("Patient in justice system",2),
    "Patient died",
    "Patient still in Hospital",
    rep("Not known",2)
  )
)



group_inpatient_admission_method <- data.frame(
  value = c(
    "11","12","13",
    "21","22","23","24","28",
    "2A","2B","2C","2D",
    "31","32",
    "82","83",
    "81",
    NA
  ),
  admission_method = c(
    rep("Elective admission",3),
    rep("Emergency admission",5),
    rep("Emergency admission",4),
    rep("Maternity admission",2),
    rep("Baby born in hospital",2),
    "Patient transfer into hospital",
    "Not known"
  )
)

usethis::use_data(group_inpatient_admission_method,
                  group_inpatient_discharge_destination,
                  internal = TRUE)
