#' Link A&E to Inpatient records
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' Link together ECDS A&E records to HES/SUS inpatient records on
#'   NHS number, Hospital Number and Date of Birth and organisation code.
#'   To note that the inpatient records should already be aggregated into
#'   spells at the desired level (standard, CIP or Mega)
#'
#' @seealso group_time continuous_inpatient_spells
#'
#' @import data.table
#'
#' @param ae a list to provide data and columns for the A&E (ECDS) data; all arguments provided quoted unless specified
#'  \describe{
#'    \item{`data`}{the ECDS A&E dataset provided unquoted}
#'    \item{`arrival_date`}{the ECDS arrival date}
#'    \item{`departure_date`}{the ECDS discharge date}
#'    \item{`nhs_number`}{the patient NHS number}
#'    \item{`hospital_number`}{the patient Hospital numbers also known as the local patient identifier}
#'    \item{`patient_dob`}{patient date of birth}
#'    \item{`org_code`}{the NHS trust organisation codes}
#'   }
#' @param inp a list to provide data and columns for the inpatient (SUS/HES) data
#'   \describe{
#'   \item{`data`}{the HES/SUS inpatient dataset provided unquoted}
#'   \item{`spell_start_date`}{a string containing the inpatient (SUS/HES) admission date column name; all arguments provided quoted unless specified}
#'   \item{`spell_id`}{the HES/SUS spell id}
#'   \item{`nhs_number`}{the patient NHS number}
#'   \item{`hospital_number`}{the patient Hospital numbers also known as the local patient identifier}
#'   \item{`patient_dob`}{patient date of birth}
#'   \item{`org_code`}{the NHS trust organisation codes}
#'   }
#' @param .forceCopy a boolean to control if you want to copy the dataset before
#'   linking together
#'
#' @return a patient level linked hospital record
#'
#' @examples
#' sample_ae <- data.table::data.table(
#'   nhs_number = c("645114517",
#'                  "645114517","645114517","382940103","321908341",
#'                  "321908341","321908341","599534707",
#'                  "403454211","349959089","252341591","180554160",
#'                  "180554160","281980720","473372796","369773534",
#'                  "474044124","639064842","662468568","115641745",
#'                  "115641745","821084975","479637024",
#'                  "527021626","527021626","233168855","702650869",NA,
#'                  "537235036",NA,"517229961",NA,"480142132",
#'                  "368288558","554675340"),
#'   local_patient_identifier = c(NA,NA,NA,
#'                                "I3348707",NA,NA,NA,"P1350948",NA,NA,
#'                                "Q4157514",NA,NA,"D1101843","K2440769","E1366499",
#'                                NA,"K1494229","R4678220","J5206297","J5206297",
#'                                "S1945338","F2159102",NA,NA,"D6300794",NA,
#'                                "W1208900","Z4975449","G7439612","T1266485",
#'                                "N4842033","Q5566884","P2689566",NA),
#'   patient_birth_date = c("2021-03-03",
#'                          "2021-03-03","2021-03-03","2003-08-24",
#'                          "2001-06-21","2001-06-21","2001-06-21","1991-10-08",
#'                          "1987-02-03","1962-06-17","1991-10-07",
#'                          "1985-10-16","1985-10-16","1990-09-24","1984-11-14",
#'                          "1994-05-05","1999-08-11","1983-01-04",
#'                          "2017-06-01","1975-09-04","1975-09-04","1993-07-13",
#'                          "2014-01-05","1995-09-30","1995-09-30",
#'                          "1976-06-25","2000-06-02","2017-06-11","2007-05-03",
#'                          "1986-08-28","2016-10-14","2016-02-04",
#'                          "2004-03-02","1979-01-17","1974-06-14"),
#'   organisation_code = c("P3P","P3P",
#'                         "P3P","Z4R","A9I","A9I","A9I","Z4R","V5T",
#'                         "Z9V","P3P","T7N","T7N","V2P","T4H","V9Y",
#'                         "Z7N","W6Y","G2H","V5T","V5T","W6Y","J6J",
#'                         "J6J","J6J","L4Q","P3P","F0N","A6C","O1A",
#'                         "F0N","O2R","W6Y","V0R","O1A"),
#'   arrival_date = c("2022-05-10",
#'                    "2022-05-29","2022-08-03","2022-05-17",
#'                    "2022-05-07","2022-05-07","2022-05-07","2022-05-23",
#'                    "2022-05-13","2022-06-04","2022-05-14",
#'                    "2022-05-17","2022-06-05","2022-05-25","2022-05-24",
#'                    "2022-06-09","2022-06-18","2022-06-11",
#'                    "2022-06-22","2022-06-20","2022-07-18","2022-06-09",
#'                    "2022-06-05","2022-06-26","2022-06-11",
#'                    "2022-06-25","2022-06-10","2022-06-12","2022-06-16",
#'                    "2022-07-10","2022-06-20","2022-07-10",
#'                    "2022-07-20","2022-07-20","2022-07-19"),
#'   departure_date = c("2022-05-10",
#'                      "2022-05-30","2022-08-03","2022-05-17",
#'                      "2022-05-07","2022-05-07","2022-05-07","2022-05-23",
#'                      "2022-05-13","2022-06-04","2022-05-14",
#'                      "2022-05-17","2022-06-05","2022-05-25","2022-05-24",
#'                      "2022-06-09","2022-06-18","2022-06-11",
#'                      "2022-06-22","2022-06-20","2022-07-18","2022-06-09",
#'                      "2022-06-05","2022-06-26","2022-06-11",
#'                      "2022-06-25","2022-06-10","2022-06-12","2022-06-16",
#'                      "2022-07-11","2022-06-20","2022-07-10",
#'                      "2022-07-20","2022-07-20","2022-07-19")
#' )
#'
#' sample_inp <- data.table::data.table(
#'   nhs_number = c("335661151",
#'                  "335661151","335661151","335661151","335661151",
#'                  NA,NA,NA,NA,NA,NA,"645114517","645114517",
#'                  "645114517","143423716","212261130",
#'                  "212261130","212261130","212261130","212261130",
#'                  "349959089","317344169","317344169","317344169",
#'                  "317344169","317344169","317344169","317344169",
#'                  "317344169","317344169","317344169",
#'                  "317344169","317344169","317344169","180554160",
#'                  "180554160",NA,NA,NA,NA,NA,NA,NA,"230782291",
#'                  "977111015","977111015","977111015","977111015",
#'                  "683785606",NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                  NA,NA,"281980720","270646497",NA,
#'                  "387252583","639064842","836297039","836297039",
#'                  "348614531","348614531","662468568","112340924",
#'                  "112340924","381361439","493239044",NA,NA,NA,
#'                  "115641745","115641745","233761482","233761482",
#'                  "479637024","527021626","527021626",
#'                  "294666415","233168855","702650869","460180094",
#'                  "561169746","517229961",NA,"480142132","554675340",
#'                  "135888675",NA,"684718902"),
#'   local_patient_identifier = c(NA,NA,NA,NA,
#'                                NA,"D4809270","D4809270","D4809270",
#'                                "D4809270","D4809270","D4809270",NA,NA,NA,
#'                                "J2098200","D2139084","D2139084","D2139084","D2139084",
#'                                "D2139084",NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA,NA,NA,NA,NA,NA,NA,"A1706089",
#'                                "A1706089","A1706089","A1706089","A1706089",
#'                                "A1706089","A1706089","Z3093435","I1605735","I1888797",
#'                                "I1888797","I1888797","J2901593","U1111563",
#'                                "U1111563","U1111563","U1111563","V2246708",
#'                                "V2246708","V2246708","V2246708","V2246708",
#'                                "V2246708","V2246708","D1101843",NA,"O2700100",
#'                                "I5040881","K1494229","I1222012","I1222012",
#'                                NA,NA,"R4678220","P2632883","P2632883",
#'                                "J6723431","Y1506318","F3501197","F3501197",
#'                                "F3501197","J5206297","J5206297","B2651449",
#'                                "B2651449","F2159102",NA,NA,"W5097806","D6300794",
#'                                NA,"U2715517","O5278248","T1266485",
#'                                "N4842033","Q5566884",NA,"X2768295","H3196212",
#'                                "J9365439"),
#'   date_birth = c("2021-08-14",
#'                          "2021-08-14","2021-08-14","2021-08-14",
#'                          "2021-08-14","1960-05-20","1960-05-20","1960-05-20",
#'                          "1960-05-20","1960-05-20","1960-05-20",
#'                          "2021-03-03","2021-03-03","2021-03-03","2019-11-09",
#'                          "1953-04-05","1953-04-05","1953-04-05",
#'                          "1953-04-05","1953-04-05","1962-06-17","1952-04-03",
#'                          "1952-04-03","1952-04-03","1952-04-03",
#'                          "1952-04-03","1952-04-03","1952-04-03","1952-04-03",
#'                          "1952-04-03","1952-04-03","1952-04-03",
#'                          "1952-04-03","1952-04-03","1985-10-16","1985-10-16",
#'                          "1993-07-09","1993-07-09","1993-07-09",
#'                          "1993-07-09","1993-07-09","1993-07-09","1993-07-09",
#'                          "1980-10-14","1976-08-03","1976-08-03",
#'                          "1976-08-03","1976-08-03","1981-08-27","2017-08-20",
#'                          "2017-08-20","2017-08-20","2017-08-20",
#'                          "2017-08-20","2017-08-20","2017-08-20","2017-08-20",
#'                          "2017-08-20","2017-08-20","2017-08-20",
#'                          "1989-07-11","1964-04-30","1991-12-25","1961-08-16",
#'                          "1983-01-04","1957-01-29","1957-01-29",
#'                          "1982-12-05","1982-12-05","2017-06-01","1989-09-21",
#'                          "1989-09-21","1986-10-06","1995-03-01",
#'                          "1964-04-25","1964-04-25","1964-04-25","1975-09-04",
#'                          "1975-09-04","1995-06-17","1995-06-17",
#'                          "2014-01-05","1995-09-30","1995-09-30","1993-06-09",
#'                          "1976-06-25","2000-06-02","1986-09-14",
#'                          "2016-11-19","2016-10-14","2016-02-04","2004-03-02",
#'                          "1974-06-14","1945-05-14","2001-09-16",
#'                          "1987-08-19"),
#'   organisation_code = c("L4Q","L4Q",
#'                         "L4Q","P3P","P3P","U6X","U6X","U6X","U6X",
#'                         "U6X","U6X","P3P","P3P","P3P","L4Q","O2B",
#'                         "O2B","O2B","O2B","O2B","Z9V","U8V","U8V",
#'                         "U8V","U8V","U8V","U8V","U8V","U8V","U8V",
#'                         "U8V","U8V","U8V","U8V","T7N","T7N","V5T",
#'                         "V5T","V5T","V5T","V5T","V5T","V5T","V7E",
#'                         "J6J","Y9V","Y9V","Y9V","V7E","B1A","B1A",
#'                         "B1A","B1A","J2W","J2W","J2W","J2W","J2W",
#'                         "J2W","J2W","V2P","O1A","O2A","F1O","W6Y",
#'                         "T2Y","T2Y","G2H","G2H","G2H","J6J","J6J",
#'                         "J6J","V5T","G2H","G2H","G2H","V5T","V5T",
#'                         "T4H","T4H","J6J","J6J","J6J","G7H","L4Q",
#'                         "P3P","L4Q","U8V","F0N","O2R","W6Y","O1A",
#'                         "A9V","G9V","L4Q"),
#'   mega_spell_id = c("10.3.0",
#'                     "10.3.1","10.3.2","4.2.0","4.2.1","7.12.0",
#'                     "7.12.1","7.12.2","7.12.3","7.12.4","7.12.5",
#'                     "14.3.0","14.3.1","14.3.2","22.2.1","49.6.0",
#'                     "49.6.1","49.6.2","49.6.3","49.6.4","69.1.0",
#'                     "76.20.0","76.20.1","76.20.2","76.20.3",
#'                     "76.20.4","76.20.5","76.20.6","76.20.7","76.20.8",
#'                     "76.20.9","76.20.10","76.20.11","76.20.12",
#'                     "77.7.0","77.7.1","79.7.0","79.7.1","79.7.2",
#'                     "79.7.3","79.7.4","79.7.5","79.7.6","83.1.0",
#'                     "157.1.0","85.5.0","85.5.1","85.5.2","90.1.0",
#'                     "167.4.0","167.4.1","167.4.2","167.4.3",
#'                     "91.9.0","91.9.1","91.9.2","91.9.3","91.9.4",
#'                     "91.9.5","91.9.6","101.2.0","111.5.0","122.1.0",
#'                     "151.1.0","154.1.0","161.3.0","161.3.1",
#'                     "181.4.0","181.4.1","184.1.0","185.2.0","185.2.1",
#'                     "201.1.0","214.1.0","226.3.0","226.3.1",
#'                     "226.3.2","247.4.0","247.4.1","266.4.0","266.4.1",
#'                     "269.2.0","270.2.0","270.2.1","284.1.0",
#'                     "299.2.0","307.1.0","314.3.0","345.1.0",
#'                     "400.1.0","419.1.0","430.3.1","494.3.0","498.1.0",
#'                     "501.1.0","535.1.0"),
#'   spell_start_date = c("2022-05-20",
#'                        "2022-06-14","2022-06-20","2022-05-01",
#'                        "2022-05-07","2022-05-16","2022-05-29","2022-05-18",
#'                        "2022-05-21","2022-06-27","2022-07-18",
#'                        "2022-05-10","2022-05-29","2022-08-03","2022-05-13",
#'                        "2022-05-27","2022-05-20","2022-06-09",
#'                        "2022-06-27","2022-07-27","2022-06-04","2022-05-21",
#'                        "2022-06-18","2022-06-24","2022-07-16",
#'                        "2022-07-10","2022-07-17","2022-07-20","2022-07-15",
#'                        "2022-07-08","2022-08-01","2022-08-04",
#'                        "2022-07-27","2022-07-14","2022-05-17","2022-06-05",
#'                        "2022-06-01","2022-06-05","2022-06-09",
#'                        "2022-05-23","2022-05-27","2022-06-10","2022-06-12",
#'                        "2022-05-29","2022-05-29","2022-06-02",
#'                        "2022-05-29","2022-06-15","2022-06-09","2022-06-21",
#'                        "2022-05-29","2022-07-18","2022-07-26",
#'                        "2022-05-12","2022-06-12","2022-06-11","2022-06-28",
#'                        "2022-06-29","2022-06-22","2022-07-09",
#'                        "2022-05-19","2022-05-25","2022-05-18","2022-05-26",
#'                        "2022-06-11","2022-06-21","2022-06-13",
#'                        "2022-05-27","2022-06-27","2022-06-22","2022-06-08",
#'                        "2022-07-04","2022-06-26","2022-06-26",
#'                        "2022-06-11","2022-06-17","2022-07-13","2022-06-20",
#'                        "2022-07-18","2022-06-15","2022-07-03",
#'                        "2022-06-05","2022-06-26","2022-06-11","2022-06-24",
#'                        "2022-06-25","2022-06-10","2022-07-01",
#'                        "2022-07-04","2022-06-20","2022-07-10","2022-07-20",
#'                        "2022-07-19","2022-07-28","2022-07-27",
#'                        "2022-07-10"),
#'   spell_end_date = c("2022-06-17",
#'                      "2022-06-16","2022-06-20","2022-05-01",
#'                      "2022-05-11","2022-05-16","2022-05-29","2022-05-18",
#'                      "2022-05-21","2022-07-05","2022-07-18",
#'                      "2022-05-11","2022-06-01","2022-08-04","2022-05-13",
#'                      "2022-05-29","2022-05-21","2022-06-10",
#'                      "2022-06-30","2022-07-28","2022-06-05","2022-06-25",
#'                      "2022-06-18","2022-06-24","2022-07-16",
#'                      "2022-07-10","2022-07-17","2022-07-20","2022-07-15",
#'                      "2022-07-08","2022-08-01","2022-08-04",
#'                      "2022-07-27","2022-07-14","2022-05-30","2022-06-10",
#'                      "2022-06-01","2022-06-05","2022-06-09",
#'                      "2022-05-23","2022-05-27","2022-06-10","2022-06-12",
#'                      "2022-05-30","2022-06-05","2022-06-02",
#'                      "2022-06-01","2022-06-15","2022-06-15","2022-06-21",
#'                      "2022-05-29","2022-07-18","2022-07-26",
#'                      "2022-05-15","2022-06-12","2022-06-17","2022-06-28",
#'                      "2022-06-29","2022-06-23","2022-07-11",
#'                      "2022-05-19","2022-06-09","2022-05-23","2022-05-26",
#'                      "2022-06-20","2022-06-21","2022-06-14",
#'                      "2022-06-02","2022-06-27","2022-06-23","2022-06-08",
#'                      "2022-07-04","2022-06-26","2022-06-28",
#'                      "2022-06-11","2022-06-17","2022-07-13","2022-06-21",
#'                      "2022-07-20","2022-06-18","2022-07-03",
#'                      "2022-06-11","2022-06-26","2022-06-11","2022-06-24",
#'                      "2022-07-01","2022-06-10","2022-07-06",
#'                      "2022-07-06","2022-06-23","2022-07-11","2022-07-22",
#'                      "2022-07-22","2022-07-28","2022-07-27",
#'                      "2022-07-10")
#' )
#'
#' link_ae_inpatient(
#'   ae = list(
#'     data = sample_ae,
#'     arrival_date = 'arrival_date',
#'     departure_date = 'departure_date',
#'     nhs_number = 'nhs_number',
#'     hospital_number = 'local_patient_identifier',
#'     patient_dob = 'patient_birth_date',
#'     org_code = 'organisation_code'
#'   ),
#'   inp = list(
#'     data = sample_inp,
#'     spell_id = 'mega_spell_id',
#'     spell_start_date = 'spell_start_date',
#'     nhs_number = 'nhs_number',
#'     hospital_number = 'local_patient_identifier',
#'     patient_dob = 'date_birth',
#'     org_code = 'organisation_code'
#'   )
#' )[]
#'
#' @export

### FUNCTION START #############################################################

link_ae_inpatient <- function(
    ae = list(
      data,
      arrival_date = 'arrival_date',
      departure_date = 'departure_date',
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      patient_dob = 'patient_birth_date',
      org_code = 'organisation_code_of_provider'
    ),
    inp = list(
      data,
      spell_id = 'mega_spell_id',
      spell_start_date = 'spell_start_date',
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      patient_dob = 'date_birth',
      org_code = 'organisation_code_code_of_provider'
    ),
    .forceCopy = FALSE) {

  if (.forceCopy) {
    inp$data <- data.table::copy(inp$data)
    ae$data <- data.table::copy(ae$data)
  } else {
    data.table::setDT(inp$data)
    data.table::setDT(ae$data)
  }

  ## allow people to match on either A&E admission or discharge date
  ae$data <- data.table::rbindlist(
    list(ae$data[get(ae$arrival_date) != get(ae$departure_date),
                 link_date := .SD,
                 .SDcols = ae$arrival_date],
         ae$data[, link_date := .SD,
                 .SDcols = ae$departure_date]
    ))

  ## dont want to ovewrite the admission date; so create a new one for linking
  inp$data[,
           link_date := .SD,
           .SDcols = inp$spell_start_date]

  data.table::setnames(
    inp$data,
    c(
      inp$nhs_number,
      inp$patient_dob,
      inp$hospital_number,
      inp$org_code
    ),
    c(
      ae$nhs_number,
      ae$patient_dob,
      ae$hospital_number,
      ae$org_code
    ),
    skip_absent = TRUE
  )

  ## valid nhs links
  aeNHS <- ae$data[eval(data.table::substitute2(!is.na(x),
                                                list(x = ae$nhs_number))), ]
  inpNHS <- inp$data[eval(data.table::substitute2(!is.na(x),
                                                  list(x = ae$nhs_number))), ]

  aeNHS[, c('link_id',
            'link_dob',
            'link_org') := .(get(ae$nhs_number),
                             get(ae$patient_dob),
                             get(ae$org_code))]
  inpNHS[, c('link_id',
             'link_dob',
             'link_org') := .(get(ae$nhs_number),
                              get(ae$patient_dob),
                              get(ae$org_code))]

  ## valid hospital number links
  aeHOS <- ae$data[eval(data.table::substitute2(
    is.na(x) & !is.na(y),
    list(x = ae$nhs_number,
         y = ae$hospital_number)
  )),]

  inpHOS <- inp$data[eval(data.table::substitute2(
    is.na(x) & !is.na(y),
    list(x = ae$nhs_number,
         y = ae$hospital_number)
  )),]

  aeHOS[, c('link_id',
            'link_dob',
            'link_org') := .(get(ae$hospital_number),
                             get(ae$patient_dob),
                             get(ae$org_code))]
  inpHOS[, c('link_id',
             'link_dob',
             'link_org') := .(get(ae$hospital_number),
                              get(ae$patient_dob),
                              get(ae$org_code))]


  ## create the two datasets linked by NHS number and Hospital number
  link <- unique(
    data.table::rbindlist(
      list(
        ##  source 1
        data.table::merge.data.table(
          x = aeNHS,
          y = inpNHS,
          by = c('link_id', 'link_dob', 'link_org', 'link_date'),
          all.x = TRUE,
          suffixes = c(".ae", ".inp")
        ),
        ##  source 2
        data.table::merge.data.table(
          x = inpNHS,
          y = aeNHS,
          by = c('link_id', 'link_dob', 'link_org', 'link_date'),
          all.x = TRUE,
          suffixes = c(".inp", ".ae")
        ),
        ##  source 3
        data.table::merge.data.table(
          x = aeHOS,
          y = inpHOS,
          by = c('link_id', 'link_dob', 'link_org', 'link_date'),
          all.x = TRUE,
          suffixes = c(".ae", ".inp")
        ),
        ##  source 4
        data.table::merge.data.table(
          x = inpHOS,
          y = aeHOS,
          by = c('link_id', 'link_dob', 'link_org', 'link_date'),
          all.x = TRUE,
          suffixes = c(".inp", ".ae")
        )),
      idcol = "source",
      use.names = TRUE,
      fill = TRUE
    )
  )

  ## put a meaningful source tag
  link[,source := eval(data.table::substitute2(
    data.table::fcase(
      !is.na(arr) & !is.na(sid), "ECDS:SUS",
      !is.na(arr) & is.na(sid), "ECDS",
      is.na(arr) & !is.na(sid), "SUS",
      default = NA
    ),
    list(arr = ae$arrival_date,
         sid = inp$spell_id)
  ))
  ]

  ## cleanup varnames
  linknames <- list()
  linknames[[ae$nhs_number]] <-  grep(ae$nhs_number,names(link),value=TRUE)
  linknames[[ae$hospital_number]] <-  grep(ae$hospital_number,names(link),value=TRUE)
  linknames[[ae$patient_dob]] <-  grep(ae$patient_dob,names(link),value=TRUE)
  linknames[[ae$org_code]] <-  grep(ae$org_code,names(link),value=TRUE)

  # if an ID column exists
  if("id.ae" %in% names(link)) {
    link[, id := data.table::fifelse(is.na(id.ae),id.inp,id.ae)]
  }

  ## loop through identifiers and consolidate
  for(i in c(ae[[4]],ae[[5]],ae[[6]],ae[[7]])){
    eval(data.table::substitute2(
      link[,(i) := data.table::fifelse(is.na(v1),v2,v1)],
      list(
        v1 = linknames[[i]][1],
        v2 = linknames[[i]][2]
      )))
  }

  ## if the postcode is included
  if(any(grepl("*pcd*|*postcode*",names(link),ignore.case = TRUE))){
    if (any(grepl(".*postcode.*.ae$",
                  names(link),
                  ignore.case = TRUE))) {
      pcdname <- grep(".*postcode.*...$", names(link), value = TRUE)

    } else if (any(grepl(".*pcd.*.ae$",
                     names(link),
                     ignore.case = TRUE))) {
      pcdname <- grep(".*pcd*...$", names(link), value = TRUE)

    }

    pcdvar <- gsub(".ae", "", pcdname[1])
    link[, (pcdvar) := data.table::fifelse(is.na(get(pcdname[1])),
                                           get(pcdname[2]),
                                           get(pcdname[2]))]
  }

  # cpature and delete extra cols
  rmcols <- c(
    grep("link_",names(link),value = TRUE),
    grep(".ae",names(link),value = TRUE),
    grep(".inp",names(link),value = TRUE))

  link[, (rmcols) := NULL]

  ## put ID cols at the beginning
  ##TODO add fix for missing pcdvar
  if('id' %in% names(link) & exists("pcdvar")) {
    data.table::setcolorder(link, c('id', ae[[4]], ae[[5]], ae[[6]], ae[[7]], pcdvar))
  } else if ('id' %in% names(link)) {
    data.table::setcolorder(link, c('id', ae[[4]], ae[[5]], ae[[6]], ae[[7]]))
  } else if (exists("pcdvar")) {
    data.table::setcolorder(link, c(ae[[4]], ae[[5]], ae[[6]], ae[[7]], pcdvar))
  } else {
    data.table::setcolorder(link, c(ae[[4]], ae[[5]], ae[[6]], ae[[7]]))
  }


  return(link)
}
