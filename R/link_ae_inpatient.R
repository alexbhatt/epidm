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
#' @param ae a list to provide data and columns for the A&E (ECDS) data; all arguments provided quoted unless specified
#' @param ae$data the ECDS A&E dataset provided unquoted
#' @param ae$arrival_date the ECDS arrival date
#' @param ae$departure_date the ECDS discharge date
#' @param ae$nhs_number the patient NHS number
#' @param ae$hospital_number the patient Hospital numbers also known as the local patient identifier
#' @param ae$patient_dob patient date of birth
#' @param ae$org_code the NHS trust organisation codes
#' @param inp a list to provide data and columns for the inpatient (SUS/HES) data
#' @param inp$data the HES/SUS inpatient dataset provided unquoted
#' @param inp$spell_start_date a string containing the inpatient (SUS/HES) admission date column name; all arguments provided quoted unless specified
#' @param inp$spell_id the HES/SUS spell id
#' @param inp$nhs_number the patient NHS number
#' @param inp$hospital_number the patient Hospital numbers also known as the local patient identifier
#' @param inp$patient_dob patient date of birth
#' @param inp$org_code the NHS trust organisation codes
#' @param .forceCopy a boolean to control if you want to copy the dataset before
#'   linking together
#'
#' @seealso group_time continuous_inpatient_spells
#'
#' @return a patient level linked hospital record
#'
#' @examples
#' sample_ae <- data.table::data.table(
#'   nhs_number = c("589042614",
#'                  "396453156","390310824","288486330","117082902",
#'                  "117082902","100344084","347467045",
#'                  "510067733","370659458","319971144","319971144",
#'                  "218302313","615161419","476921945","268950533",
#'                  "396036365","371087423","238514929",NA,
#'                  "210387062","130254747","130254747","476054959",
#'                  "334796244","328944194"),
#'   local_patient_identifier = c("H7887665",
#'                                "P9002864","U6388826","V5868678","A2580913",
#'                                "A2580913","V3900488",NA,"N1748692","A1641408",
#'                                "A5595054","A5595054","D1646807","B4607750",NA,
#'                                "S3483623","Z3633402","R8184533",NA,
#'                                "Q2518201",NA,"B7136736","B7136736","N1184803",NA,
#'                                "G5019233"),
#'   patient_birth_date = as.Date(c("1984-05-14",
#'                          "1955-03-30","1973-09-01","1962-10-14",
#'                          "1986-11-21","1986-11-21","1987-08-10","1955-05-21",
#'                          "1955-09-18","1971-12-10","1976-09-11",
#'                          "1976-09-11","1997-08-28","1980-08-26","1996-07-11",
#'                          "1999-11-04","1931-10-22","2016-04-18",
#'                          "2013-11-23","1992-10-30","1978-09-26","1997-12-01",
#'                          "1997-12-01","1991-01-20","1996-07-10",
#'                          "2012-03-02")),
#'   arrival_date = as.Date(c("2022-05-15",
#'                    "2022-05-26","2022-05-25","2022-05-17",
#'                    "2022-06-05","2022-06-17","2022-06-10","2022-06-10",
#'                    "2022-06-23","2022-06-23","2022-06-05",
#'                    "2022-07-18","2022-06-25","2022-06-26","2022-06-18",
#'                    "2022-06-15","2022-07-12","2022-06-23",
#'                    "2022-07-03","2022-07-15","2022-07-15","2022-07-04",
#'                    "2022-07-19","2022-07-17","2022-07-30",
#'                    "2022-07-26")),
#'   departure_date = as.Date(c("2022-05-15",
#'                      "2022-05-26","2022-05-26","2022-05-17",
#'                      "2022-06-05","2022-06-17","2022-06-10","2022-06-10",
#'                      "2022-06-23","2022-06-23","2022-06-05",
#'                      "2022-07-18","2022-06-26","2022-06-26","2022-06-18",
#'                      "2022-06-15","2022-07-12","2022-06-23",
#'                      "2022-07-03","2022-07-15","2022-07-15","2022-07-04",
#'                      "2022-07-19","2022-07-17","2022-07-30",
#'                      "2022-07-26"))
#' )
#' sample_inp <- data.table::data.table(
#'   nhs_number = c("467450019",
#'                  "467450019","467450019","467450019","467450019",
#'                  "589042614","396453156",NA,"554421641",
#'                  "399268089","399268089","940857685","940857685",
#'                  "940857685","940857685","940857685",NA,NA,NA,
#'                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                  "127375964","127375964","127375964","127375964",
#'                  "127375964","127375964","127375964","231351845",
#'                  "231351845","231351845","231351845",NA,NA,
#'                  NA,NA,NA,NA,NA,NA,NA,NA,NA,"722458112",
#'                  "407916984","146662988","146662988","146662988",
#'                  "146662988","146662988","525228487",
#'                  "525228487","159788511",NA,NA,"370659458",
#'                  "297663092","137232458","615161419","396519853",
#'                  "391918879","196596027",NA,"176851814","176851814",
#'                  "176851814","238514929",NA,"321702417",NA,
#'                  "344122276","328944194","395851308","395851308",
#'                  "395851308","395851308"),
#'   local_patient_identifier = c(NA,NA,NA,NA,
#'                                NA,"H7887665","P9002864","A4010187",NA,NA,
#'                                NA,NA,NA,NA,NA,NA,"R1998488","X2106396",
#'                                "X2106396","X2106396","X2106396","X2106396",
#'                                "X2106396","X2106396","X2106396","X2106396",
#'                                "X2106396","X2106396","X2106396","X2106396",NA,
#'                                NA,NA,NA,NA,NA,NA,"M1121088","O1857557",
#'                                "O1857557","O1857557","Y3768580","Y3768580",
#'                                "Y3768580","Y3768580","A1907919","A1907919",
#'                                "A1907919","A1907919","A1907919","A1907919",
#'                                "A1907919","C3392765",NA,NA,NA,NA,NA,NA,NA,
#'                                NA,"B2569823","S6536866","S6536866","A1641408",
#'                                NA,"F5463560","B4607750","O5578718",
#'                                "Q2921208","Y3136978","J1565800",NA,NA,NA,NA,
#'                                "Q2518201",NA,"H9659463","P2787072","G5019233",
#'                                "W1761440","W1761440","W1761440","W1761440"),
#'   patient_birth_date = as.Date(c("2022-02-27",
#'                          "2022-02-27","2022-02-27","2022-02-27",
#'                          "2022-02-27","1984-05-14","1955-03-30","2003-09-23",
#'                          "1991-10-24","1984-09-01","1984-09-01",
#'                          "1953-02-17","1953-02-17","1953-02-17","1953-02-17",
#'                          "1953-02-17","1962-10-21","1951-09-29",
#'                          "1951-09-29","1951-09-29","1951-09-29","1951-09-29",
#'                          "1951-09-29","1951-09-29","1951-09-29",
#'                          "1951-09-29","1951-09-29","1951-09-29","1951-09-29",
#'                          "1951-09-29","1994-04-20","1994-04-20",
#'                          "1994-04-20","1994-04-20","1994-04-20","1994-04-20",
#'                          "1994-04-20","1975-01-22","1975-01-22",
#'                          "1975-01-22","1975-01-22","2016-05-30","2016-05-30",
#'                          "2016-05-30","2016-05-30","2016-05-30",
#'                          "2016-05-30","2016-05-30","2016-05-30","2016-05-30",
#'                          "2016-05-30","2016-05-30","2003-03-20",
#'                          "1979-04-03","1988-01-25","1988-01-25","1988-01-25",
#'                          "1988-01-25","1988-01-25","1989-07-04",
#'                          "1989-07-04","1987-12-11","1988-01-02","1988-01-02",
#'                          "1971-12-10","1990-05-13","1989-11-03",
#'                          "1980-08-26","1984-06-19","1991-05-20","1982-09-25",
#'                          "1982-05-19","1997-10-10","1997-10-10",
#'                          "1997-10-10","2013-11-23","1992-10-30","1994-09-08",
#'                          "1946-01-30","1992-06-29","2012-03-02",
#'                          "1990-05-10","1990-05-10","1990-05-10","1990-05-10")),
#'   mega_spell_id = c("10.3.0",
#'                     "10.3.1","10.3.2","4.2.0","4.2.1","13.1.0",
#'                     "16.3.0","20.3.1","28.2.0","30.2.0","30.2.1",
#'                     "49.6.0","49.6.1","49.6.2","49.6.3","49.6.4",
#'                     "56.1.0","76.20.0","76.20.1","76.20.2","76.20.3",
#'                     "76.20.4","76.20.5","76.20.6","76.20.7",
#'                     "76.20.8","76.20.9","76.20.10","76.20.11",
#'                     "76.20.12","79.7.0","79.7.1","79.7.2","79.7.3",
#'                     "79.7.4","79.7.5","79.7.6","157.1.0","85.5.0",
#'                     "85.5.1","85.5.2","167.4.0","167.4.1","167.4.2",
#'                     "167.4.3","91.9.0","91.9.1","91.9.2",
#'                     "91.9.3","91.9.4","91.9.5","91.9.6","113.1.0",
#'                     "128.1.0","130.5.0","130.5.1","130.5.2","130.5.3",
#'                     "130.5.4","192.6.0","192.6.1","196.2.0",
#'                     "213.2.0","213.2.1","233.4.0","253.1.0",
#'                     "278.1.0","291.2.0","298.1.0","335.1.0","349.1.0",
#'                     "354.2.0","411.3.0","411.3.1","411.3.2",
#'                     "438.2.0","439.2.0","449.2.0","498.1.0","537.2.0",
#'                     "562.1.0","584.5.0","584.5.1","584.5.2",
#'                     "584.5.3"),
#'   spell_start_date = as.Date(c("2022-05-11",
#'                        "2022-06-17","2022-06-22","2022-04-22",
#'                        "2022-04-22","2022-05-15","2022-05-26","2022-05-06",
#'                        "2022-05-16","2022-05-31","2022-06-14",
#'                        "2022-05-30","2022-05-16","2022-06-09","2022-07-07",
#'                        "2022-07-14","2022-05-27","2022-05-11",
#'                        "2022-06-21","2022-07-08","2022-07-05","2022-07-06",
#'                        "2022-07-18","2022-07-26","2022-07-03",
#'                        "2022-07-08","2022-08-01","2022-07-31","2022-07-27",
#'                        "2022-07-31","2022-06-02","2022-06-08",
#'                        "2022-05-18","2022-05-29","2022-05-30","2022-06-01",
#'                        "2022-05-18","2022-06-07","2022-06-08",
#'                        "2022-05-25","2022-06-21","2022-05-29","2022-06-08",
#'                        "2022-07-17","2022-07-22","2022-05-26",
#'                        "2022-06-01","2022-06-09","2022-06-30","2022-07-07",
#'                        "2022-06-29","2022-07-15","2022-05-18",
#'                        "2022-05-20","2022-05-22","2022-06-01","2022-05-27",
#'                        "2022-05-29","2022-06-07","2022-06-11",
#'                        "2022-06-25","2022-06-23","2022-06-03","2022-07-06",
#'                        "2022-06-23","2022-06-22","2022-06-06",
#'                        "2022-06-26","2022-06-21","2022-07-09","2022-06-19",
#'                        "2022-07-12","2022-06-24","2022-07-05",
#'                        "2022-07-12","2022-07-03","2022-07-15","2022-07-03",
#'                        "2022-07-22","2022-07-13","2022-07-26",
#'                        "2022-07-19","2022-08-02","2022-07-21","2022-08-09")),
#'   spell_end_date = as.Date(c("2022-06-08",
#'                      "2022-06-19","2022-06-22","2022-04-22",
#'                      "2022-04-26","2022-05-15","2022-06-24","2022-05-10",
#'                      "2022-05-17","2022-05-31","2022-06-14",
#'                      "2022-06-01","2022-05-17","2022-06-10","2022-07-10",
#'                      "2022-07-15","2022-05-27","2022-06-15",
#'                      "2022-06-21","2022-07-08","2022-07-05","2022-07-06",
#'                      "2022-07-18","2022-07-26","2022-07-03",
#'                      "2022-07-08","2022-08-01","2022-07-31","2022-07-27",
#'                      "2022-07-31","2022-06-02","2022-06-08",
#'                      "2022-05-18","2022-05-29","2022-05-30","2022-06-01",
#'                      "2022-05-18","2022-06-14","2022-06-08",
#'                      "2022-05-28","2022-06-21","2022-05-29","2022-06-08",
#'                      "2022-07-17","2022-07-22","2022-05-29",
#'                      "2022-06-01","2022-06-15","2022-06-30","2022-07-07",
#'                      "2022-06-30","2022-07-17","2022-05-20",
#'                      "2022-05-20","2022-05-22","2022-06-01","2022-05-27",
#'                      "2022-05-29","2022-06-20","2022-06-11",
#'                      "2022-07-03","2022-06-23","2022-06-03","2022-07-06",
#'                      "2022-06-24","2022-06-26","2022-06-13",
#'                      "2022-06-30","2022-06-27","2022-07-09","2022-06-19",
#'                      "2022-07-14","2022-06-24","2022-07-05",
#'                      "2022-07-12","2022-07-03","2022-07-16","2022-07-03",
#'                      "2022-07-22","2022-07-13","2022-07-27",
#'                      "2022-07-19","2022-08-02","2022-07-21","2022-08-09"))
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
#'     org_code = 'organisation_code_of_provider'
#'   ),
#'   inp = list(
#'     data = sample_inp,
#'     spell_id = 'mega_spell_id',
#'     spell_start_date = 'spell_start_date',
#'     nhs_number = 'nhs_number',
#'     hospital_number = 'local_patient_identifier',
#'     patient_dob = 'date_birth',
#'     org_code = 'organisation_code_code_of_provider'
#'   )
#' )
#'
#' @export

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

  setnames(
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

  link[,source := eval(substitute2(
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
    link[, id := fifelse(is.na(id.ae),id.inp,id.ae)]
  }

  ## loop through identifiers and consolidate
  for(i in c(ae[[4]],ae[[5]],ae[[6]],ae[[7]])){
    eval(data.table::substitute2(
      link[,(i) := fifelse(is.na(v1),v2,v1)],
      list(
        v1 = linknames[[i]][1],
        v2 = linknames[[i]][2]
      )))
  }

  ## if the postcode is included
  if(grep(".*postcode.*.ae$",
          names(link),
          value = TRUE,
          ignore.case = TRUE) %in% names(link)) {

    pcdname <- grep(".*postcode.*...$",names(link),value=TRUE)
    pcdvar <- gsub(".ae","",pcdname[1])

    link[,(pcdvar):=fifelse(is.na(get(pcdname[1])),
                            get(pcdname[2]),
                            get(pcdname[2]))
    ]
  }

  # cpature and delete extra cols
  rmcols <- c(
    grep("link_",names(link),value = TRUE),
    grep(".ae",names(link),value = TRUE),
    grep(".inp",names(link),value = TRUE))

  link[, (rmcols) := NULL]

  ## put ID cols at the beginning
  if(all(c('id',pcdvar) %in% names(link))) {
    setcolorder(link, c('id', ae[[4]], ae[[5]], ae[[6]], ae[[7]], pcdvar))
  } else if ('id' %in% names(link)) {
    setcolorder(link, c('id', ae[[4]], ae[[5]], ae[[6]], ae[[7]]))
  } else if (pcdvar %in% names(link)) {
    setcolorder(link, c(ae[[4]], ae[[5]], ae[[6]], ae[[7]], pcdvar))
  } else {
    setcolorder(link, c(ae[[4]], ae[[5]], ae[[6]], ae[[7]]))
  }


  return(link)
}
