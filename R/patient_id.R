#' @title Patient ID record grouping
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'
#' Groups patient records from multiple isolates with a single integer patientID
#' by grouping patient identifiers.
#'
#' Grouping is based on five stages:
#' \enumerate{
#' \item matching nhs number and date of birth
#' \item Hospital number &  Date of Birth
#' \item NHS number & Hospital Number
#' \item Date of Birth & Surname IF nhs unknown
#' \item Sex & Date of Birth & Fuzzy Name
#' }
#'
#' Identifiers are copied over where they are missing or invalid to the grouped
#' records.
#'
#' @import data.table
#' @importFrom phonics soundex
#' @importFrom stringr word
#' @importFrom stringi stri_trans_general stri_trans_toupper
#'
#' @param x a data.frame or data.table containing the cleaned line list
#' @param nhs_number a column as a character containing the patient NHS numbers
#' @param hospital_number a column as a character containing the patient
#'   Hospital numbers
#' @param date_of_birth a column as a date variable containing the patient
#'   date of birth in date format
#' @param sex_mfu column as a character containing the patient sex;
#'   NOTE only works if coded only as character versions of Male/Female/Unknown;
#'   does not currently work with additional options `#future update`
#' @param forename a column as a character containing the patient forename;
#'   leave as NONAME if unavailable
#' @param surname a column as a character containing the patient surname;
#'   leave as NONAME if unavailable
#' @param .keepValidNHS optional, default FALSE; set TRUE if you wish to retain
#'   the column with the NHS checksum result stored as a BOOLEAN
#' @param .sortOrder optional; a column as a character to allow a sorting
#'   order on the id generation
#' @param .forceCopy optional, default FALSE; TRUE will force data.table to take a copy
#'   instead of editing the data without reference
#' @param .experimental optional, default FALSE; TRUE will enable the
#'   experimental features for recoding NA values based on the mode
#'
#' @return A dataframe with one new variable:
#' \describe{
#'   \item{`id`}{a unique patient id}
#'   \item{`valid_nhs`}{if retained using argument `.keepValidNHS=TRUE`, a
#'     BOOLEAN containing the result of the NHS checksum validation}
#' }
#'
#' @examples
#' id_test <- data.frame(
#'   nhs_n = c(
#'     9434765919,9434765919,9434765919,NA,NA,
#'     3367170666,5185293519,5185293519,5185293519,8082318562,NA,NA,NA
#'   ),
#'   hosp_n = c(
#'     '13','13','13','UNKNOWN','13','13','13','31','31','96','96',NA,'96'),
#'   sex = c(rep('F',6),rep('Male',4), 'U', 'U', 'M'),
#'   dateofbirth = as.Date(
#'     c(
#'       '1988-10-06','1988-10-06','1900-01-01','1988-10-06','1988-10-06',
#'       '1988-10-06','1988-10-06','1988-10-06','1988-10-06','2020-01-28',
#'       '2020-01-28','2020-01-28','2020-01-28'
#'     )
#'   ),
#'   firstname = c(
#'     'Danger','Danger','Denger','Danger','Danger','DANGER','Danger',
#'     'Danger','Danger','Crazy','Crazy','Krazy','C'
#'   ),
#'   lastname = c(
#'     'Mouse','Mause','Mouse','Moose','Moose','Mouse','MOUSe',
#'     'Mouse','Mouse','Frog','FROG','Frug','Frog'
#'   ),
#'   testdate = sample(seq.Date(Sys.Date()-21,Sys.Date(),"day"),13,replace = TRUE)
#' )
#' id_test <- data.frame(
#'   stringsAsFactors = FALSE,
#'   nhs_number = c("9434765919",
#'                  "9434765919","9434765919","382940103",
#'                  "382940103","3367170666","3367170666","3367170666",
#'                  "3367170666","9176325008",NA,NA,"9068667262",
#'                  "2520311053","5703411017","3275446444",NA,NA,
#'                  "0917395816","473372796",NA,"473372796",NA,
#'                  "1789154138","3599923868","0158691032",
#'                  "0158691032","8354073580",NA,NA,"2726167470",NA,NA,
#'                  NA,"9329053475","2063142777","2063142777",
#'                  "5119650317","5062866472",NA,"537235036",NA,
#'                  "0967448493",NA,NA,NA,"5555555555",NA),
#'   local_patient_identifier = c(NA,NA,NA,
#'                                "I3348707","I3348707",NA,NA,NA,NA,"P1350948",
#'                                "P1350948","P1350948",NA,NA,"Q4157514",NA,
#'                                NA,NA,"D1101843",NA,"K2440769","K2440769",NA,
#'                                "E1366499",NA,"K1494229","K1494229",
#'                                "NO PATIENT ID","J5206297","J5206297","S1945338",NA,
#'                                NA,NA,"F2159102",NA,NA,"UNKNOWN",NA,
#'                                "W1208900","Z4975449","G7439612","T1266485",
#'                                "N4842033","Q5566884","Q5566884","P2689566",NA),
#'   patient_birth_date = c("2021-03-03",
#'                          NA,"2021-03-03","2003-08-24","2003-08-24",
#'                          "2001-06-21",NA,"2001-06-21","2001-06-21",
#'                          "1991-10-08","1991-10-08","1991-10-08","1987-02-03",
#'                          NA,"1991-10-07","1985-10-16","1985-10-16",
#'                          "1985-10-16","1990-09-24","1984-11-14",
#'                          "1984-11-14","1984-11-14","1984-11-14","1994-05-05",
#'                          "1999-08-11","1983-01-04","1983-01-04",
#'                          "2007-06-01","1975-09-04","1975-09-04","1993-07-13",
#'                          "2014-01-05","2014-01-05","2014-01-05",
#'                          "2014-01-05",NA,NA,"1976-06-25","1900-01-01",
#'                          "2017-06-11","2007-05-03","1986-08-28",NA,"2016-02-04",
#'                          "2004-03-02","2004-03-02","1979-01-17",
#'                          "1974-06-14"),
#'   event_date = c("2022-05-10",
#'                  "2022-05-29","2022-08-03","2022-05-17",
#'                  "2022-05-17","2022-05-07","2022-05-07","2022-05-07",
#'                  "2022-05-07","2022-05-23","2022-05-23",
#'                  "2022-05-23","2022-05-13","2022-06-04","2022-05-14",
#'                  "2022-05-17","2022-06-05","2022-06-05",
#'                  "2022-05-25","2022-05-24","2022-05-24","2022-05-24",
#'                  "2022-05-24","2022-06-09","2022-06-18",
#'                  "2022-06-11","2022-06-11","2022-06-22","2022-06-20",
#'                  "2022-07-18","2022-06-09","2022-06-05",
#'                  "2022-06-05","2022-06-05","2022-06-05","2022-06-26",
#'                  "2022-06-11","2022-06-25","2022-06-10",
#'                  "2022-06-12","2022-06-16","2022-07-10","2022-06-20",
#'                  "2022-07-10","2022-07-20","2022-07-20",
#'                  "2022-07-20","2022-07-19"),
#'   sex = c("Male","Male",
#'           "Male","Female",NA,NA,"Female","Female",
#'           "Female","Female","Female","Female","Male",
#'           "Female","Female","Male","Male","Male","Male",
#'           "Female","Female","Female","Female","Male",
#'           "Male","Male","Male","Female","Male","Male",
#'           "Male","Male","Male","Male","Male","Female",
#'           "Female","Male","Female","Female","Female",
#'           "Female","Female","Female","Male","Male",
#'           "Male","Male"),
#'   forename = c("NICHOLAS",
#'                "NICHOLAS","NICHOLAS","SARAH",NA,"FUAADA",
#'                "FUAADA","FUAADA","EL-SHAHIDI","CHANTEL","CHANTEL",
#'                "CHANTEL","JOSHUA",NA,"KENDRA","ALEXANDER",
#'                "ALEXANDER","ALEX","KONDA","ODESSA","ODESSA",
#'                "ODESSA","ODESSA",NA,"NICHOLAS","KEVIN",
#'                "KEVIN",NA,"NAJEEB","NAJEEB","ANTHONY",
#'                "WILLIAM","WILL","WILLY","WILLIAM","EMMA","EMMA",
#'                "RANDY","BRIANNA","ALMAASA","SARAH","ALMAASA",
#'                "CECILIA","ALMAASA",NA,NA,"JEFFERY",
#'                "DEMETRIUS"),
#'   surname = c("MCCREARY",
#'               "MCCREARY","MCCREARY","VATHANAVARIN",
#'               "VATHANAVARIN","EL-SHAHIDI","EL-SHAHIDI","EL-SHAHIDI",
#'               "FUAADA","LENHART","LENHART","LENHART","MASON",
#'               NA,"VIGIL","CARTER","CARTER","CARTER",
#'               "AL-SADER","RINHART","RINHART","RINHART","RINHART",
#'               NA,"HEU","EL-ASMAR","EL-ASMAR",NA,
#'               "CAMPBELL","CAMPBELL","WILLIAMS","YAMAMOTO","YAMAMOTO",
#'               "YAMAMOTO","YAMAMOTO","BRAVE","BRAVE",
#'               "CHIN","GUERETTE","BROWN","LIGHTFOOT","BROWN",
#'               "HUFF","BROWN",NA,NA,"LUCERO","HUYNH")
#' )
#'
#' uk_patient_id(x = id_test,
#'               nhs_number = 'nhs_n',
#'               hospital_number = 'hosp_n',
#'               forename = 'firstname',
#'               surname = 'lastname',
#'               sex_mfu = 'sex',
#'               date_of_birth = 'dateofbirth',
#'               .sortOrder = 'testdate')[]
#'
#' @export


uk_patient_id <- function(x,
                          nhs_number,
                          hospital_number,
                          date_of_birth,
                          sex_mfu,
                          forename = "NONAME",
                          surname = "NONAME",
                          .sortOrder,
                          .keepValidNHS = FALSE,
                          .forceCopy = FALSE,
                          .experimental = FALSE) {

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  ## allow a forced sort order; but not necessary
  if(!missing(.sortOrder)){
    data.table::setorderv(x,c(.sortOrder))
  }

  ## setup variables for entry into data.table
  ## Needed to prevent RCMD Check fails
  ## recommended by data.table
  ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  # id <-
  #   tmp.valid.nhs <- tmp.valid.hos <- tmp.valid.dob <- tmp.valid.sex <-
  #   tmp.valid.n1 <- tmp.valid.n2 <- tmp.valid.ym <-
  #   NULL

  # apply other validity features
  # use SDcols version to ensure that the column name and argument name work if the same
  x[,id := seq_len(.N)]
  x[,tmp.idN := 1L]

  ## set id to column 1
  data.table::setcolorder(x,c('id','tmp.idN'))

  x[,tmp.valid.nhs := lapply(.SD,
                             function(x) epidm::valid_nhs(x) == 1),
    .SDcols = nhs_number]
  x[,tmp.valid.dob := lapply(.SD,
                             function(x) !x %in% as.Date(c("1900-01-01",
                                                           "1800-01-01",
                                                           NA))),
    .SDcols = date_of_birth
    ]
  x[,tmp.valid.hos := lapply(.SD,
                             function(x) !x %in% c("UNKNOWN",
                                                   "NO PATIENT ID",
                                                   NA)),
    .SDcols = hospital_number]
  x[,
    tmp.valid.sex := lapply(.SD,
                            function(x) grepl("^(M|F)",x,ignore.case=TRUE)),
    .SDcols = sex_mfu
    ]

  x[,
    c(sex_mfu) := .(
      data.table::fifelse(tmp.valid.sex,
                          toupper(substr(as.character(get(sex_mfu)),1,1)),
                          NA_character_)
      )
  ]

  ## cleanup as some codes have massive leading or lagging whitespace
  x[,
    c(hospital_number) := .(trimws(get(hospital_number)))
    ]


  ## S1: NHS + DOB ###########################################################
  x[,
    id := data.table::fifelse(
      tmp.valid.nhs & tmp.valid.dob,
      id[1],
      id
    ),
    by = c(
      nhs_number,
      date_of_birth
      )
  ][
    ,tmp.idN := .GRP,
    by = 'id'
  ]


  ## S2: HOS + DOB ###########################################################
  x[,
    id := data.table::fifelse(
      tmp.valid.hos & tmp.valid.dob,
      id[1],
      id),
    by = c(
      hospital_number,
      date_of_birth
      )
  ][
    ,tmp.idN := .GRP,
    by = 'id'
  ]

  ## S3: NHS + HOS ###########################################################
  x[,
    id := data.table::fifelse(
      tmp.valid.nhs & tmp.valid.hos,
      id[1],
      id),
    by = c(
      nhs_number,
      hospital_number
      )
  ][
    ,tmp.idN := .GRP,
    by = 'id'
  ]

  if(surname!="NONAME"){
    ## S4: SEX + DOB + NAME ####################################################
    namecols <- grep("name",names(x),ignore.case=TRUE,value=TRUE)
    x[,
      (namecols) := lapply(.SD,
             function(X) stringi::stri_trans_general(
               stringi::stri_trans_toupper(X),
               "Latin-ASCII")
      ),
      .SDcols = namecols
    ]
    x[,
      tmp.valid.n2 := !get(surname) %in% c("","NA",NA)
    ]
    x[,
      id := data.table::fifelse(
        tmp.valid.dob & tmp.valid.n2 & !tmp.valid.nhs,
        id[1],
        id),
      by = c(
        date_of_birth,
        surname,
        forename
        )
    ][
      ,tmp.idN := .GRP,
      by = 'id'
    ]


    ## S5: SEX + DOB + FUZZY NAME ##############################################
    x[,tmp.fuzz.n1 := base::substr(get(forename),1,1)]
    x[,tmp.fuzz.n2 := phonics::soundex(stringr::word(get(surname),1))]
    x[,tmp.fuzz.ym := substr(get(date_of_birth),1,7)]

    x[,
      id := data.table::fifelse(
        tmp.valid.sex & tmp.valid.dob & tmp.valid.n2 & !tmp.valid.nhs,
        id[1],
        id),
      by = c(
        sex_mfu,
        'tmp.fuzz.ym',
        'tmp.fuzz.n1',
        'tmp.fuzz.n2'
      )
    ][
      ,tmp.idN := .GRP,
      by = 'id'
    ]

  }


  if(.experimental){
  ## capture legit IDs where NA or invalid

  ## set types for the columns; and clear out the invalid results
  ## we will bring the results together later

  x[,
    c(nhs_number) := .(
      data.table::fifelse(tmp.valid.hos,
                          as.numeric(get(nhs_number)),
                          NA_integer_)
    )
    ]

  x[,
    c(hospital_number) := .(
      data.table::fifelse(tmp.valid.hos,
                          as.character(get(hospital_number)),
                          NA_character_)
    )
  ]

  x[,
    c(date_of_birth) := .(
      data.table::fifelse(tmp.valid.dob,
                          as.character(get(date_of_birth)),
                          NA_character_)
    )
  ]


  copyid <- c(nhs_number,hospital_number,date_of_birth,sex_mfu)

  ## capture the most common value aka. mode, but for numeric or characters
  na_replace_mode <- function(x){

    ## get the mode
    len <- length(x)
    uni <- unique(na.omit(x))
    mode <- uni[which.max(table(match(x, uni)))]
    vec <- rep(mode,len)

    ## where the group only has 1 entry, and its NA
    if(length(vec)==0){
      return(NA)
    } else {
      res <- data.table::fcoalesce(x,vec)
      return(res)
    }
  }

  x[,
    (copyid) := lapply(.SD,na_replace_mode),
    by = 'id',
    .SDcols = copyid
  ]


  x[,
    c(sex_mfu) := .(
      data.table::fifelse(is.na(get(sex_mfu)),
                          "U",
                          get(sex_mfu))
      )
  ]
  }

  ## order the final results
  if(!missing(.sortOrder)){
    data.table::setorderv(x,c('id',.sortOrder))
  } else {
    data.table::setorder(x,'id')
  }

  if(.keepValidNHS){
    data.table::setnames(x,'tmp.valid.nhs','valid_nhs')
    }

  ## cleanup and remove temporary vars
  tmpcols <- grep("^tmp.",names(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)

}

