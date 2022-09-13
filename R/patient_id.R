#' @title Patient ID record grouping
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'
#' Groups patient records from multiple isolates with a single integer patientID
#' by grouping patient identifiers.
#'
#' Grouping is based on the following stages:
#' \enumerate{
#' \item matching nhs number and date of birth
#' \item Hospital number &  Date of Birth
#' \item NHS number & Hospital Number
#' \item NHS number & Name
#' \item Hospital number & Name
#' \item Sex & Date of Birth & Surname
#' \item Sex & Date of Birth & Fuzzy Name
#' \item Sex & Year and Month of Birth & Fuzzy Name
#' \item Postcode & Name
#' \item Name Swaps (when first and last name are the wrong way around)
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
#' @param data a data.frame or data.table containing the patient data
#' @param id a named list to provide the column names with identifiers, quoted
#'  \describe{
#'    \item{`nhs_number`}{the patient NHS number}
#'    \item{`hospital_number`}{the patient Hospital numbers also known as the local patient identifier}
#'    \item{`date_of_birth`}{the patient date of birth}
#'    \item{`sex_mfu`}{the patient sex or gender field as Male/Female/Unknown}
#'    \item{`forename`}{the patient forename}
#'    \item{`surname`}{the patient surname}
#'    \item{`postcode`}{the patient postcode}
#'   }
#' @param .sortOrder optional; a column as a character to allow a sorting
#'   order on the id generation
#' @param .keepValidNHS optional, default FALSE; set TRUE if you wish to retain
#'   the column with the NHS checksum result stored as a BOOLEAN
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
#' nhs_number = c(9434765919,
#'                9434765919,9434765919,382940103,NA,382940103,
#'                3367170666,3367170666,3367170666,3367170666,
#'                9176325008,NA,NA,5703411017,5703411017,
#'                3275446444,NA,NA,473372796,NA,473372796,NA,
#'                1789154138,158691032,158691032,8354073580,NA,NA,NA,
#'                NA,NA,9329053475,2063142777,2063142777,NA,
#'                NA,NA,NA,NA,5555555555,5555555555),
#' local_patient_identifier = c(NA,NA,NA,
#'                              "I3348707",NA,"I3348707",NA,NA,NA,NA,
#'                              "P1350948","P1350948","P1350948",NA,"Q4157514",
#'                              "UNKNOWN",NA,"UNKNOWN",NA,"K2440769","K2440769",
#'                              NA,"E1366499","K1494229","K1494229",
#'                              "NO PATIENT ID","J5206297","J5206297",NA,NA,NA,
#'                              "F2159102",NA,NA,"W1208900","G7439612","N4842033",
#'                              "Q5566884","Q5566884","P2689566","P2689566"),
#' patient_birth_date = as.Date(c("2021-03-03",
#'                                NA,"2021-03-03","2003-08-24","2003-08-24",
#'                                "2003-08-24","2001-06-21",NA,"2001-06-21",
#'                                "2001-06-21","1991-10-08","1991-10-08","1991-10-08",
#'                                NA,"1991-10-07","1985-10-16","1985-10-16",
#'                                "1985-10-16","1984-11-14","1984-11-14",
#'                                "1984-11-14","1984-11-14","1994-05-05","1983-01-04",
#'                                "1983-01-04","2007-06-01","1975-09-04",
#'                                "1975-09-04","2014-01-05","2014-01-05","2014-01-05",
#'                                "2014-01-05",NA,NA,"2017-06-11","1986-08-28",
#'                                "1986-08-26","2004-03-02","2004-03-02",
#'                                "1979-01-17","1979-01-17")),
#' sex = c("Male","Male",
#'         "Male","Female",NA,NA,NA,"Female","Female",
#'         "Female","Female","Female","Female",
#'         "Female","Female","Male","Male","Male","Female",
#'         "Female","Female","Female","Male","Male",
#'         "Male","Female","Male","Male","Male","Male",
#'         "Male","Male","Female","Female","Female",
#'         "Female","Female","Male","Male","Male","Male"),
#' forename = c("NICHOLAS",
#'              "NICHOLAS","NICHOLAS","SARAH","VATHANAVARIN",NA,
#'              "FUAADA","FUAADA","FUAADA","EL-SHAHIDI",
#'              "CHANTEL","CHANTEL","CHANTEL",NA,"KENDRA",
#'              "ALEXANDER","ALEXANDER","ALEX","ODESSA","ODESSA",
#'              "ODESSA","ODESSA",NA,"KEVIN","KEVIN",NA,
#'              "NAJEEB","NAJEEB","WILLIAM","WILL","WILLY",
#'              "WILLIAM","EMMA","EMMA","ALMAASA","ALMAASA",
#'              "ALMAASA",NA,NA,"JEFFERY","JEFFERY"),
#' surname = c("MCCREARY",
#'             "MCCREARY","MCCREARY","VATHANAVARIN","SARAH",
#'             "VATHANAVARIN","EL-SHAHIDI","EL-SHAHIDI",
#'             "EL-SHAHIDI","FUAADA","LENHART","LENHART","LENHART",
#'             NA,"VIGIL","CARTER","CARTER","CARTER",
#'             "RINHART","RINHART","RINHART","RINHART",NA,
#'             "EL-ASMAR","EL-ASMAR",NA,"CAMPBELL","CAMPBELL",
#'             "YAMAMOTO","YAMAMOTO","YAMAMOTO","YAMAMOTO",
#'             "BRAVE","BRAVE","BROWN","BROWN","BROWN",NA,NA,
#'             "LUCERO","LUCERO"),
#' event_date = as.Date(sample(seq.Date(Sys.Date()-365,Sys.Date(),1),41,replace = TRUE)),
#' stringsAsFactors = FALSE)
#'
#' uk_patient_id(
#'   data = id_test,
#'   id = list(
#'     nhs_number = 'nhs_number',
#'     hospital_number = 'local_patient_identifier',
#'     date_of_birth = 'patient_birth_date',
#'     sex_mfu = 'sex',
#'     forename = 'forename',
#'     surname = 'surname'
#'   ),
#'   .sortOrder = 'event_date',
#'   .forceCopy = TRUE
#' )[]
#'
#' @export

uk_patient_id <- function(data,
                           id = list(
                             nhs_number = 'nhs_number',
                             hospital_number = 'patient_hospital_number',
                             date_of_birth = 'date_of_birth',
                             sex_mfu = 'sex',
                             forename = 'forename',
                             surname = 'surname',
                             postcode = 'postcode'
                           ),
                           .sortOrder,
                           .keepValidNHS = FALSE,
                           .forceCopy = FALSE,
                           .experimental = FALSE) {

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::setDT(data.table::copy(data))
  } else {
    x <- data.table::setDT(data)
  }

  ## allow a forced sort order; but not necessary
  ## this is quite helpful practice to include
  if(!missing(.sortOrder)){
    data.table::setorderv(x,c(.sortOrder))
  }

  # apply other validity features
  # use SDcols version to ensure that the column name and argument name work if the same
  x[,id := .I]
  x[,tmp.recid := id]
  x[,tmp.idN := id]
  x[,tmp.GRP := .GRP]
  x[,tmp.stage := ""]

  ## set id to column 1
  data.table::setcolorder(x,c('id','tmp.recid','tmp.idN','tmp.GRP','tmp.stage'))

  ## VALIDIDTY MARKERS #########################################################
  ## NOTE: using exists(x,where=id) as the items X are within a list
  ## missing() does not recognise them as they are not primary arguments

  ## valid NHS numbers via checksum formula
  if(exists('nhs_number',where=id)){
    x[,tmp.valid.nhs := lapply(.SD,
                               function(x) epidm::valid_nhs(x) == 1),
      .SDcols = id$nhs_number]
  }

  ## known missing patient ID entries
  if(exists('hospital_number',where=id)){
    x[,tmp.valid.hos := lapply(.SD,
                               function(x) !x %in% c("UNKNOWN",
                                                     "NO PATIENT ID",
                                                     NA)),
      .SDcols = id$hospital_number]

    ## cleanup as some codes have massive leading or lagging whitespace
    x[, col := .(trimws(col)),
      env = list(col = id$hospital_number)]

  }

  ## known proxy unknown dates
  if(exists('date_of_birth',where=id)){
    x[,tmp.valid.dob := lapply(.SD,
                               function(x) !x %in% as.Date(c("1900-01-01",
                                                             "1800-01-01",
                                                             NA))),
      .SDcols = id$date_of_birth
    ]

    ## takes year and month
    x[,tmp.fuzz.ym := substr(dob,1,7),
      env = list(dob = id$date_of_birth)]

  }

  ## help to standardise sex/gender fields
  if(exists('sex_mfu',where=id)){
    x[,
      tmp.valid.sex := lapply(.SD,
                              function(x) grepl("^(M|F)",
                                                x,
                                                ignore.case=TRUE)),
      .SDcols = id$sex_mfu
    ]

    x[,
      col := .(
        data.table::fifelse(tmp.valid.sex,
                            toupper(substr(as.character(col),1,1)),
                            NA_character_)
      ),
      env = list(col = id$sex_mfu)
    ]
  }

  ## names cleanup and matching
  if(exists('surname',where=id)){

    x[,
      tmp.valid.n2 := !n2 %in% c("","NA",NA),
      env = list(n2 = id$surname)
    ]

    if(exists('forename',where=id)){
      x[,
        tmp.valid.n1 := !n %in% c("","NA",NA),
        env = list(n = id$forename)
      ]

      namecols <- c(id$surname,id$forename)

      ## look for name swaps, when the first and last have been swapped
      ## common in asian names
      x[,tmp.store.n1 := n,
        env = list(n = id$forename)]

      x[,tmp.store.n2 := n,
        env = list(n = id$surname)]

    } else {
      namecols <- c(id$surname)
    }

    ## clean up the strings for standard uppercase characters
    x[,
      (namecols) := lapply(.SD,
                           function(X) stringi::stri_trans_general(
                             stringi::stri_trans_toupper(X),
                             "Latin-ASCII")
      ),
      .SDcols = namecols
    ]

    ## soundex is a non-reversible pseudonymisation technique for names
    ## is always results in the pattern [A-Z][0-9][0-9][0-9]
    ## eg. phonics::soundex("bhattacharya") == 'B326'
    x[!is.na(n2),
      tmp.fuzz.n2 := phonics::soundex(
        stringr::word(
          gsub("[^[:alpha:]]", " ", n2),
          1
          )
        ),
      env = list(n2 = id$surname)]


    if(exists('forename',where=id)){
      x[,tmp.fuzz.n1 := base::substr(n1,1,1),
        env = list(n1 = id$forename)
      ]

      tmp.fuzz.n <- c('tmp.fuzz.n1','tmp.fuzz.n2')

    } else {
      tmp.fuzz.n <- c('tmp.fuzz.n2')
    }
  }

  if(exists('sex_mfu',where=id)){
    x[,
      tmp.valid.pcd := !is.na(pcd),
      env = list(pcd = id$postcode)
    ]

  }

  ## RECORD MATCHING ###########################################################
  ## a function to undertake the validation and dedupe steps
  ## stage = integer for flag
  ## validation = vector with validation cols
  ## group = vector with grouping cols


  stage <- function(stage = 1,
                    required,
                    validation,
                    group){

    if(all(sapply(required,
                  function(x) exists(x,where=id)))){

    valid <- paste(validation,collapse=" & ")

    ## use eval(parse(text=valid)) to allow the submission of a text
    #   string to be evaluated as code

      x[,`:=` (
        id = data.table::fifelse(
          eval(parse(text = valid)),
          data.table::fifelse(
            id==tmp.recid & tmp.idN==1,
            data.table::fifelse(
              data.table::last(tmp.idN)>1,
              data.table::last(id),
              id[1]),
            id),
          id),
        tmp.stage = data.table::fifelse(
          eval(parse(text = valid)),
          paste0(tmp.stage,paste0('s',stage)),
          tmp.stage)
      ),
      by = group
      ][
        ,`:=` (tmp.idN = .N,
               tmp.GRP = .GRP),
        by = 'id'
      ]

    return(x)
    }
  }

  ## S1: NHS + DOB ###########################################################

    stage(stage = 1,
          required = c('nhs_number',
                       'date_of_birth'),
          validation = c('tmp.valid.nhs',
                         'tmp.valid.dob'),
          group = c(id$nhs_number,
                    id$date_of_birth))

  ## S2: HOS + DOB ###########################################################

  stage(stage = 2,
        required = c('hospital_number',
                     'date_of_birth'),
        validation = c('tmp.valid.hos',
                       'tmp.valid.dob'),
        group = c(id$hospital_number,
                  id$date_of_birth))


  ## S3: NHS + HOS ###########################################################

  stage(stage = 3,
        required = c('nhs_number',
                     'hospital_number'),
        validation = c('tmp.valid.hos',
                       'tmp.valid.nhs'),
        group = c(id$hospital_number,
                  id$nhs_number))


  ## S4: NHS + NAME ##########################################################

  stage(stage = 4,
        required = c('nhs_number',
                     'surname'),
        validation = c('tmp.valid.nhs',
                       'tmp.valid.n2'),
        group = c(id$nhs_number,
                  id$surname))

  ## S5: HOS + NAME ##########################################################
  stage(stage = 5,
        required = c('hospital_number',
                     'surname'),
        validation = c('tmp.valid.hos',
                       'tmp.valid.n2'),
        group = c(id$hospital_number,
                  id$surname))

  ## S6: DOB + NAME ##########################################################
  stage(stage = 6,
        required = c('surname',
                     'date_of_birth'),
        validation = c('!tmp.valid.nhs',
                       'tmp.valid.n2',
                       'tmp.valid.dob'),
        group = c(id$date_of_birth,
                  namecols))

  ## S7: SEX + FULL NAME ##########################################################
  stage(stage = 7,
        required = c('surname',
                     'forename',
                     'sex_mfu'),
        validation = c('tmp.valid.n1',
                       'tmp.valid.n2',
                       'tmp.valid.sex'),
        group = c(id$sex_mfu,
                  namecols))


  ## S8: SEX + DOB + FUZZY NAME ##############################################
  stage(stage = 8,
        required = c('sex_mfu',
                     'date_of_birth',
                     'surname'),
        validation = c('tmp.valid.sex',
                       'tmp.valid.dob',
                       'tmp.valid.n2',
                       '!tmp.valid.nhs'),
        group = c(id$sex_mfu,
                  'tmp.fuzz.ym',
                  tmp.fuzz.n))

  ## S9: DOB + FUZZY NAME ####################################################
  stage(stage = 9,
        required = c('surname',
                     'date_of_birth'),
        validation = c('tmp.valid.dob',
                       'tmp.valid.n2'),
        group = c('tmp.fuzz.ym',
                  tmp.fuzz.n))

  ## S10: NAME + PCD ####################################################

  stage(stage = 10,
        required = c('postcode',
                     'surname'),
        validation = c('tmp.valid.pcd',
                       'tmp.valid.n2'),
        group = c(namecols,
                  id$postcode))


  ## S11: NAME SWAP  ####################################################
  ## TODO not working
  if(all(sapply(c('surname','forename','date_of_birth'),
                function(x) exists(x,where=id)))){

    x[tmp.idN == 1,
      `:=`(
        n1 = tmp.store.n2,
        n2 = tmp.store.n1,
        tmp.swap = TRUE
      ),
      env = list(
        n1 = id$forename,
        n2 = id$surname)
      ]

  stage(stage = 11,
        required = c('surname',
                     'forename'
,                     'date_of_birth'),
        validation = c('tmp.valid.dob',
                       'tmp.valid.n2',
                       'tmp.valid.n1'),
        group = c(namecols,
                  id$date_of_birth))

    x[tmp.swap == TRUE,
      `:=`(
        n1 = tmp.store.n1,
        n2 = tmp.store.n2
      ),
      env = list(
        n1 = id$forename,
        n2 = id$surname)
    ]

  }

  ## EXPERIMENTAL ##############################################################

  if(.experimental){
    ## capture legit IDs where NA or invalid

    ## set types for the columns; and clear out the invalid results
    ## we will bring the results together later

    for (i in c(id$nhs_number,id$hospital_number,id$date_of_birth)) {
      x[,
        col := .(
          data.table::fifelse(tmp.valid.hos,
                              as.numeric(col),
                              NA_integer_)
        ),
        env = list(col = i)
      ]

    }

    copyid <- c(id$nhs_number,id$hospital_number,id$date_of_birth,id$sex_mfu)

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
        data.table::fifelse(is.na(get(id$sex_mfu)),
                            "U",
                            get(id$sex_mfu))
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

