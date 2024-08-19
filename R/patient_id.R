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
#' @param .useStages optional, default 1:11; set to 1 if you wish patient ID to
#' be assigned cases with the same DOB and NHS number, set to 2 if you wish patient
#' ID to be assigned to cases with the same hospital number (HOS) and DOB, set to
#' 3 if you wish patient ID to be assigned cases with the same NHS and HOS number,
#' set to 4 if you wish patient ID to be assigned cases with the same NHS number
#' and surname, set to 5 if you wish patient ID to be assigned cases with the same
#' hospital number and surname, set to 6 if you wish patient ID to be assigned
#' cases with the same DOB and surname, set to 7 if you wish patient ID to be
#' assigned cases with the same sex and full name, set to 8 if you wish patient
#' ID to be assigned cases with the same sex, DOB and fuzzy name, set to 9 if you
#' wish patient ID to be assigned cases with the same DOB and fuzzy name, set to
#' 10 if you wish patient ID to be assigned cases with the same name and postcode,
#' set to 11 if you wish patient ID to be assigned cases with the same first name
#' or second name in changing order and date of birth.
#'
#'
#' @return A dataframe with one new variable:
#' \describe{
#'   \item{`id`}{a unique patient id}
#'   \item{`valid_nhs`}{if retained using argument `.keepValidNHS=TRUE`, a
#'     BOOLEAN containing the result of the NHS checksum validation}
#' }
#'
#' @examples
#' uk_patient_id(
#'  data = head(epidm::lab_data),
#'  id = list(
#'    nhs_number = 'nhs_number',
#'    hospital_number = 'local_patient_identifier',
#'    date_of_birth = 'patient_birth_date',
#'    sex_mfu = 'sex',
#'    forename = 'forename',
#'    surname = 'surname'
#'    postcode = 'postcode'
#'  ),
#'  .sortOrder = 'specimen_date',
#'  .forceCopy = TRUE
#' )[]
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
                          .useStages = c(1:11),
                          .sortOrder,
                          .keepValidNHS = FALSE,
                          .forceCopy = FALSE) {

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

  ## VALIDITY MARKERS ##########################################################
  ## NOTE: using exists(x,where=id) as the items X are within a list
  ## missing() does not recognise them as they are not primary arguments

  ## valid NHS numbers via checksum formula
  if(exists('nhs_number',where=id)){

    #Remove all letters from NHS number
    x[, nhs := gsub("\\D+" , "" , nhs),
     env = list(nhs = id$nhs_number)]

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

  if(exists('postcode',where=id)){

  ##Removal of spaces from postcode
  x[, pcd := gsub(" ", "", pcd),
     env = list(pcd = id$postcode)]

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

    # capture .useStages from primary function call
    if(stage %in% .useStages){

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

 if(max(.useStages) %in% 11){

  if(all(sapply(c('surname','forename','date_of_birth'),
                function(x) exists(x,where=id)))){

    #Switch forename and surname
    x[tmp.idN == 1,
      ':=' (tmp.store.forename = n1,
            tmp.store.surname = n2,
            tmp.store.forename.switch = n2,
            tmp.store.surname.switch = n1,
            tmp.swap = TRUE),
      env = list(
        n1 = id$forename,
        n2 = id$surname)
      ]

  cols_swap <- c("tmp.store.forename.switch", "tmp.store.surname.switch", "tmp.valid.dob", "tmp.idN")

  #Extract columns where surname and forename have been switched
  dt_swap <- x[tmp.valid.n2 == TRUE & tmp.valid.dob == TRUE & tmp.idN == 1, ..cols_swap]

  #Create a match column
  dt_swap <-  dt_swap[, tmp.match := TRUE]

  #Merge  data tables back together - to match on where forename and surname have been switched
  dt_merged <- merge(x, dt_swap,
          by.x = c("tmp.store.forename", "tmp.store.surname", "tmp.valid.dob"),
          by.y = c("tmp.store.forename.switch", "tmp.store.surname.switch", "tmp.valid.dob"),
          all = FALSE,
          all.x = TRUE,
          all.y = FALSE)

    group = c(id$date_of_birth)
    stage = 11

    dt_merged[, tmp.idN :=  data.table::fifelse(
              tmp.match == TRUE,
                .N,
                0,
              na = 0),
              by = group
              ] [,  `:=` (
                  id = data.table::fifelse(
                    tmp.idN > 1,
                          data.table::last(id),
                          id),
                  tmp.stage = data.table::fifelse(
                    tmp.idN > 1,
                    paste0(tmp.stage, paste0('s', stage)),
                    tmp.stage)
                  ),
                        by = group]


    x <- dt_merged
                }

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

