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
#' \item Date of Birth & Surname
#' \item Sex & Full name
#' \item Sex & Year and Month of Birth & Fuzzy Name
#' \item Year and Month of Birth & Fuzzy Name
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
#' @param .keepStages optional, default FALSE; to generate a new column (stageMatch)
#'   to retain the stage information for which the record matched the group.
#' @param .keepValidNHS optional, default FALSE; set TRUE if you wish to retain
#'   the column with the NHS checksum result stored as a BOOLEAN
#' @param .sortOrder optional; a column as a character to allow a sorting
#'   order on the id generation
#' @param .forceCopy optional, default FALSE; TRUE will force data.table to take a copy
#'   instead of editing the data without reference
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
#' id_test <-
#'   data.frame(
#'     stringsAsFactors = FALSE,
#'     record_id = c(1L,2L,3L,4L,
#'                   5L,6L,7L,8L,9L,10L,11L,12L,13L,14L,15L,
#'                   16L,17L,18L,19L,20L,21L,22L,23L,24L),
#'     nhs_number = c(9435754422,
#'                    9435754422,NA,9435754422,5555555555,NA,
#'                    9435773982,NA,9999999999,NA,9435773982,NA,
#'                    9435802508,9435802508,NA,NA,9435802508,9435802508,NA,
#'                    3333333333,NA,9999999999,9435817777,
#'                    9435817777),
#'     local_patient_identifier = c(NA,"IG12067",
#'                                  NA,NA,"IG12067","IG12067","KR2535","KR2535",
#'                                  "KR2535",NA,NA,NA,"UK8734","UK8734",NA,NA,
#'                                  "UK8734","UK8734",NA,NA,"JH45204",
#'                                  "HS45202","HS45202","JH45204"),
#'     patient_birth_date = c("1993-07-16",
#'                            "1993-07-16","1993-07-16","1993-07-16",
#'                            "1993-07-16",NA,"1967-02-10",NA,"1967-02-10",NA,NA,
#'                            "1967-02-10",NA,NA,"1952-10-22","1952-10-22",
#'                            "1952-10-22",NA,"1947-09-14","1947-09-14",
#'                            "1947-09-14","1947-09-14","1947-09-14",
#'                            "1947-09-14"),
#'     sex = c("Male","Male",
#'             "Male","Male",NA,"Male","Female","Female",
#'             "Female","Female","Female","Female","Male",
#'             "Male","Male","Male","Male","Male","Male",
#'             "Male","Male","Male",NA,"Male"),
#'     forename = c(NA,"DENNIS",
#'                  NA,NA,"DENNIS",NA,"ELLIE","ELLIE",NA,
#'                  "ELLIE","ELLIE","ELLIE","IAN","IAN","MALCOLM",
#'                  "IAN","IAN",NA,"GRANT","ALAN","ALAN","ALAN",
#'                  "GRANT","ALAN"),
#'     surname = c(NA,"NEDRY",
#'                 "NEDRY",NA,"NEDRY","NEDRY","SATTLER","SATTLER",
#'                 NA,"SATTLER","SATTLER","SATTLER","M",NA,
#'                 "IAN","MALCOLM","MALCOLM",NA,"ALAN","GRANT",
#'                 "GRANT","GRANT","ALAN","GRANT"),
#'     postcode = c("HA4 0FF",
#'                  "HA4 0FF","HA4 0FF",NA,"HA4 0FF","HA4 0FF",
#'                  "L3 1DZ","L3 1DZ","L3 1DZ","L3 1DZ",NA,"L3 1DZ",
#'                  "BN14 9EP",NA,"BN14 9EP",NA,NA,NA,"CW6 9TX",
#'                  "CW6 9TX",NA,NA,NA,NA),
#'     specimen_date = c("2024-08-14",
#'                       "2023-02-03","2023-02-07","2023-02-04",
#'                       "2023-02-09","2024-08-14","2021-03-28","2021-03-28",
#'                       "2021-03-28","2021-03-28","2021-03-28",
#'                       "2021-03-28","2024-07-06","2024-07-06","2024-07-06",
#'                       "2023-10-31","2023-10-31","2023-10-31",
#'                       "2022-01-23","2022-01-24","2022-01-25","2022-01-26",
#'                       "2022-01-27","2022-01-28")
#'   )
#'
#'
#' uk_patient_id(
#'   data = id_test,
#'   id = list(
#'     nhs_number = 'nhs_number',
#'     hospital_number = 'local_patient_identifier',
#'     date_of_birth = 'patient_birth_date',
#'     sex_mfu = 'sex',
#'     forename = 'forename',
#'     surname = 'surname',
#'     postcode = 'postcode'
#'   ),
#'   .sortOrder = 'specimen_date',
#'   .useStages = c(1:11),
#'   .keepStages = TRUE,
#'   .forceCopy = TRUE)[]
#'
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
                          .useStages = c(1:11),
                          .keepStages = FALSE,
                          .keepValidNHS = FALSE,
                          .sortOrder,
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
  x[,tmp.id := id]
  x[,tmp.idMode := id]
  x[,tmp.recid := id]
  x[,tmp.idN := id]
  x[,tmp.GRP := .GRP]
  x[,tmp.stage := ""]
  x[,tmp.sN := 1]

  ## set id to column 1; setting the others for debugging purposes
  data.table::setcolorder(x,c('id',
                              'tmp.id',
                              'tmp.idMode',
                              'tmp.idN',
                              'tmp.sN',
                              'tmp.GRP',
                              'tmp.stage',
                              'tmp.recid'
                              )
                          )

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

  ## MODE #####################################################################
  # R dosent have a base mode function
  # this is needed to select the ID with the most results
  # NOTE: works with numbers and characters
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  ## RECORD MATCHING FUNCTION #################################################
  #  stage deduplication
  # an internal function to undertake the validation and dedupe steps
  #    within the uk_patient_id function for records with matching identifiers
  #
  # stage integer for flag
  # required vector for which fields are necessary
  # validation vector with validation columns
  # group vector with grouping columns
  # the ingest data frame with updated id column

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

      # create a flag to allow filtering of groups
      # this will identify if there are groups of 2 or more
        x[,
            tmp.sN :=
            data.table::fifelse(
              eval(parse(text = valid)),
              .N,
              1),
          by = group
        ]

        # this gives the mode from the PREVIOUS stage for ID onto the record
        x[,
          tmp.idMode := Mode(id),
          by = 'tmp.GRP'
          ]

        # sort the data so you can use positionally grab the right ID
          data.table::setorder(x,'tmp.GRP','tmp.sN','tmp.idN','tmp.recid')

        x[,`:=` (
          # group the records on a shared id
          id = data.table::fifelse(
            tmp.sN > 1,
            data.table::fcase(
              max(tmp.idN) == 1, Mode(tmp.idMode),
              min(tmp.idN) == 1, Mode(tmp.idMode[tmp.idN>1]),
              default = Mode(id)
            ),
              id
            ),
          # flag the records within the group which matched on that stage
          tmp.stage = data.table::fifelse(
            tmp.sN > 1,
            data.table::fifelse(
              tmp.stage == "",
              paste0('s',stage),
              paste0(tmp.stage,paste0('s',stage))
              ),
            tmp.stage)
        ),
        by = group
        ][
          ,`:=` (tmp.idN = .N,
                 tmp.GRP = .GRP),
          by = 'id'
        ]

        # capture the id for the previous stage
        x[, tmp.id := id]

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

  ## S6: DOB + SURNAME ########################################################

  stage(stage = 6,
        required = c('surname',
                     'date_of_birth'),
        validation = c('!tmp.valid.nhs',
                       'tmp.valid.n2',
                       'tmp.valid.dob'),
        group = c(id$date_of_birth,
                  namecols))

  ## S7: SEX + FULL NAME ######################################################

  stage(stage = 7,
        required = c('surname',
                     'forename',
                     'sex_mfu'),
        validation = c('tmp.valid.n1',
                       'tmp.valid.n2',
                       'tmp.valid.sex'),
        group = c(id$sex_mfu,
                  namecols))


  ## S8: SEX + YM DOB + FUZZY NAME ###############################################

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

  ## S9: YM DOB + FUZZY NAME ##################################################

  stage(stage = 9,
        required = c('surname',
                     'date_of_birth'),
        validation = c('tmp.valid.dob',
                       'tmp.valid.n2'),
        group = c('tmp.fuzz.ym',
                  tmp.fuzz.n))

  ## S10: NAME + PCD ##########################################################

  stage(stage = 10,
        required = c('postcode',
                     'surname'),
        validation = c('tmp.valid.pcd',
                       'tmp.valid.n2'),
        group = c(namecols,
                  id$postcode))


  ## S11: NAME SWAP + DOB #####################################################

  if(11 %in% .useStages){

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
      x <- merge(x, dt_swap,
                         by.x = c("tmp.store.forename", "tmp.store.surname", "tmp.valid.dob"),
                         by.y = c("tmp.store.forename.switch", "tmp.store.surname.switch", "tmp.valid.dob"),
                         all = FALSE,
                         all.x = TRUE,
                         all.y = FALSE)

      data.table::setnames(x,
                           'tmp.idN.x',
                           'tmp.idN')

      x[is.na(tmp.swap),
                ':=' (
                  tmp.store.forename.switch = n1,
                  tmp.store.surname.switch = n2
                ),
                env = list(
                  n1 = id$forename,
                  n2 = id$surname)]

      x[,
        tmp.valid.n.swap := sum(tmp.swap,na.rm=TRUE)>=1,
        by = c('tmp.store.forename.switch',
               'tmp.store.surname.switch')]

      stage(stage = 11,
            required = c('forename',
                         'surname'),
            validation = c('tmp.valid.n1',
                           'tmp.valid.n2',
                           'tmp.valid.n.swap'),
            group = c('tmp.store.forename.switch',
                      'tmp.store.surname.switch',
                      id$date_of_birth))
    }

  }

  ## CLEANUP ##################################################################
  ## retain and remove temporary vars
  if(.keepValidNHS){
    data.table::setnames(x,'tmp.valid.nhs','valid_nhs')
  }

  if(.keepStages){
    data.table::setnames(x,'tmp.stage','stageMatch')
  }

  tmpcols <- grep("^tmp.",names(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

    ## order the final results
  if(!missing(.sortOrder)){
    data.table::setorderv(x,c('id',.sortOrder))
  } else {
    data.table::setorder(x,'id')
  }

  return(x)

}

