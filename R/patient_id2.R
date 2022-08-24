
uk_patient_id2 <- function(data,
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
                           .keepValGRPHS = FALSE,
                           .forceCopy = FALSE,
                           .experimental = FALSE) {

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::copy(data)
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
  x[,id := seq_len(.N)]
  x[,tmp.idN := seq_len(.N)]
  x[,tmp.GRP := 1L]
  x[,tmp.stage := ""]

  ## set id to column 1
  data.table::setcolorder(x,c('id','tmp.idN','tmp.GRP','tmp.stage'))

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
      namecols <- c(id$surname,id$forename)

      ## look for name swaps, when the first and last have been swapped
      ## common in asian names
      x[,tmp.flip.n1 := n,
        env = list(n = id$surname)]

      x[,tmp.flip.n2 := n,
        env = list(n = id$forename)]

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


  ## S1: NHS + DOB ###########################################################
  if(all(sapply(c('nhs_number','date_of_birth'),
                function(x) exists(x,where=id)))){
    x[tmp.valid.nhs & tmp.valid.dob,
      c('id',
        'tmp.stage') := .(
          id[1],
          paste0(tmp.stage,'s1')
          ),
      by = c(
        id$nhs_number,
        id$date_of_birth
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
    ]


  }

  ## S2: HOS + DOB ###########################################################
  if(all(sapply(c('hospital_number','date_of_birth'),
                function(x) exists(x,where=id)))){
    x[tmp.valid.hos & tmp.valid.dob
      ,c('id',
         'tmp.stage') := .(
           id[1],
           paste0(tmp.stage,';s2')
           ),
      by = c(
        id$hospital_number,
        id$date_of_birth
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
    ]

  }

  ## S3: NHS + HOS ###########################################################
  if(all(sapply(c('nhs_number','hospital_number'),
                function(x) exists(x,where=id)))){

    x[tmp.valid.nhs & tmp.valid.hos,
      c('id',
         'tmp.stage') := .(
           id[1],
           paste0(tmp.stage,';s3')
         ),
      by = c(
        id$nhs_number,
        id$hospital_number
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
    ]

  }

  ## S4: DOB + NAME ##########################################################
  if(all(sapply(c('surname','date_of_birth'),
                function(x) exists(x,where=id)))){

    x[tmp.valid.dob & tmp.valid.n2 & !tmp.valid.nhs,
      c('id',
         'tmp.stage') := .(
             id[1],
             paste0(tmp.stage,';s4')
         ),
      by = c(
        id$date_of_birth,
        namecols
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
    ]

  }

  ## S5: SEX + DOB + FUZZY NAME ##############################################
  if(all(sapply(c('surname','date_of_birth','sex_mfu'),
                function(x) exists(x,where=id)))){

    x[tmp.valid.sex & tmp.valid.dob & tmp.valid.n2 & !tmp.valid.nhs,
      c('id',
         'tmp.stage') := .(
             id[1],
             paste0(tmp.stage,';s5')
         ),
      by = c(
        id$sex_mfu,
        'tmp.fuzz.ym',
        tmp.fuzz.n
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
    ]

  }

  ## S6: DOB + FUZZY NAME ####################################################
  if(all(sapply(c('surname','date_of_birth'),
                function(x) exists(x,where=id)))){

    x[tmp.valid.dob & tmp.valid.n2 & !tmp.valid.nhs,
      c('id',
         'tmp.stage') := .(
             id[1],
             paste0(tmp.stage,';s6')
         ),
      by = c(
        'tmp.fuzz.ym',
        tmp.fuzz.n
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
    ]

  }

  ## S7: NAME SWAP  ####################################################
  if(all(sapply(c('surname','forename','nhs_number'),
                function(x) exists(x,where=id)))){

    x[tmp.valid.n2 & tmp.valid.nhs,
      c('id',
         'tmp.stage') := .(
             id[1],
             paste0(tmp.stage,';s7')
         ),
      by = c(
        id$nhs_number,
        id$surname,
        id$forename
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
    ]

  }

  ## S8: NAME SWAP  ####################################################
  if(all(sapply(c('surname','forename'),
                function(x) exists(x,where=id)))){

    x[,c('id',
         'tmp.stage') := .(
           data.table::fifelse(
             tmp.valid.dob & tmp.valid.n2 & !tmp.valid.nhs,
             id[1],
             id),
           data.table::fifelse(
             tmp.valid.dob & tmp.valid.n2 & !tmp.valid.nhs,
             paste0(tmp.stage,';s8'),
             tmp.stage)
         ),
      by = c(
        tmp.swap.n1 = id$surname,
        tmp.swap.n2 = id$forename
      )
    ][
      ,tmp.GRP := .GRP,
      by = 'id'
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

  if(.keepValGRPHS){
    data.table::setnames(x,'tmp.valid.nhs','valid_nhs')
  }

  ## cleanup and remove temporary vars
  tmpcols <- grep("^tmp.",names(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)

}

