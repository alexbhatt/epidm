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
#' @param .sortOrder optional; a column as a character to allow a sorting
#'   order on the id generation
#' @param .forceCopy default FALSE; TRUE will force data.table to take a copy
#'   instead of editing the data without reference
#'
#' @return A dataframe with one new variable:
#' \describe{
#'   \item{`id`}{a unique patient id}
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
#'   testdate = sample(seq.Date(Sys.Date()-21,Sys.Date(),"day"),13,replace = T)
#' )
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
                          .forceCopy = FALSE) {

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  ## allow a forced sort order; but not necessary
  if(!missing(.sortOrder)){
    setorderv(x,c(.sortOrder))
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

  ## set types for the columns; and clear out the invalid results
  ## we will bring the results together later
  x[,
    c(nhs_number) := .(
      data.table::fifelse(tmp.valid.nhs,
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
    c(sex_mfu) := .(
      data.table::fifelse(tmp.valid.sex,
                          toupper(substr(as.character(get(sex_mfu)),1,1)),
                          NA_character_)
      )
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
    ,tmp.idN := .N,
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
    ,tmp.idN := .N,
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
    ,tmp.idN := .N,
    by = 'id'
  ]

  if(surname!="NONAME"){
    ## S4: SEX + DOB + NAME ####################################################
    namecols <- grep("name",names(x),ignore.case=T,value=T)
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
      ,tmp.idN := .N,
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
      ,tmp.idN := .N,
      by = 'id'
    ]

  }

  ## cleanup and remove temporary vars
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
    ]

  ## capture legit IDs where NA or invalid

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

  ## order the final results
  if(!missing(.sortOrder)){
    setorderv(x,c('id',.sortOrder))
  } else {
    setorder(x,'id')
  }

  return(x)

}

