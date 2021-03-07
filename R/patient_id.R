#' @title Patient ID record grouping
#'
#' @description
#' `r lifecycle::badge('stable')`
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
#' @return patientID grouping variable addedd to the data.frame
#'
#'
#' @import data.table
#' @importFrom data.table .N ':='
#' @importFrom phonics soundex
#' @importFrom stringr word
#' @importFrom stringi stri_trans_general stri_trans_toupper
#'
#' @param x a data.frame or data.table containing the cleaned line list
#' @param nhs_number a column as a character containing the patient NHS numbers
#' @param hospital_number a column as a character containing the patient Hospital numbers
#' @param date_of_birth a column as a date variable containing the patient date of birth in date format
#' @param sex a column as a character containing the patient sex
#' @param forename a column as a character containing the patient forename; leave as NONAME if unavailable
#' @param surname a column as a character containing the patient surname; leave as NONAME if unavailable
#'
#' @return A dataframe with two new variables: id a unique patient id, and n_in_id an integer variable with the number of rows in the id
#'
#' @examples
#' id_test <- data.frame(
#'   nhs_n = c(
#'     9434765919,9434765919,9434765919,NA,NA,
#'     3367170666,5185293519,5185293519,5185293519,8082318562,NA,NA,NA
#'   ),
#'   hosp_n = c(
#'     '13','13','13','UNKNOWN','13','13','13','31','31','96','96',NA,'96',
#'   ),
#'   sex = c('M', 'M', 'M', 'M', 'M', 'M', 'M', 'M', 'M', 'F', 'U', 'U', 'F'),
#'   dateofbirth = as.Date(
#'     c(
#'       '1988-10-06','1988-06-10','1900-01-01','1988-10-06','1988-10-06',
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
#'   )
#' )
#' id_test <- uk_patient_id(x = id_test,
#'                    nhs_number = 'nhs_n',
#'                    hospital_number = 'hosp_n',
#'                    forename = 'firstname',
#'                    surname = 'lastname',
#'                    sex = 'sex',
#'                    date_of_birth = 'dateofbirth')
#' print(id_test)
#'
#' @export

uk_patient_id <- function(x,
                          nhs_number,
                          hospital_number,
                          date_of_birth,
                          sex,
                          forename="NONAME",
                          surname="NONAME") {

  ## convert object if its not already
  if(data.table::is.data.table(x)==FALSE) {
    data.table::setDT(x)
  }

  ## setup variables for entry into data.table

  # apply other validity features
  # use SDcols version to ensure that the column name and argument name work if the same
  x[,id := seq(1:.N)]
  ## set id to column 1
  data.table::setcolorder(x,'id')

  x[,tmp.valid.nhs := lapply(.SD,function(x) epidm::valid_nhs(x) == 1),
    .SDcols = nhs_number]
  x[,tmp.valid.dob := lapply(.SD,
                             function(x) !x %in% as.Date(c("1900-01-01", NA))),
    .SDcols = date_of_birth
    ]
  x[,tmp.valid.hos := lapply(.SD,
                             function(x) !x %in% c("UNKNOWN", "NO PATIENT ID", NA)),
    .SDcols = hospital_number]
  x[,
    tmp.valid.sex := lapply(.SD,function(x) grepl("^M|F",x,ignore.case=T)),
    .SDcols = sex
    ]
  x[,
    c(sex) := .(
      data.table::fifelse(tmp.valid.sex,toupper(substr(sex,1,1)),"U")
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
        sex,
        'tmp.fuzz.ym',
        'tmp.fuzz.n1',
        'tmp.fuzz.n2'
      )
    ]

  }

  ## cleanup and remove temporary vars
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
    ]



  return(x)

}

