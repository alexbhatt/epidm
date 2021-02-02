#' @title Patient ID record grouping
#'
#' @description
#' Groups patient records from multiple isolates with a single integer patientID
#' by grouping patient identifiers.
#'
#' Grouping is based on five stages:
#' \enumerate{
#' \item matching nhs number and date of birth
#' \item Hospital number &  Date of Birth
#' \item NHS number & Hospital Number
#' \item Sex & Date of Birth & Surname IF nhs unknown
#' \item Sex & Date of Birth & Fuzzy Name
#' }
#'
#' @return patientID grouping variable addedd to the data.frame
#'
#' @import data.table
#' @importFrom data.table .N .I ':='
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
#' @param sort_by a column to give a priority sort order if required
#'
#' @return A dataframe with two new variables: id a unique patient id, and n_in_id an integer variable with the number of rows in the id
#'
#' @examples
#' dat <- data.frame(
#'   nhs_n = c(
#'     9434765919,9434765919,9434765919,NA,NA,
#'     3367170666,5185293519,5185293519,5185293519,8082318562,NA,NA,NA
#'   ),
#'   hosp_n = c(
#'     '13','13','13','NA','13','13','13','31','31','96','96','96','96'
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
# uk_patient_id(x = dat,
#               nhs_number = nhs_n,
#               hospital_number = hosp_n,
#               forename = firstname,
#               surname = lastname,
#               sex = sex,
#               date_of_birth = dateofbirth)
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
    x <- data.table::as.data.table(x)
  }

  ## setup variables for entry into data.table
  nhs_number <- substitute(nhs_number)
  hospital_number <- substitute(hospital_number)
  date_of_birth <- substitute(date_of_birth)
  sex <- substitute(sex)
  forename <- substitute(forename)
  surname <- substitute(surname)

  # apply other validity features
  x[,id := seq(1:.N)]
  x[,tmp.valid.nhs := epidm::valid_nhs(eval(nhs_number)) == 1]
  x[,tmp.valid.dob := !eval(date_of_birth) %in% c("1900-01-01", NA)]
  x[,tmp.valid.hos := !eval(hospital_number) %in% c("UNKNOWN", "NO PATIENT ID", NA)]
  x[,tmp.valid.sex := grepl("^M|F",eval(sex),ignore.case=T)]

  ## S1: NHS + DOB ###########################################################
  x[,
    id := data.table::fifelse(
      tmp.valid.nhs & tmp.valid.dob,
      id[1],
      id
    ),
    by = c(
      deparse(nhs_number),
      deparse(date_of_birth)
      )
  ]


  ## S2: HOS + DOB ###########################################################
  x[,
    id := data.table::fifelse(
      tmp.valid.hos & tmp.valid.dob,
      id[1],
      id),
    by = c(
      deparse(hospital_number),
      deparse(date_of_birth)
      )
  ]

  ## S3: NHS + HOS ###########################################################
  x[,
    id := data.table::fifelse(
      tmp.valid.nhs & tmp.valid.hos,
      id[1],
      id),
    by = c(
      deparse(nhs_number),
      deparse(hospital_number)
      )
  ]

  if(surname!="NONAME"){
    ## S4: SEX + DOB + NAME ####################################################
    namecols <- grep("name",names(dat),ignore.case=T,value=T)
    x[,
      (namecols) := lapply(.SD,
             function(X) stringi::stri_trans_general(
               stringi::stri_trans_toupper(X),
               "Latin-ASCII")
      ),
      .SDcols = namecols
    ]
    x[,
      tmp.valid.n2 := !eval(surname) %in% c("","NA",NA)
    ]
    x[,
      id := data.table::fifelse(
        tmp.valid.sex & tmp.valid.dob & tmp.valid.n2,
        id[1],
        id),
      by = c(
        deparse(sex),
        deparse(date_of_birth),
        deparse(surname),
        deparse(forename)
        )
    ]


    ## S5: SEX + DOB + FUZZY NAME ##############################################
    x[,tmp.fuzz.n1 := base::substr(eval(forename),1,1)]
    x[,tmp.fuzz.n2 := phonics::soundex(stringr::word(eval(surname),1))]
    x[,tmp.fuzz.ym := substr(eval(date_of_birth),1,7)]

    x[,
      id := data.table::fifelse(
        tmp.valid.sex & tmp.valid.dob & tmp.valid.n2,
        id[1],
        id),
      by = c(
        deparse(sex),
        tmp.fuzz.ym,
        tmp.fuzz.n1,
        tmp.fuzz.n2
        )
    ]

  }

  ## cleanup and remove temporary vars
  x[,
    lapply(.SD,NULL),
    .SDcols = grep("^tmp.",names(x),value=TRUE)
    ][]

  return(x)

}

