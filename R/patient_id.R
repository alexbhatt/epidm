#'
#' @title Patient ID record grouping
#'
#' Groups patient records from multiple isolates with a single patientID
#' GROUPED by nhs_number, hospital_number, date_of_birth, assigns a unique IDno to each patient.
#' sort by specimen date
#' generates a unique sequential number from 1-X for each record
#' then based on grouping, reassigns records to same number
#' group by different combos of patient identifiers, also dont include invalid nhs_numbers
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
#' @return patientID grouping variable
#'
#' @import dplyr
#' @importFrom phonics soundex
#' @importFrom stringr word
#' @importFrom stringi stri_trans_general stri_trans_toupper
#' @importFrom rlang .data
#'
#' @param .data a data.frame or tibble containing the cleaned line list
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
#' dat <- dplyr::tribble(
#'   ~nhs_n, ~hosp_n, ~sex, ~dateofbirth, ~firstname, ~lastname,
#'   9434765919, 13L, "M", "1988-10-06", "Danger", "Mouse",
#'   9434765919, 13L, "M", "1988-06-10", "Danger", "MÔuse",
#'   9434765919, 13L, "M", "1900-01-01", "Denger", "Mouse",
#'   NA,         NA,  "M", "1988-10-06", "Danger", "Moose",
#'   NA,         13L, "M", "1988-10-06", "Danger", "Moose",
#'   3367170666, 13L, "M", "1988-10-06", "DANGER", "Mouse",
#'   5185293519, 13L, "M", "1988-10-06", "Danger", "MOUSe",
#'   5185293519, 31L, "M", "1988-10-06", "Danger", "Mouse",
#'   5185293519, 31L, "M", "1988-10-06", "Danger", "Mouse",
#'   8082318562, 96L, "F", "2020-01-28", "Crazy",  "Frog",
#'   NA,         96L, "U", "2020-01-28", "Crazy",  "FROG",
#'   NA,         96L, "U", "2020-01-28", "Krazy",  "Frug",
#'   NA,         96L, "F", "2020-01-28", "C",      "Frog"
#' ) %>% dplyr::mutate(dateofbirth=as.Date(dateofbirth))
#'
# uk_patient_id(.data = dat,
#               nhs_number = "nhs_n",
#               hospital_number = "hosp_n",
#               forename = "forenm",
#               surname = "surnm",
#               sex = "s",
#               date_of_birth = "date_of_birth",
#               sort_by = "spec_date")
#'
#' @export



uk_patient_id <- function(.data,
                          nhs_number,
                          hospital_number,
                          date_of_birth,
                          sex,
                          forename="NONAME",
                          surname="NONAME",
                          sort_by) {

  ## apply valid_nhs flag
  .data$tmp.valid.nhs <- epidm::valid_nhs(.data[[nhs_number]])

  # apply other validity features
  .data <- .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      id = seq(1:dplyr::n()),
      tmp.valid.nhs = tmp.valid.nhs == 1,
      tmp.valid.dob = !{{date_of_birth}} %in% c("1900-01-01", NA),
      tmp.valid.hos = !{{hospital_number}} %in% c("UNKNOWN", "NO PATIENT ID", NA),
      tmp.valid.sex = grepl("^M|F",{{sex}},ignore.case=T)
    ) %>%
    dplyr::arrange({{sort_by}})  %>%

    ## S1: NHS + DOB ###########################################################
  dplyr::group_by(
    dplyr::across(c(
      {{nhs_number}},
      {{date_of_birth}})
    )
  ) %>%
    dplyr::mutate(
      id = ifelse(
        tmp.valid.nhs & tmp.valid.dob,
        id[1],
        id)
    ) %>%
    dplyr::ungroup() %>%

    ## S2: HOS + DOB ###########################################################
  dplyr::group_by(
    dplyr::across(c(
      {{hospital_number}},
      {{date_of_birth}}
    ))
  ) %>%
    dplyr::mutate(
      id = ifelse(
        tmp.valid.hos & tmp.valid.dob,
        id[1],
        id)
    ) %>%
    dplyr::ungroup() %>%

    ## S3: NHS + HOS ###########################################################
  dplyr::group_by(
    dplyr::across(c(
      {{nhs_number}},
      {{hospital_number}}
    ))
  ) %>%
    dplyr::mutate(
      id = ifelse(
        tmp.valid.nhs & tmp.valid.hos,
        id[1],
        id)
    ) %>%
    dplyr::ungroup()

  if(surname!="NONAME"){
    ## S4: SEX + DOB + NAME ####################################################
    .data <- .data %>%
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::contains("name",ignore.case=T),
          .fns = ~stringi::stri_trans_general(
            stringi::stri_trans_toupper(.),
            "Latin-ASCII")
          )) %>%
      dplyr::mutate(
        tmp.valid.n2 = !{{surname}} %in% c("","NA",NA)
      ) %>%
      dplyr::group_by(
        dplyr::across(c(
          {{sex}},
          {{date_of_birth}},
          {{surname}},
          {{forename}}
        ))
      ) %>%
      dplyr::mutate(
        id = ifelse(
          tmp.valid.sex & tmp.valid.dob & tmp.valid.n2,
          id[1],
          id)
      ) %>%
      dplyr::ungroup() %>%

    ## S5: SEX + DOB + FUZZY NAME ##############################################
    dplyr::mutate(
      tmp.fuzz.n1 = base::substr(.data[[forename]],1,1),
      tmp.fuzz.n2 = phonics::soundex(stringr::word(.data[[surname]],1)),
      tmp.fuzz.ym = substr(as.character(.data[[date_of_birth]]),1,7)
    ) %>%
      dplyr::group_by(
        dplyr::across(c(
          {{sex}},
          tmp.fuzz.ym,
          tmp.fuzz.n1,
          tmp.fuzz.n2
        ))
      ) %>%
      dplyr::mutate(
        id = ifelse(
          tmp.valid.sex & tmp.valid.dob & tmp.valid.n2,
          id[1],
          id
        )
      ) %>%
      dplyr::ungroup()

  }

  .data <- .data %>%
    select(-c(dplyr::starts_with("tmp.")))

  return(.data)

}


