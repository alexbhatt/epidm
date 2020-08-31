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
#' Grouping is based on four stages: 
#' \enumerate{
#' \item matching nhs number and date of birth
#' \item Hospital number &  Date of Birth &  Surname
#' \item NHS number & Hospital Number
#' \item Sex & Date of Birth & Surname IF nhs unknown
#' \item Sex & Date of Birth & Fuzzy Name
#' }
#' 
#'
#' @return patientID grouping variable
#'
#'
#' @import dplyr
#' @import tidyselect
#' @import phonics
#' @import stringr
#' @param data a data.frame or tibble containing the cleaned line list
#' @param nhs_number a column as a character containing the patient NHS numbers
#' @param hospital_number a column as a character containing the patient Hospital numbers
#' @param forename a column as a character containing the patient forename
#' @param surname a column as a character containing the patient surname
#' @param sex a column as a character containing the patient sex
#' @param date_of_birth a column as a date variable containing the patient date of birth in date format
#' @param sort_date a column as a date variable containing a date by which patients should be sorted, often the specimen date
#' @param flags logical if you want to keep the calculation data
#'
#' @return A dataframe with two new variables: id a unique patient id, and n_in_id an integer variable with the number of rows in the id.
#' 
#' @examples 
#' dat <- structure(list(nhs_n = c(9434765919, 5185293519, 3367170666), 
#' hosp_n = c(1L, 1L, 2L), forenm = c("Danger", "Danger", "Danger"), 
#' surnm = c("Mouse", "Mouse", "Mouse"), s = c("M", "M", "M"), 
#' dob = structure(c(-25567, -25567, -25567), class = "Date"), 
#' spec_date = structure(c(18262, 18294, 18324), class = "Date")), 
#' class = "data.frame", row.names = c(NA, -3L))
#' 
#' patient_id(data = dat, nhs_number = "nhs_n", hospital_number = "hosp_n", 
#'   forename = "forenm", surname = "surnm", sex = "s", 
#'   date_of_birth = "dob", sort_date = "spec_date", flags = TRUE)
#'
#'@export

patient_id <- function(data,
                       nhs_number,
                       hospital_number,
                       forename="NONAME",
                       surname="NONAME",
                       sex,
                       date_of_birth,
                       sort_date,
                       flags=FALSE) {
  

  ## grab NHS numbers for validation
  # use the valid_nhs function to asses if NHS numbers are real
  # nhs_data <- data[nhs_number]
  # real_nhs <- valid_nhs(nhs_data)

  ## quosures becuse they suck
  if(forename!="NONAME" & surname!="NONAME"){
    data <- data %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        nhs = nhs_number,
        hos = hospital_number,
        name1 = forename,
        name2 = surname,
        sex = sex,
        dob = date_of_birth,
        date = sort_date
      ) 
  }else{
    data <- data %>%
      dplyr::ungroup() %>%
      dplyr::rename(
        nhs = nhs_number,
        hos = hospital_number,
        sex = sex,
        dob = date_of_birth,
        date = sort_date
      ) 
  }
  if(forename=="NONAME") {
    data <- dplyr::mutate(data,name1=NA)
  }
  if(surname=="NONAME") {
    data <- dplyr::mutate(data,name2=NA)
  }
  
  ## to translate non UTF-8 characters in names in a uniform way
  data <- dplyr::mutate_at(data,
                           tidyselect::all_of(vars(contains("name",
                                                            ignore.case = T))),
                           ~stringi::stri_trans_general(.,"Latin-ASCII"))
  data <- dplyr::mutate_at(data,
                           tidyselect::all_of(vars(contains("name",
                                                            ignore.case = T))),
                           ~stringi::stri_trans_toupper(.))
  
  
  if (class(data$dob) != "Date" | class(data$date) != "Date") {
    stop("Date variables must be in date format")
  }
  
  data$valid_nhs <- valid_nhs_2(data$nhs)
  
  # # join the NHS validation result to the data
  # data <- dplyr::left_join(data,
  #                          real_nhs,
  #                          by = c("nhs" = "nhs_number")) 
  
  ## unknown data
  data <- data %>%
    dplyr::mutate(
      # nhs_unknown = !valid_nhs,
      nhs_unknown = valid_nhs == 0,
      dob_unknown = dob %in% c("1900-01-01", NA),
      hos_unknown = hos %in% c("UNKNOWN", "NO PATIENT ID", NA)
    ) %>%
    dplyr::mutate(id = as.numeric(1:nrow(data)),
                  s_0 = id) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup()
  
  ## STAGE 1: NHS number & Date of Birth
  data <- data %>%
    dplyr::group_by(nhs, dob, nhs_unknown) %>%
    dplyr::mutate(id = if_else(nhs_unknown | dob_unknown,
                               id, id[1]),
                  s_1 = id) %>%
    dplyr::ungroup()

  ## STAGE 2: Hospital number &  Date of Birth &  Surname
  data <- data %>%
    dplyr::group_by(hos, dob, name2, dob_unknown) %>%
    dplyr::mutate(id = ifelse(hos_unknown | dob_unknown | is.na(name2),
                              id, id[1]),
                  s_2 = id) %>%
    dplyr::ungroup()

  ## STAGE 3: NHS number & Hospital Number
  data <- data %>%
    dplyr::group_by(nhs, hos, nhs_unknown) %>%
    dplyr::mutate(id = ifelse(nhs_unknown & hos_unknown,
                              id, id[1]),
                  s_3 = id) %>%
    dplyr::ungroup()

  ## ONLY IF CONTAINS SURNAME
  ## STAGE 4: Sex & Date of Birth & Surname
  if(surname!="NONAME") {
    
    data <- data %>%
      dplyr::group_by(sex, dob, name2, name1) %>%
      dplyr::mutate(id=ifelse(is.na(sex) | dob_unknown | is.na(name2),
                       id,id[1]
                       ),
             s_4=id) %>%
      dplyr::ungroup()
  
  ## STAGE 5: Sex & Date of Birth & Fuzzy Name
    
    data <- data %>%
      dplyr::mutate(fuzz_name1=stringr::str_sub(name1,1,1),
                    fuzz_name2=phonics::soundex(stringr::word(name2,1))) %>% 
      dplyr::group_by(sex, dob, fuzz_name2, fuzz_name1) %>%
      dplyr::mutate(id=ifelse(dob_unknown | (is.na(name1)  & is.na(name2)) | is.na(sex),
                              id,id[1]),
                    s_5=id) %>%
      dplyr::ungroup()
    
  }

  ## count records/id
  data <- data %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(n_id = n()) %>%
    dplyr::select(id, contains("^s_[0-9]"), everything()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(id, date)

  if (flags == FALSE) {
    data <- select(data, -starts_with("s_"),-ends_with("_unknown"))
  }
  
  if(forename=="NONAME") {
    data <- select(data, -name1)
  }else{
    names(data)[names(data) == "name1"] <- quo_name(forename)
  }
  if(surname=="NONAME") {
    data <- select(data, -name2)
  }else{
    names(data)[names(data) == "name2"] <- quo_name(surname)
  }
  
  ## quosure fix
  names(data)[names(data) == "date"] <- quo_name(sort_date)
  names(data)[names(data) == "dob"] <- quo_name(date_of_birth)
  names(data)[names(data) == "nhs"] <- quo_name(nhs_number)
  names(data)[names(data) == "hos"] <- quo_name(hospital_number)
  names(data)[names(data) == "sex"] <- quo_name(sex)

  return(data)
}


