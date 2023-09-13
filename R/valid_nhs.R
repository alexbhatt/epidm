#' Find sequences in NHS number using regex
#' @param value NHS number find sequences such as 9999999999 in NHS numbers using regex in strings
#' @author Owen Pullen
#' @export
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Check if NHS numbers are valid based on the checksum algorithm\cr
#'
#' This uses the first 9 digits, multiplied by 10 down to 2 eg digit 1x10, d2x9\cr
#'
#' The sum of the products of the first 9 digits are divided by 11\cr
#'
#' The remainder is checked against the 10th digit\cr
#'
#' Where the remainder is 11, it is replaced with NA_character\cr
#'
#' Examples are not real NHS numbers please do not use real\cr
#' NHS numbers, in tests, code or documentation.\cr
#' Function using regex to remove consecutive 10 digit strings from NHS numbers,\cr
#' i.e strings contains 10 1's or 0's\cr
#' variable or string containing numeric NHS numbers as class character\cr
#' Can be re-purposed to clean other numeric id variables by altering regex `n`\cr
#' Use: to be used inside dplyr::mutate()\cr
#' e.g. df %>% mutate(nhsnumber = seq_nhs_number(nhsnumber)) # use inside mutate
#'
#' @examples
#' nhs_no <- stringr::str_flatten(replicate(10, 9))
#' seq_nhs_number(nhs_no)
#' @seealso [valid_nhs()] [checksum_nhs_number()] [range_nhs_number()] [remove_non_numeric_char()]
#'
seq_nhs_number <- function(value) {
  stringi::stri_replace_all_regex(
    as.character(value),
    "^(?=([0-9]))\\1{10}$",
    NA_character_
  )
}

#' range_nhs_number
#' @param value an NHS number for validation using range check
#' @export
#' @author Owen Pullen
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Checks NHS number range against valid range for England, Scotland and Northern Ireland\cr
#' Examples are not real NHS numbers please do not use real
#'  NHS numbers, in tests, code or documentation
#' @examples
#' range_nhs_number("1")
#' range_nhs_number(200000000000)
#' range_nhs_number(1111111111)
#' @seealso [valid_nhs()] [checksum_nhs_number()] [seq_nhs_number()] [remove_non_numeric_char()]
#'
range_nhs_number <- function(value) {
  ifelse(
    as.double(value) >= 0101010000 &
      as.double(value) < 9990000000,
    as.double(value),
    NA_integer_
  )
}

#' checksum_nhs_number
#' @param value an NHS number as an integer or character string or vector for validation using
#'  the NHS designed checksum algorithm
#' @export
#' @author Owen Pullen
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Preforms the checksum validation algorithm  on an NHS number. Can also be called inside a mutate\cr
#' If you want to use the NHS checksum algorithm but do not want to do any other validation use checksum_nhs_number()\cr
#' Examples are not real NHS numbers please do not use real
#'  NHS numbers, in tests, code or documentation
#' @examples
#' checksum_nhs_number("1234567890")
#' example = data.frame(example_1 = epidm:::gen_test_nhs(100))
#' dplyr::mutate(example, checksum_nhs_number(example_1))
#' @seealso [range_nhs_number()] [seq_nhs_number()] [valid_nhs()] [remove_non_numeric_char()]
#'
checksum_nhs_number <- function(value) {
  value[stringi::stri_length(value) != 10 | stringi::stri_detect_regex(value,"[^0-9]")] <- NA_character_
  ## what is the remainder
  Modulus <- c(
    (as.integer(substr(value, 1, 1)) * 10) +
      (as.integer(substr(value, 2, 2)) * 9) +
      (as.integer(substr(value, 3, 3)) * 8) +
      (as.integer(substr(value, 4, 4)) * 7) +
      (as.integer(substr(value, 5, 5)) * 6) +
      (as.integer(substr(value, 6, 6)) * 5) +
      (as.integer(substr(value, 7, 7)) * 4) +
      (as.integer(substr(value, 8, 8)) * 3) +
      (as.integer(substr(value, 9, 9)) * 2)
  )
  n10 <- as.integer(substr(value, 10, 10))
  Modulus <- 11 - (Modulus %% 11)
  ReturnValue <- ifelse((Modulus == n10 | (Modulus == 11 & n10 == 0)),
    value, NA_character_)
  return(as.character(ReturnValue))
}

#' @title NHS Number Validity Check
#' @param nhs_number an NHS number as a character string, interger or vector.
#' @param return_nhsno boolean [TRUE]/[FALSE] TRUE returns the NHS number as entered if valid and NA if invalid
#' FALSE returns a 0/1
#' @export
#' @author Owen Pullen
#' @examples
#' valid_nhs("1234567890")
#' test_vect <- epidm:::gen_test_nhs(100)
#' valid_nhs(test_vect)
#' valid_nhs(NA)
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Check if NHS numbers are valid based on the checksum algorithm\cr
#'
#' This uses the first 9 digits, multiplied by 10 down to 2 eg digit 1x10, d2x9\cr
#'
#' The sum of the products of the first 9 digits are divided by 11\cr
#'
#' The remainder is checked against the 10th digit\cr
#'
#' Where the remainder is 11, it is replaced with 0\cr
#'
#' There is also an option to replace the output with valid NHS numbers or NA if invalid
#'
#' # More info
#' NHS Number validation function calls checksum_nhs_number() which validates NHS numbers using the NHS checksum validation algorithm\cr
#' Also calls seq_nhs_number() which looks for 10 character single number sequences using regex e.g. 9999999999\cr
#' Calls range_nhs_number() checks NHS number is within the valid range for England, Wales, Scotland and Northern Ireland\cr
#' Calls remove_non_numeric_char() removes all non numeric characters from a string field
#' If you want to use the NHS checksum algorithm but do not want to do any other validation use checksum_nhs_number()\cr
#' \cr
#' Examples are not real NHS numbers please do not use real
#'  NHS numbers, in tests, code or documentation
#'
#' @examples
#' valid_nhs("1234567890")
#' test_vect <- epidm:::gen_test_nhs(100)
#' valid_nhs(test_vect)
#' valid_nhs(NA)
#'
#' @returns A vector, 1 if NHS number is valid, 0 if not valid as an integer, also if\cr
#' return_nhsno = TRUE returns NHS number if valid and NA_character
#' @export
#' @seealso [range_nhs_number()] [seq_nhs_number()] [checksum_nhs_number()] [remove_non_numeric_char()]
#'
valid_nhs <- function(nhs_number, return_nhsno = FALSE) {
  value <- checksum_nhs_number(seq_nhs_number(range_nhs_number(
    remove_non_numeric_char(as.character(nhs_number))
  )))
  if (!return_nhsno) {
    return(as.integer(!is.na(value)))
  }
  return(as.character(value))
}
