#' Find sequences in NHS number using regex
#' @param value NHS number find sequences such as 9999999999 in NHS numbers using regex in strings
#' @author Owen Pullen
#' @export
#' @description Examples are not real NHS numbers please do not use real
#'  NHS numbers, in tests, code or documentation.
#'  Function using regex to remove consecutive 10 digit strings from NHS numbers,
#'  i.e strings contains 10 1's or 0's
#' variable or string containing numeric NHS numbers as class character
#' Can be re-purposed to clean other numeric id variables by altering regex {n}
#' Use: to be used inside dplyr::mutate()
#' e.g. df %>% mutate(nhsnumber = seq_nhs_number(nhsnumber)) # use inside mutate
#' @examples
#' nhs_no <- stringr::str_flatten(replicate(10, 9))
#' seq_nhs_number(nhs_no)
#' @seealso [validate_nhs_number()] [checksum_nhs_number()] [range_nhs_number()] [remove_non_numeric_char()]
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
#' Checks NHS number range against valid range for England, Scotland and Northern Ireland\cr
#' Examples are not real NHS numbers please do not use real
#'  NHS numbers, in tests, code or documentation
#' @examples
#' range_nhs_number("1")
#' range_nhs_number(200000000000)
#' range_nhs_number(1111111111)
#' @seealso [validate_nhs_number()] [checksum_nhs_number()] [seq_nhs_number()] [remove_non_numeric_char()]
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
#' Preforms the checksum validation algorithm  on an NHS number. Can also be called inside a mutate\cr
#' If you want to use the NHS checksum algorithm but do not want to do any other validation use checksum_nhs_number()\cr
#' Examples are not real NHS numbers please do not use real
#'  NHS numbers, in tests, code or documentation
#' @examples
#' checksum_nhs_number("1234567890")
#' example = data.frame(example_1 = nhsnumbergenerator::generate_nhs_number(100))
#' dplyr::mutate(example, checksum_nhs_number(example_1))
#' @seealso [range_nhs_number()] [seq_nhs_number()] [validate_nhs_number()] [remove_non_numeric_char()]
#'
checksum_nhs_number <- function(value) {
  value[stringi::stri_length(value) != 10] <- NA_character_
  digit_vector <- as.double(strsplit(substr(as.character(value), 1, 9), "")[[1]])
  check_digit <- (11 - sum(digit_vector * c(10, 9, 8, 7, 6, 5, 4, 3, 2)) %% 11)
  if (is.na(check_digit)) {
    return(NA_character_)
  }
  if (check_digit == 10) {
    return(NA_character_)
  }
  return(value)
}

#' valid_nhs
#' @param nhs_number an NHS number as a character string, interger or vector.
#' @param return_nhsno boolean [TRUE]/[FALSE] TRUE returns the NHS number as entered if valid and NA if invalid
#' FALSE returns a 0/1
#' @export
#' @author Owen Pullen
#' @description
#' NHS Number validation function calls checksum_nhs_number() which validates NHS numbers using the NHS checksum validation algorithm\cr
#' Also calls seq_nhs_number() which looks for 10 character single number sequences using regex e.g. 9999999999\cr
#' Calls range_nhs_number() checks NHS number is within the valid range for England, Wales, Scotland and Northern Ireland\cr
#' Calls remove_non_numeric_char() removes all non numeric characters from a string field
#' If you want to use the NHS checksum algorithm but do not want to do any other validation use checksum_nhs_number()\cr
#' Examples are not real NHS numbers please do not use real
#'  NHS numbers, in tests, code or documentation
#' @examples
#' valid_nhs("1234567890")
#' test_vect <- nhsnumbergenerator::generate_nhs_number(100)
#' valid_nhs(test_vect)
#' valid_nhs(NA)
#' @seealso [range_nhs_number()] [seq_nhs_number()] [checksum_nhs_number()] [remove_non_numeric_char()]
validate_nhs_number <- function(nhs_number, return_nhsno = FALSE) {
  value <- checksum_nhs_number(seq_nhs_number(range_nhs_number(
    remove_non_numeric_char(as.character(nhs_number))
  )))
  if (!return_nhsno) {
    return(as.integer(!is.na(value)))
  }
  return(as.character(value))
}
