#' @title NHS Number Validity Check
#'
#' Check if NHS numbers are valid based on the checksum algorithm
#' This uses the first 9 digits, multiplied by 10 down to 2 eg digit 1x10, d2x9
#' The sum of the products of the first 9 digits are divided by 11
#' The remainder is checked against the 10th digit
#' Where the remainder is 11, it is replaced with 0
#' The result is a data frame with the NHS number and a column for the validity in logical format
#'
#' @param nhs_number a vector
#'
#' @examples
#' test <- floor(runif(1000,1000000000,9999999999))
#' valid_nhs(test)
#' valid_nhs(9434765919)
#' @return a vector, 1 if NHS number is valid, 0 if not valid
#' @export

valid_nhs <- function(nhs_number){

  NHSlength <- nchar(as.character(nhs_number))

  n1 <- as.numeric(substr(nhs_number, 1, 1))
  n2 <- as.numeric(substr(nhs_number, 2, 2))
  n3 <- as.numeric(substr(nhs_number, 3, 3))
  n4 <- as.numeric(substr(nhs_number, 4, 4))
  n5 <- as.numeric(substr(nhs_number, 5, 5))
  n6 <- as.numeric(substr(nhs_number, 6, 6))
  n7 <- as.numeric(substr(nhs_number, 7, 7))
  n8 <- as.numeric(substr(nhs_number, 8, 8))
  n9 <- as.numeric(substr(nhs_number, 9, 9))
  n10 <- as.numeric(substr(nhs_number, 10, 10))

  UniformNumberCheck <- ifelse((n1 == n2) &
                                 (n2 == n3) &
                                 (n3 == n4) &
                                 (n4 == n5) &
                                 (n5 == n6) &
                                 (n6 == n7) &
                                 (n7 == n8) &
                                 (n8 == n9) &
                                 (n9 == n10),
                               1,
                               0)


  Modulus <- (
    (n1 * 10) +
    (n2 * 9) +
    (n3 * 8) +
    (n4 * 7) +
    (n5 * 6) +
    (n6 * 5) +
    (n7 * 4) +
    (n8 * 3) +
    (n9 * 2)
    )

  Modulus <- (11 - (Modulus %% 11))

  ReturnValue <-
    ifelse(
      NHSlength == 10 & UniformNumberCheck != 1 &
        (Modulus == n10 | (Modulus == 11 & n10 == 0)),
    1, 0)

  ReturnValue <- ifelse(is.na(nhs_number),0,ReturnValue)

  return(ReturnValue)
}
