#' alternate method nhs number validation
#' @keywords internal
valid_nhs_internal <- function(nhs_number){

  ## create a nested function
  ## if(is.na(x)) uses the first element of a vector
  ## sapply of the nested function applies the function to each element

  checksum_algorithm <- function(NHS){

    ## immediately fail the missing NHS numbers OR
    ## if its got the wrong number of digits

    if(is.na(NHS)){

      ReturnValue <- 0

    } else if(nchar(as.character(NHS))!=10) {

      ReturnValue <- 0

    } else {

      ## break up the NHS number into its individual digits
      n1 <- as.numeric(substr(NHS, 1, 1))
      n2 <- as.numeric(substr(NHS, 2, 2))
      n3 <- as.numeric(substr(NHS, 3, 3))
      n4 <- as.numeric(substr(NHS, 4, 4))
      n5 <- as.numeric(substr(NHS, 5, 5))
      n6 <- as.numeric(substr(NHS, 6, 6))
      n7 <- as.numeric(substr(NHS, 7, 7))
      n8 <- as.numeric(substr(NHS, 8, 8))
      n9 <- as.numeric(substr(NHS, 9, 9))
      n10 <- as.numeric(substr(NHS, 10, 10))

      ## are all the numbers the same
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

      ## what is the remainder
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

      ## 1 is valid, 0 is not
      ReturnValue <-
        ifelse(
          UniformNumberCheck != 1 &
            (Modulus == n10 | (Modulus == 11 & n10 == 0)),
          1, 0)

    }

    return(ReturnValue)

  }

  ## to prevent NA errors when looking at a vector of NHS numbers
  sapply(nhs_number,checksum_algorithm)

}

#' gen test nhs number for single number only called in gen test nhs_number
#' @keywords internal

gen_testing_nhsno_single <- function() {
  value <- 0
  while (valid_nhs_internal(value)==0) {
    value <- as.double(paste(sample(1:9, 10, replace = T), collapse = ''))
    value <- ifelse(value >= 0101010000 & value < 9990000000,
      value,
      NA_integer_
      )
  }
  return(value)
}

#' gen test nhs number
#' @param n number of times to be replicated
#' @keywords internal
gen_test_nhs <- function(n = 1) {
  as.character(replicate(n, gen_testing_nhsno_single()))
}
