#'
#' @title NHS Number Validity Check
#'
#' Check if NHS numbers are valid based on the checksum algorithm
#' This uses the first 9 digits, multiplied by 10 down to 2 eg digit 1x10, d2x9
#' The sum of the products of the first 9 digits are divided by 11
#' The remainder is checked against the 10th digit
#' Where the remainder is 11, it is replaced with 0
#' The result is a data frame with the NHS number and a column for the validity in logical format
#'
#' @param nhs a vector or data.frame column in format data$nhs for testing
#' @param keep to retain the checksum values
#' @import dplyr
#' @import tidyr
#' @examples
#' test <- floor(runif(1000,1000000000,9999999999))
#' valid_nhs(test)
#' @return a data.frame containing the NHS numbers and a logical flag if its valid
#' @export

valid_nhs <- function(nhs_number,
                      keep = FALSE) {

  if(!class(nhs_number) %in% c("character", "numeric", "factor","data.frame", "tbl", "tbl_df")) {
    stop("You must input the column in data$column format or as a vector")
  }

  ## import vector or data.frame column
  x <- as.data.frame(nhs_number) 
  names(x) <- "nhs"
  
  x <- x %>% 
    dplyr::mutate(nhs=gsub(" ","",as.character(nhs))) %>%
    dplyr::distinct(nhs)

  ## calculate checksum
  # split the NHS number into its 10 seperate digits
  # postive look-behind for each character
  x <- x %>%
    tidyr::separate(
      nhs,
      c(paste0("n", seq(1:10))),
      sep = "(?<=.)", 
      convert = TRUE,
      remove = FALSE,
      extra = "drop"
    ) %>%
    dplyr::mutate(
      n1 = n1 * 10,
      n2 = n2 * 9,
      n3 = n3 * 8,
      n4 = n4 * 7,
      n5 = n5 * 6,
      n6 = n6 * 5,
      n7 = n7 * 4,
      n8 = n8 * 3,
      n9 = n9 * 2
    ) %>%
    dplyr::rename(check_digit=n10) %>%
    # sum values and check modulus
    # compare modulus to check_digit
    dplyr::mutate(n_sum = n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8 + n9,
                  check_remainder = n_sum %% 11,
                  check_remainder = dplyr::if_else(
                    11 - check_remainder != 11,
                    11 - check_remainder,
                    0)
                  ) %>%
    dplyr::mutate(
      valid_nhs = dplyr::case_when(
        nchar(nhs) != 10 ~ FALSE,
        as.numeric(nhs) %% 1111111111 == 0 ~ FALSE,
        check_digit == check_remainder ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    dplyr::rename("nhs_number"=nhs)

  if(keep == FALSE) {
    x <- x %>%
      dplyr::select(nhs_number, valid_nhs)
  }

  return(x)
}

#'
#' @title NHS Number Validity Check 2
#'
#' Check if NHS numbers are valid based on the checksum algorithm
#' This uses the first 9 digits, multiplied by 10 down to 2 eg digit 1x10, d2x9
#' The sum of the products of the first 9 digits are divided by 11
#' The remainder is checked against the 10th digit
#' Where the remainder is 11, it is replaced with 0
#' The result is a data frame with the NHS number and a column for the validity in logical format
#'
#' @param nhs a vector
#' @examples
#' test <- floor(runif(1000,1000000000,9999999999))
#' valid_nhs_2(test)
#' valid_nhs_2(9434765919)
#' @return a vector, 1 if NHS number is valid, 0 if not valid
#' @export

valid_nhs_2 <- function(nhs_number){
  
  NHSlength<-nchar(as.character(nhs_number))
  
  A<-as.numeric(substr(nhs_number,1,1))
  B<-as.numeric(substr(nhs_number,2,2))
  C<-as.numeric(substr(nhs_number,3,3))
  D<-as.numeric(substr(nhs_number,4,4))
  E<-as.numeric(substr(nhs_number,5,5))
  F<-as.numeric(substr(nhs_number,6,6))
  G<-as.numeric(substr(nhs_number,7,7))
  H<-as.numeric(substr(nhs_number,8,8))
  I<-as.numeric(substr(nhs_number,9,9))
  J<-as.numeric(substr(nhs_number,10,10))
  
  UniformNumberCheck <- ifelse(
    (A==B)&(B==C)&(C==D)&(D==E)&(E==F)&(F==G)&(G==H)&(H==I)&(I==J),
    1, 0)
  
  
  Modulus<-((A*10)+(B*9)+(C*8)+(D*7)+(E*6)+(F*5)+(G*4)+(H*3)+(I*2))
  Modulus<-(11-(Modulus%%11))
  
  ReturnValue <- ifelse(
    ((Modulus==J) & (UniformNumberCheck!=1) & (NHSlength==10))|((Modulus==11) & (J==0) & (UniformNumberCheck!=1) & (NHSlength==10)),
  1, 0)
  
  ReturnValue <- ifelse(is.na(nhs_number), 0, ReturnValue)
  
  return(ReturnValue)
}
