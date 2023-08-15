#' @title Clean alpha numeric strings (letters and numbers only)
#' @author Owen Pullen
#' @param str string to be cleaned
#' @param remove_punct boolean TRUE/FALSE, remove punctuation from string when TRUE all punctuation will be removed,
#' when FALSE punctuation that is not `!&',-.:;?` will be removed. Default FALSE
#' @param remove_all boolean TRUE/FALSE, removes all non A-z or 0-9 characters inducing spaces and punctuation,
#' remove_all will override other boolean arguments
#' @param alpha_num_id boolean TRUE/FALSE removes all non A-z characters except '-', including space
#' use on alphanumeric ID variables only
#' @returns character string
#' @export
#' @description
#' `r lifecycle::badge('experimental')`
#' Wrapper around regex to remove all non alpha-numeric characters
#' \itemize{
#' \item remove_punct = TRUE regex is `[^[:alnum:]]` will remove all non A-z and 0-9 characters
#' \item remove_punct = FALSE regex is `[^A-Za-z0-9!&',-.:;? ]` will remove all non A-z and 0-9 characters
#' \item remove_all = TRUE regex is `[^[:alnum:]]` will remove all non-alpha numeric characters
#' \item alpha_num_id = TRUE regex is `[^A-Za-z0-9-]` will remove all non alphanumeric characters expect `-`
#' \item If you are using one a Abc-123-Def-456 type ID and want to remove numeric all other characters use alpha_num_id = TRUE
#' }
#' If you are interested visit: https://regex101.com to test and build regexs
#' @seealso [clean_alpha()] [remove_non_numeric_char()] [remove_whitespace()]
#' @examples
#' clean_alpha_numeric("ABC-123 ;", remove_punct = FALSE, remove_all = FALSE)
#' clean_alpha_numeric("ABC-123 ;", remove_punct = TRUE, remove_all = FALSE)
#' clean_alpha_numeric("ABC-123 ;", remove_punct = FALSE, remove_all = TRUE)
#' clean_alpha_numeric("Abc-123;'#?>-D<ef-456", alpha_num_id = TRUE)

clean_alpha_numeric <- function(str, remove_punct = FALSE, remove_all = FALSE, alpha_num_id = FALSE) {
  if (alpha_num_id) {return(stringi::stri_replace_all_regex(str, "[^A-Za-z0-9-]", ""))}
  if (remove_all) {return(stringi::stri_replace_all_regex(str, "[^[:alnum:]]", ""))}
  if (remove_punct) {return(stringi::stri_replace_all_regex(str, "[^A-za-z0-9 ]", ""))}
  else {return(stringi::stri_replace_all_regex(str,"[^A-Za-z0-9!&',-.:;? ]",""))}
}

#' @title Clean alpha strings (letters only)
#' @author Owen Pullen
#' @param str string to be cleaned
#' @param remove_punct \itemize{\item boolean TRUE/ FALSE, TRUE removes all punctuation,
#' \item when FALSE will remove punctuation that is not `!&',-.:;?`
#' \item This argument is default FALSE
#' }
#' @param remove_all boolean `TRUE/FALSE`, TRUE removes anything non A-Z or a-z including spaces and punctuation
#' @export
#' @returns character string
#' @description
#' `r lifecycle::badge('experimental')`
#' Wrapper around a regex to remove all non letter characters, will remove all numbers from strings
#' \itemize{
#' \item remove_punct = FALSE `[^A-Za-z !&',-.:;?]|[0-9]` will remove all punctuation that<br>
#' is not `!&',-.:;?` will not remove spaces
#' \item remove_punct = TRUE `[^A-za-z ]` will remove all non A-z characters except spaces
#' \item remove_all = TRUE `[^[:alpha:]]` will remove all non A-z characters including spaces and all punctuation
#' }
#' If you are interested visit: https://regex101.com to test and build regexs
#' @seealso [clean_alpha_numeric()] [remove_non_numeric_char()] [remove_whitespace()]
#' @examples
#' clean_alpha("Test, string! ;", remove_punct = TRUE, remove_all = FALSE)
#' clean_alpha("Test, string! ;", remove_punct = FALSE, remove_all = FALSE)
#' # Argument remove_all is best used on ID variables only as it removes all spaces
#' clean_alpha("Test, string! ;", remove_punct = FALSE, remove_all = TRUE)

clean_alpha <- function(str,
                        remove_punct = FALSE,
                        remove_all = FALSE
                        ) {
  if (remove_all) {return(stringi::stri_replace_all_regex(str, "[^[:alpha:]]", ""))}
  if (remove_punct) {return(stringi::stri_replace_all_regex(str, "[^A-Za-z ]", ""))}
  else {return(stringi::stri_replace_all_regex(str,"[^A-Za-z!&',-.:;? ]|[0-9]", ""))}
}

#' @title Remove non numeric characters
#' @author OWen Pullen
#' @param str string to be cleaned
#' @export
#' @returns character string
#' @description
#' `r lifecycle::badge('stable')`
#' Wrapper around the `[^0-9]` regex to remove all non-numeric characters from a string
#' \itemize{
#' \item This is particularly useful for cleaning out numeric ID fields.
#' \item This should not be used where characters are expected
#' \item This function is useful for cleaning out all numeric Id's such as NHS number
#' }
#' @seealso [clean_alpha_numeric()] [clean_alpha()] [remove_whitespace()]
#' @examples
#' remove_non_numeric_char('123 and 456')
#' remove_non_numeric_char("12;3*456:78.'9")

remove_non_numeric_char <- function(str) {
  stringi::stri_replace_all_regex(str, "[^0-9]", "")
}

#' @title Remove all white space characters
#' @author Owen Pullen
#' @param str string to be cleaned
#' @export
#' @returns character string
#' @description
#' `r lifecycle::badge('experimental')`
#' \itemize{
#' \item Wrapper around `[[:space:]]` regex to remove all white space characters including space tab and new line characters
#' }
#' If you are interested visit: https://regex101.com to test and build regexs
#' @seealso [clean_alpha_numeric()] [clean_alpha()] [remove_non_numeric_char()]
#' @examples
#' remove_whitespace("A B C")
#' remove_whitespace("1 2 3")
#' remove_whitespace(". . .\t")

remove_whitespace <- function(str) {
  stringi::stri_replace_all_regex(str, "[[:space:]]", "")
}
