#' Lookup table switch handler
#'
#' @description
#' `r lifecycle::badge('stable')`
#' A function to call an epidm lookup table and
#' recode  where we are aware of a new value.
#'
#' Built in are  the organism re-classifications and specimen_type groupings
#' and a manual mode.
#'
#' @param src a character, vector or column containing the value(s) to be referenced
#' @param type a character value to denote the lookup table used
#' @param .import a list  in the order list(new,old) containing the
#' values for another lookup table existing in the environment
#'
#' @importFrom purrr imap
#' @importFrom stats na.omit setNames
#'
#'
#' @return a list object of the recoded field
#' @export
#'
#' @examples
#' df <- data.frame(
#'   spec = c(
#'     sample(grep(")",
#'                 respeciate_organism$previous_organism_name,
#'                 value=TRUE,
#'                 invert = TRUE),
#'            9),
#'     "ESCHERICHIA COLI","SARS-COV-2","CANDIDA AUREUS"),
#'   type = sample(specimen_type_grouping$specimen_type,12),
#'   date = sample(seq.Date(from = Sys.Date()-365,
#'                          to = Sys.Date(),
#'                          by = "day"),12)
#' )
#' df <- df[order(df$date),]
#'
#' # show the data before the changes
#' df
#'
#' # check the lookup tables
#' # observe the changes
#' head(respeciate_organism[1:2])
#' df$species <- lookup_recode(df$spec,'species')
#' df[,c('spec','species','date')]
#'
#' head(specimen_type_grouping)
#' df$grp <- lookup_recode(df$type,'specimen')
#' df[,c('species','type','grp','date')]
#'
#' # for a tidyverse use
#' # df %>% mutate(spec=lookup_recode(spec,'species))
#'
#' # manual input of your own lookup
#' # .import=list(new,old)
#' lookup_recode(
#'   "ALCALIGENES DENITRIFICANS",
#'   type = 'manual',
#'   .import=list(respeciate_organism$organism_species_name,
#'                respeciate_organism$previous_organism_name)
#'   )



lookup_recode <- function(src,
                          type=c('species',
                                 'specimen',
                                 'inpatient_admission_method',
                                 'inpatient_discharge_destination',
                                 'ecds_destination_code',
                                 'manual'),
                          .import = NULL) {


  if (type == 'manual' & missing(.import)) {
    stop("supply a two item list for the lookup table inthe format list(new,old)")
  }

  if(type == "species"){

    ## calls upon the lookup table stored in the epidm package
    ## data(respeciate_organism)
    lk <- as.list(
      setNames(
        respeciate_organism$organism_species_name,
        respeciate_organism$previous_organism_name
      )
    )

  } else if (type == "specimen") {

    ## calls upon the lookup table stored in the epidm package
    ## data(specimen_type_grouping)
    lk <- as.list(
      setNames(
        specimen_type_grouping$specimen_group,
        specimen_type_grouping$specimen_type
      )
    )

  } else if (type == "inpatient_admission_method") {

    ## calls upon the internal lookup table stored in the epidm package
    ## epidm:::group_inpatient_admission_method
    lk <- as.list(
      setNames(
        group_inpatient_admission_method[[2]],
        group_inpatient_admission_method[[1]]
      )
    )
  } else if (type == "inpatient_discharge_destination") {

    ## calls upon the internal lookup table stored in the epidm package
    ## epidm:::group_inpatient_admission_method
    lk <- as.list(
      setNames(
        group_inpatient_discharge_destination[[2]],
        group_inpatient_discharge_destination[[1]]
      )
    )
  } else if (type == "ecds_destination_code") {

    ## calls upon the internal lookup table stored in the epidm package
    ## epidm:::group_inpatient_admission_method
    lk <- as.list(
      setNames(
        group_ecds_discharge_destination[[2]],
        group_ecds_discharge_destination[[1]]
      )
    )

  } else if (type == 'manual') {

    lk <- as.list(
      stats::setNames(
        .import[[1]], ## NEW replacement
        .import[[2]]  ## OLD value
      )
    )

  }


  # create a list of the original and lookup elements
  x <- unname(lk[src])


  ## what happens if the value does not exist in the lookup
  ## use the original value, as they will often be overwritten
  nullReplace <- function(x,y) {
    if(is.null(x)) {
      src[y]
    } else {
      x
    }
  }

  ## purrr::imap uses two arguments, the first is the mapped item
  ## the second is the index number of that item
  ## you do not need to specify the arguments if there are only 2
  x <- purrr::imap(x,nullReplace)

  if (type %in% c("ecds_destination_code",
                  "inpatient_admission_method",
                  "inpatient_discharge_destination")) {

    if(is.na(x)){
      return(x)
    } else {
      return(as.character(x))
    }

  }

  else {

    return(x)
  }

}
