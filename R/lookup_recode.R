#' Lookup table switch handler
#'
#' A function to call an epidm lookup table and
#' recode  where we are aware of a new value.
#'
#' Built in are  the organism re-classifications and specimen_type groupings
#' and a manual mode.
#'
#' @param col a vector containing the variable to be created/changed
#' @param type a character value to denote the lookup
#' @param .import a list  in the order list(new,old) containing the
#' values for another lookup table existing in the environment
#'
#' @return a vector/list object of the recoded field
#' @export
#'
#' @examples
#' respec_example <- data.frame(
#'   sp=respeciate_organism$previous_organism_name,
#'   ge=gsub("([A-Za-z]+).*", "\\1",respeciate_organism$previous_organism_name)
#'   )
#' respec_example$new <- lookup_recode("species",respec_example$sp)
#' head(respec_example)
#'
#' lookup_recode(
#'   "ALCALIGENES DENITRIFICANS",
#'   type = 'manual',
#'   .import=list(respeciate_organism$organism_species_name,
#'                respeciate_organism$previous_organism_name)
#'   )


lookup_recode <- function(col,
                          type=c('species','specimen','manual'),
                          .import = NULL) {


  if (type == 'manual' & missing(.import)) {
    stop("supply a two column list for the lookup vector in the format list(new,old)")
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
    ## data(respeciate_organism)
    lk <- as.list(
      setNames(
        specimen_type_grouping$specimen_group,
        specimen_type_grouping$specimen_type
      )
    )

  } else if (type == 'manual') {

    lk <- as.list(
      setNames(
        .import[[1]],
        .import[[2]]
      )
    )

  }

  x <- unname(lk[col])

  return(x)

}
