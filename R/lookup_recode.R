#' Lookup switch handler
#'
#' A function to call an epidm lookup table and
#' recode  where we are aware of a new value.
#'
#' Currently working with the organism reclassifications and
#' specimen_type groupings
#'
#' @param type a character value to denote the lookup
#' @param col a vector containing the organism species name
#' @param ... two vectors, in the order new,old of another lookup
#'
#' @return a vector containing the updated scientific name for the organism
#' @export
#'
#' @examples
#' respec_example <- data.frame(
#'   sp=respeciate_organism$previous_organism_name,
#'   ge=gsub("([A-Za-z]+).*", "\\1",respeciate_organism$previous_organism_name)
#'   )
#' respec_example$new <- respec_org(respec_example$sp)
#' head(respec_example)


lookup_recode <- function(type,
                          col,
                          ...=NULL) {


  if(type=="species"){

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

  } else {

      lk <- as.list(
      setNames(
        ...
      )
    )

  }

  x <- unname(lk[col])

  return(x)

}
