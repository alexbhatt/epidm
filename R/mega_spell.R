#' 
#' @title Mega Spell grouping
#' 
#' Group overlapping time periods, with start and end dates
#' 
#' @return a data frame with 3 new variables: indx, a grouping flag; and new start and end dates
#' 
#' @import dplyr
#' @param data data frame, this can be piped in
#' @param start_date the start dates for the intervals
#' @param end_date the end dates for the intervals
#' @param group_vars in a vector, the all vars used to group records
#' 
#' @example
#' test <- data.frame(
#'   id = c(99),
#'   org = c("YXZ",rep("ZXY",5)),
#'   spell_start = as.Date(
#'     c(
#'       "2020-03-01",
#'       "2020-07-07",
#'       "2020-02-08",
#'       "2020-04-28",
#'       "2020-03-15",
#'       "2020-07-01"
#'     )
#'   ),
#'   spell_end = as.Date(
#'     c(
#'       "2020-03-10",
#'       "2020-07-26",
#'       "2020-05-22",
#'       "2020-04-30",
#'       "2020-05-20",
#'       "2020-07-08"
#'     )
#'   )
#' )
#' 
#' test %>% mega_spells(start_date=spell_start,end_date=spell_end,group_vars=c(id,org))
#' 
#' @export

mega_spells <- function(data,
                        start_date,
                        end_date,
                        group_vars){

  t1 <- data %>% dplyr::pull({{start_date}}) %>% class()
  t2 <- data %>% dplyr::pull({{end_date}}) %>% class()
  if(!t1 %in% c("Date","POSIXct","POSIXt")) stop("start_date must be in date format")
  if(!t2 %in% c("Date","POSIXct","POSIXt")) stop("end_date must be in date format")
  
  data %>% 
    dplyr::group_by(dplyr::across({{group_vars}})) %>% 
    dplyr::arrange({{start_date}},{{end_date}},.by_group=T) %>%
    dplyr::mutate(indx = c(0, 
                    cumsum(as.numeric(dplyr::lead({{start_date}})) >
                             cummax(as.numeric({{end_date}})))[-dplyr::n()])
    ) %>%  
    dplyr::group_by(indx,.add=TRUE) %>%
    dplyr::mutate(megaspell_start_date=min({{start_date}},na.rm=TRUE),
           megaspell_end_date=max({{end_date}})) %>% 
    dplyr::ungroup()

}

