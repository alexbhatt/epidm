#'
#' @title Time grouping
#'
#' Group overlapping time periods, with start and end dates, or start dates with a rolling window
#'
#' @return a data frame with 4 new variables: indx, a grouping flag; and new start and end dates
#'
#' @import dplyr
#' @import rlang .data
#' @param data data frame, this can be piped in
#' @param date_start the start dates for the grouping
#' @param date_end the end dates for the grouping
#' @param window if there is no end date, a time window which will be applied to the start date
#' @param min_varname set variable name for the time period minimum
#' @param max_varname set variable name for the time period maximum
#' @param group_vars in a vector, the all vars used to group records
#'
#' @examples
#' episode_test <- data.frame(
#'   id = c(99),
#'   org = c(rep("E. coli",7),
#'           rep("K. pneumoniae",6)
#'           ),
#'   specimen_date = as.Date(
#'     c(
#'       "2020-03-01",
#'       "2020-03-11",
#'       "2020-03-16",
#'       "2020-04-03",
#'       "2020-04-04",
#'       "2020-04-24",
#'       "2020-04-27",
#'       "2020-07-17",
#'       "2020-07-23",
#'       "2020-07-30",
#'       "2020-08-04",
#'       "2020-07-12",
#'       "2020-08-24"
#'     )
#'   )
#' )
#' episode_test %>% group_time(date_start=specimen_date,window=14,group_vars=c(id,org))
#'
#' spell_test <- data.frame(
#'   id = c(rep(99,6),rep(88,4)),
#'   provider = c("YXZ",rep("ZXY",5),rep("XYZ",4)),
#'   spell_start = as.Date(
#'     c(
#'       "2020-03-01",
#'       "2020-07-07",
#'       "2020-02-08",
#'       "2020-04-28",
#'       "2020-03-15",
#'       "2020-07-01",
#'       "2020-01-01",
#'       "2020-01-12",
#'       "2019-12-25",
#'       "2020-03-28"
#'     )
#'   ),
#'   spell_end = as.Date(
#'     c(
#'       "2020-03-10",
#'       "2020-07-26",
#'       "2020-05-22",
#'       "2020-04-30",
#'       "2020-05-20",
#'       "2020-07-08",
#'       "2020-01-23",
#'       "2020-03-30",
#'       "2020-01-02",
#'       "2020-04-20"
#'     )
#'   )
#' )
#'
#' spell_test %>% group_time(date_start=spell_start,
#'                           date_end=spell_end,
#'                           min_varname="spell_min_date",
#'                           max_varname="spell_max_date",
#'                           group_vars=c(id,org))
#'
#' @export


group_time <- function(.data,
                       date_start,
                       date_end,
                       window,
                       min_varname="window_min",
                       max_varname="window_max",
                       group_vars){

  ## check date start
  if(!missing(date_start)){
    t1 <- .data %>% dplyr::pull({{date_start}}) %>% class()
    if(!t1 %in% c("Date","POSIXct","POSIXt")) stop("date_start must be in date format")

    .data <- .data %>%
      dplyr::mutate(
        date=as.numeric({{date_start}})
      )
  } else {
    stop("input date required for start_date")
  }

  ## check end points (date_end or window)
  if(!missing(date_end) & !missing(window)){
    stop("window or date_end argument required")
  } else if (!missing(date_end)) {
    t2 <- .data %>% dplyr::pull({{date_end}}) %>% class()
    if(!t2 %in% c("Date","POSIXct","POSIXt")) stop("date_start must be in date format")

    print("end date specified")
    .data <- .data %>%
      dplyr::mutate(
        window_end=as.numeric({{date_end}})
      )

  } else {
    print(paste(window,"day rolling window applied"))
    .data <- .data %>%
      dplyr::mutate(
        window_end = date + {{window}}
      )
  }

  .data <- .data %>%
    dplyr::group_by(dplyr::across({{group_vars}})) %>%
    dplyr::arrange({{date_start}},.by_group=T)  %>%
    dplyr::mutate(
      window_start = dplyr::lead(date,default = max(date)),
      window_cmax = cummax(window_end),
      indx = paste0(
        dplyr::cur_group_id(),".",
        c(0,cumsum(window_start > window_cmax))[-dplyr::n()]
      )
    ) %>%
    dplyr::group_by(indx,.add=TRUE) %>%
    dplyr::mutate(
      {{min_varname}} := min({{date_start}},na.rm=TRUE),
      {{max_varname}} := max(as.Date(window_cmax,origin="1970-01-01"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(date,window_start,window_end,window_cmax))

  return(.data)

}
