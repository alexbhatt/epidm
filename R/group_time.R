#'
#' @title Time grouping
#'
#' Group overlapping time periods, with start and end dates, or start dates with a rolling window
#'
#' @return a data frame with 4 new variables: indx, a grouping flag; and new start and end dates
#'
#' @import dplyr
#' @importFrom rlang .data
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
#'   id = c(rep(99,6),rep(88,4),rep(3,3)),
#'   provider = c("YXZ",rep("ZXY",5),rep("XYZ",4),rep("YZX",3)),
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
#'       "2020-03-28",
#'       "2020-01-01",
#'       rep(NA,2)
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
#'       "2020-04-20",
#'       "2020-01-01",
#'       rep(NA,2)
#'     )
#'   )
#' )
#'
#' spell_test %>% group_time(date_start=spell_start,
#'                           date_end=spell_end,
#'                           group_vars=c(id,provider),
#'                           min_varname="spell_min_date",
#'                           max_varname="spell_max_date",
#'                           drop_original = FALSE)
#'
#' @export


group_time <- function(.data,
                       date_start,
                       date_end,
                       window,
                       group_vars,
                       min_varname="date_min",
                       max_varname="date_max",
                       drop_original = TRUE
){



  ## check date start
  if(!missing(date_start)){
    t1 <- .data %>% dplyr::pull({{date_start}}) %>% class()
    if(!t1 %in% c("Date","POSIXct","POSIXt")) stop("date_start must be in date format")

    .data <- .data %>%
      dplyr::mutate(
        dateNum=as.numeric({{date_start}})
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
        window_end = dateNum + {{window}}
      )
  }

  ## retain or rename original window varnames if they match the rewrites
  if(min_varname %in% names(.data)) {
    if(drop_original == TRUE) {
      .data <- .data %>% dplyr::select(-{{min_varname}})
    } else {
      .data <- .data %>% dplyr::rename("{min_varname}_original" := {{min_varname}})
    }
  }
  if(max_varname %in% names(.data)) {
    if(drop_original == TRUE) {
      .data <- .data %>% dplyr::select(-{{max_varname}})
    } else {
      .data <- .data %>% dplyr::rename("{max_varname}_original" := {{max_varname}})
    }
  }

  ## group time
  .data <- .data %>%
    dplyr::group_by(dplyr::across({{group_vars}})) %>%
    dplyr::mutate(N = dplyr::n())

  ## remove these from the group analysis
  ## groups with 1 observation by definition cannot join with others
  ## missing start dates are null entries as well
  chunkOut <- .data %>%
    dplyr::filter(is.na(dateNum) | N==1)

  .data <- .data %>%
    dplyr::filter(
      N>1,
      !is.na(dateNum)
    ) %>%
    dplyr::arrange(dateNum,.by_group=T) %>%
    dplyr::mutate(
      window_start = dplyr::lead(dateNum,
                                 default = dplyr::last(dateNum)),
      window_cmax = cummax(window_end),
      indx = paste0(
        dplyr::cur_group_id(),
        ".",
        N,
        ".",
        c(0,cumsum(window_start > window_cmax))[-dplyr::n()]
      )
    ) %>%
    dplyr::group_by(indx,.add=TRUE) %>%
    dplyr::mutate(
      {{min_varname}} := min(as.Date(dateNum, origin="1970-01-01")),
      {{max_varname}} := max(as.Date(window_cmax, origin="1970-01-01"))
    )

  ## bring back in the removed records
  ## cleanup variable names and remove the temp columns created
  .data <-
    dplyr::bind_rows(
      .data,
      chunkOut
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(dateNum,window_start,window_end,window_cmax,N))

  return(.data)

}
