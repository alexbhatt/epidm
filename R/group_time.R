#'
#' @title Time grouping
#'
#' Group overlapping time periods, with start and end dates, or start dates with a rolling window
#'
#' @return a data frame with 4 new variables: indx, a grouping flag; and new start and end dates
#'
#' @import data.table
#'
#' @param x data frame, this can be piped in
#' @param date_start the start dates for the grouping
#' @param date_end the end dates for the grouping
#' @param window if there is no end date, a time window which will be applied to the start date
#' @param group_vars in a vector, the all vars used to group records
#' @param min_varname set variable name for the time period minimum
#' @param max_varname set variable name for the time period maximum
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
#' group_time(x=episode_test,date_start=specimen_date,window=14,group_vars=c(id,org))
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
#' group_time(x=spell_test,
#'            date_start=spell_start,
#'            date_end=spell_end,
#'            group_vars=c(id,provider),
#'            min_varname="spell_min_date",
#'            max_varname="spell_max_date")
#'
#' @export


group_time2 <- function(x,
                        date_start,
                        date_end,
                        window,
                        group_vars,
                        min_varname="date_min",
                        max_varname="date_max"
){

  eds.grp <- eval(deparse(substitute(group_vars)))

  x <- data.table::data.table(x)

  x[,
    `:=`(
      dateNum = as.numeric(deparse(substitute(date_start))),
      window_end = as.numeric(deparse(substitute(date_end)))
    )
  ]

  data.table::setorder(x,dateNum)

  x[,
    `:=`(
      window_start = data.table::shift(dateNum, 1,
                                       type="lead",
                                       fill = dateNum[data.table::.N]),
      window_cmax = cummax(window_end)
    ),
    keyby=eds.grp
  ][,
    indx := paste0(
      data.table::.GRP,
      ".",
      data.table::.N,
      ".",
      c(0,cumsum(window_start > window_cmax))[-data.table::.N]),
    keyby=eds.grp
  ][,
    `:=`(
      min = min(as.Date(dateNum, origin="1970-01-01")),
      max = max(as.Date(window_cmax, origin="1970-01-01"))
    ),
    keyby=indx
  ][]

  return(x)

}
