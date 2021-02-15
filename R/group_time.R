#'
#' @title Time grouping
#'
#' Group overlapping time periods, with start and end dates, or start dates with a rolling window
#'
#' @return a data frame with 4 new variables: indx, a grouping flag; and new start and end dates
#'
#' @import data.table
#' @importFrom data.table .I .N .GRP ':='
#'
#' @param x data frame, this can be piped in
#' @param date_start the start dates for the grouping, provided quoted
#' @param date_end the end dates for the grouping, provided quoted
#' @param window if there is no end date, a time window which will be applied to the start date
#' @param group_vars in a vector, the all vars used to group records, quoted
#' @param min_varname set variable name for the time period minimum
#' @param max_varname set variable name for the time period maximum
#'
#' @examples
#' episode_test <- structure(
#'   list(
#'     pat_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L,
#'                1L, 1L, 1L, 1L, 2L, 2L, 2L),
#'     species = c(rep("E. coli",7),rep("K. pneumonia",7)),
#'     spec_type = c(rep("Blood",7),rep("Blood",4),rep("Sputum",3)),
#'     sp_date = structure(c(18262, 18263, 18281, 18282, 18262, 18263, 18281,
#'                           18265, 18270, 18281, 18283, 18259, 18260, 18281),
#'                         class = "Date")
#'   ),
#'   row.names = c(NA, -14L), class = "data.frame")
#'
#' group_time(x=episode_test,
#'            date_start='sp_date',
#'            window=14,
#'            group_vars=c('pat_id','species','spec_type'))[]
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
#' group_time(x = spell_test,
#'            date_start = 'spell_start',
#'            date_end = 'spell_end',
#'            group_vars = c('id','provider'),
#'            min_varname = 'spell_min_date',
#'            max_varname = 'spell_max_date')[]
#'
#' @export


group_time <- function(x,
                        date_start,
                        date_end,
                        window,
                        group_vars,
                        min_varname="date_min",
                        max_varname="date_max"
){

  ## convert object if its not already
  if(data.table::is.data.table(x)==FALSE) {
    x <- data.table::as.data.table(x)
  }

  # setup NSE
  # subtitute() not needed on other vars as quoted so use get()
  group_vars <- substitute(group_vars)

  ## change the dates into numeric
  x[,tmp.dateNum := as.numeric(get(date_start))]

  ## select based on end date or window methods
  if(missing(date_end) & missing(window)){
    stop("date_end or window argument required")
  }

  if(!missing(date_end)){
    x[,tmp.window_end := as.numeric(get(date_end))]
  }

  if(!missing(window)){
    x[,tmp.window_end := tmp.dateNum + window]
  }

  ## set sort order
  data.table::setorder(x,tmp.dateNum)

  ## look at the next start date
  x[,
    tmp.window_start := data.table::shift(
      tmp.dateNum,
      1,
      type="lead",
      fill = tmp.dateNum[.N]
    ),
    by = group_vars
  ]

  ## compare the end end date within the groups
  x[,
    tmp.window_cmax := cummax(tmp.window_end),
    by = group_vars
    ]

  ## correct for missing values
  x[,
    tmp.window_cmax := data.table::fifelse(
      is.na(tmp.window_cmax) & !is.na(tmp.window_end),
      tmp.window_end,
      tmp.window_cmax
    ),
    by = group_vars
    ]

  ## create an index to group records sequentially and overlapping in time
  x[,
    indx := paste0(
      .GRP,
      ".",
      .N,
      ".",
      c(0,
        data.table::fifelse(
          is.na(tmp.dateNum),
          .I,
          cumsum(tmp.window_start > tmp.window_cmax)
          )[-.N]
        )
      ),
    by = group_vars
  ]

  ## create new columns back in date format
  x[,
    min_date := min(as.Date(tmp.dateNum, origin="1970-01-01")),
    by = indx
    ]

  if(!missing(date_end)){
    x[,
      max_date := max(as.Date(tmp.window_cmax, origin="1970-01-01")),
      by = indx
    ]
  } else {
    x[,
      max_date := min(as.Date(tmp.window_cmax, origin="1970-01-01")),
      by = indx
    ]
  }

  ## rename if arguments are provided
  if(min_varname!="min_date" & !missing(min_varname)){
    data.table::setnames(x,'min_date',min_varname)
  }
  if(max_varname!="max_date" & !missing(max_varname)){
    data.table::setnames(x,'max_date',max_varname)
  }

  ## cleanup and remove temp columns
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)

}
