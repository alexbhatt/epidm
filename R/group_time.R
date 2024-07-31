#'
#' @title Grouping of intervals or events in time together
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'
#' Group across multiple observations of
#'  overlapping time intervals, with defined start and end dates,
#'  or events within a static/fixed or rolling window of time.
#'  These are commonly used with inpatient HES/SUS data to group spells with
#'  defined start and end dates,  or to group positive specimen tests,
#'  based on specimen dates together into infection episodes.
#'
#' @import data.table
#'
#' @param x data frame, this will be converted to a data.table
#' @param group_vars in a vector, the all columns used to group records, quoted
#' @param date_start column containing the start dates for the grouping,
#'   provided quoted
#'
#' @param date_end column containing the end dates for the *interval*, quoted
#'
#' @param window an integer representing a time window in days which will be
#'   applied to the start date for grouping *events*
#' @param window_type character, to determine if a 'rolling' or 'static'
#'   grouping method should be used when grouping *events*
#'
#' @param indx_varname a character string to set variable name for the
#'   index column which provides a grouping key; default is indx
#' @param min_varname a character string to set variable name for the
#'   time period minimum
#' @param max_varname a character string set variable name for the time
#'   period maximum
#' @param .forceCopy default FALSE; TRUE will force data.table to take a copy
#'   instead of editing the data without reference
#'
#' @return the original data.frame as a data.table
#'   with the following new fields:
#' \describe{
#'   \item{`indx`; renamed using `indx_varname`}{an id field for the new
#'     aggregated events/intervals; note that where the `date_start` is NA, an
#'     `indx` value will also be NA}
#'   \item{`min_date`; renamed using `min_varname`}{the start date for the
#'     aggregated events/intervals}
#'   \item{`max_date`; renamed using `max_varname`}{the end date for the
#'     aggregated events/intervals}
#'   }
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
#'            window_type = 'static',
#'            indx_varname = 'static_indx',
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
#'            indx_varname = 'spell_id',
#'            min_varname = 'spell_min_date',
#'            max_varname = 'spell_max_date')[]
#' @export


group_time <- function(x,
                       date_start,
                       date_end,
                       window,
                       window_type = c('rolling','static'),
                       group_vars,
                       indx_varname = 'indx',
                       min_varname = 'date_min',
                       max_varname = 'date_max',
                       .forceCopy = FALSE
){

  ## Needed to prevent RCMD Check fails
  ## recommended by data.table
  ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  # indx <-
  #   tmp.dateNum <-
  #   max_date <- min_date <-
  #   tmp.episode <- tmp.windowEnd <- tmp.windowStart <- tmp.windowCmax <-
  #   NULL

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  # setup NSE
  # subtitute() not needed on other vars as quoted so use get()
  group_vars <- substitute(group_vars)

  ## checks
  if(missing(x)){
    stop("x must be supplied as a data.frame or data.table")
  }
  if(missing(date_start)){
    stop("date_start must be supplied as a quoted column name from x")
  }

  ## seperate out the two halfs if there are missing events/intervals
  ## records in y will not be assigned an indx since there is no event
  y <- x[is.na(get(date_start)), ]
  x <- x[!is.na(get(date_start)), ]


  ## static + window methods only ##############################################
  ## bring the static window function in so its all a one stop shop for ease
  if(missing(date_end)){

    if(missing(window_type)){
      stop("window_type must be specified as either rolling or static")
    }

    if(missing(window)){
      stop("window parameter must be supplied as numeric value, the unit is days")
    }

    ## this is for static grouping of single date windows
    if(window_type=='static'){

      ## set a fixed ordering
      data.table::setorderv(x, c(date_start))

      ## move through static episodes in a loop.
      ## next steps: update with data.table::set()
      x[,tmp.episode:=0]

      ## setup loop episode counter
      i <- 1L

      while (min(x[['tmp.episode']]) == 0) {

        x[tmp.episode==0,
          c('tmp.diff',
            'tmp.diff.start'
          ) := .(
            as.integer(difftime(get(date_start),
                                data.table::shift(date_start,n=1,type="lag"),
                                units="days")),
            as.integer(difftime(get(date_start),
                                get(date_start)[1],
                                units="days"))
          ),
          by = group_vars
        ][tmp.episode==0,
          tmp.diff := data.table::fifelse(is.na(tmp.diff),0L,tmp.diff),
          by = group_vars
        ][tmp.episode == 0,
          tmp.episode := data.table::fifelse(tmp.diff <= window &
                                               tmp.diff.start <= window,
                                             i,
                                             tmp.episode),
          by = group_vars
        ]

        i <- i + 1L
      } # static loop

      x[,
        indx := paste0(
          .GRP,
          ".",
          .N,
          ".",
          tmp.episode),
        keyby = group_vars]

      ## only produce these if the arguments are defined
      if(!missing(min_varname) & !missing(max_varname))
        x[,
          c('min_date',
            'max_date'
          ) := .(
            min(get(date_start)),
            max(get(date_start))
          ),
          by = indx
        ]


    } # static method

    if(window_type == 'rolling') {
      x[,tmp.windowEnd := as.numeric(get(date_start)) + window]
    }
  }

  ## everything else ###########################################################
  if(any(window_type == "rolling" | !missing(date_end))) {

    ## change the dates into numeric
    x[,tmp.dateNum := as.numeric(get(date_start))]

    if(!missing(date_end)){
      x[,tmp.windowEnd := as.numeric(get(date_end))]
    }

    ## set sort order
    data.table::setorder(x,tmp.dateNum)

    ## look at the next start date
    x[,
      tmp.windowStart := data.table::shift(
        tmp.dateNum,
        1,
        type="lead",
        fill = tmp.dateNum[.N]
      ),
      by = group_vars
    ]

    ## compare the end end date within the groups
    x[,
      tmp.windowCmax := cummax(tmp.windowEnd),
      by = group_vars
    ]

    ## correct for missing values
    x[,
      tmp.windowCmax := data.table::fifelse(
        is.na(tmp.windowCmax) & !is.na(tmp.windowEnd),
        tmp.windowEnd,
        tmp.windowCmax
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
            cumsum(tmp.windowStart > tmp.windowCmax)
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
      ## there are confirmed end dates, so use them
      x[,
        max_date := max(as.Date(tmp.windowCmax, origin="1970-01-01")),
        by = indx
      ]
    } else {
      ## these are for windows, so we cant always assume the window
      ## time was still relevant, so we use the last known time in the series
      x[,
        max_date := min(as.Date(tmp.windowCmax, origin="1970-01-01")),
        by = indx
      ]
    }

  } ## rolling windows


  ## variable cleanup ##########################################################
  ## rename if arguments are provided
  if(min_varname!="min_date" & !missing(min_varname)){
    data.table::setnames(x,'min_date',min_varname)
  }
  if(max_varname!="max_date" & !missing(max_varname)){
    data.table::setnames(x,'max_date',max_varname)
  }

  ## allow rename of indx column for multiple runs
  if(indx_varname!='indx' & !missing(indx_varname)){
    data.table::setnames(x,'indx',indx_varname)
  }

  ## cleanup and remove temp columns
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  ## rejoin on the missing section
  x <- data.table::rbindlist(
    list(x,y),
    fill = TRUE
  )

  return(x)

}
