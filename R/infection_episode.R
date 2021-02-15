#'
#' @title Patient Episode Groupings
#'
#' @description
#' Groups single event instances eg. test results in time based on a
#' static or rolling window in days. Events can be grouped according to a
#' any number of columns, to group monomicrobial or polymicrobial episodes
#' eg. group infection episodes within 14-days
#'
#' @return A new dataframe containing the original data, and columns specifying monomicrobial or polymicrobial patient episodes.
#'
#' @seealso epidm::group_time
#'
#' @import data.table
#' @importFrom data.table .I .N .GRP ':='
#'
#' @param data a data.frame or tibble containing the line list
#' @param group_vars a vector containing any columns used to identify the patient, infection, and site
#' @param event_date the column as a date variable containing the specimen date
#' @param event_window an integer to specify the length of the event_window in time
#' @param window_type a choice of static or rolling for how the event_window is calculated
#' @param indx_name a character string to rename an indx column
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
#' infection_episode(
#'   x = episode_test,
#'   group_vars = c("pat_id", "spec_type", "species"),
#'   event_date = "sp_date",
#'   episode_window = 14
#' )[]
#'
#' @export
#'

infection_episode <- function(x,
                              group_vars,
                              event_date,
                              event_window = c(0:Inf),
                              indx_name = 'indx',
                              window_type = c('static','rolling')
) {

  ## convert object if its not already
  if(data.table::is.data.table(x)==FALSE) {
    x <- data.table::as.data.table(x)
  }

  # check dates
  if(!class(x[[event_date]]) %in% c("Date","POSIXct","POSIXt")){
    stop("column given to event_date must by in Date format.")
  }


    # setup NSE
    # subtitute() not needed on other vars as quoted so use get()
    # this is just to make our life a little easier
    group_vars <- substitute(group_vars)

    ## set a fixed ordering
    data.table::setorderv(x, c(event_date))

  if(window_type == 'static'){

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
          as.integer(difftime(get(event_date),
                              data.table::shift(event_date,n=1,type="lag"),
                              units="days")),
          as.integer(difftime(get(event_date),
                              get(event_date)[1],
                              units="days"))
        ),
        by = group_vars
      ][tmp.episode==0,
        tmp.diff := data.table::fifelse(is.na(tmp.diff),0L,tmp.diff),
        by = group_vars
      ][tmp.episode == 0,
        tmp.episode := data.table::fifelse(tmp.diff <= event_window &
                                             tmp.diff.start <= event_window,
                                           i,
                                           tmp.episode),
        by = group_vars
      ]

      i <- i + 1L
    }

    x[,
      indx := paste0(
        .GRP,
        ".",
        .N,
        ".",
        tmp.episode),
      keyby = group_vars]

  }

  if(window_type == 'rolling') {

    ## change the dates into numeric
    x[,tmp.dateNum := as.numeric(get(event_date))]
    x[,tmp.window_end := tmp.dateNum + event_window]

    ## look at the next start date
    x[,
      tmp.window_start := data.table::shift(
        tmp.dateNum,
        1,
        type="lead",
        fill = tmp.dateNum[.N]
      ),
      keyby = group_vars
    ]

    ## compare the end end date within the groups
    x[,
      tmp.window_cmax := cummax(tmp.window_end),
      keyby = group_vars
    ]

    ## correct for missing values
    x[,
      tmp.window_cmax := data.table::fifelse(
        is.na(tmp.window_cmax) & !is.na(tmp.window_end),
        tmp.window_end,
        tmp.window_cmax
      ),
      keyby = group_vars
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
      keyby = group_vars
    ]

  }



  ## allow rename of indx column for multiple runs
  if(indx_name!='indx' & !missing(indx_name)){
    data.table::setnames(x,'tmp.indx',indx_name)
  }

  ## cleanup and remove temporary vars
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)
}
