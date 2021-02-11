#'
#' @title Patient Episode Groupings
#'
#' assigns isolates of same species to an episode window
#' works on line listings only
#' designed for SGSS data
#' MONO EPISODES ARE PER ORGANISM PER PATIENT
#' Note that first specimen date is day zero.
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
#' @param spec_date the column as a date variable containing the specimen date
#' @param episode_window an integer to specify the length of the episode
#' @param episode_type a choice of static or rolling for how the window is calculated
#'
#' @examples
#' episode_test <- structure(
#'   list(
#'     pat_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L,
#'                1L, 1L, 1L, 1L, 2L, 2L, 2L),
#'     sp_date = structure(c(18262, 18263, 18281, 18282, 18262, 18263, 18281,
#'                           18265, 18270, 18281, 18283, 18259, 18260, 18281),
#'                         class = "Date"),
#'     species = c(rep("E. coli",7),rep("K. pneumonia",7)),
#'     spec_type = c(rep("Blood",7),rep("Blood",4),rep("Sputum",3))
#'   ),
#'   row.names = c(NA, -14L), class = "data.frame")
#'
#' infection_episode(
#'   x = episode_test,
#'   group_vars = c("pat_id", "spec_type", "species"),
#'   specimen_date = "sp_date",
#'   episode_window = 14
#' )[]
#'
#' @export
#'

infection_episode <- function(x,
                              group_vars,
                              specimen_date,
                              episode_window = c(0:Inf)
                              ) {

  ## convert object if its not already
  if(data.table::is.data.table(x)==FALSE) {
    x <- data.table::as.data.table(x)
  }

  # setup NSE
  # subtitute() not needed on other vars as quoted so use get()
  # this is just to make our life a little easier
  group_vars <- substitute(group_vars)
  data.table::setorder(x, specimen_date)


  x[,
    c('tmp.ep.n',
      'tmp.ep.max',
    ) := .(
      seq_len(.N),
      .N,
    ),
    by = group_vars
  ]

  x[,
    c('tmp.diff',
      'tmp.diff.start',
      'tmp.dateNum'
    ) := .(
      as.integer(difftime(specimen_date,
                          data.table::shift(specimen_date,n=1,type="lag"),
                          units="days")),
      as.integer(difftime(specimen_date,
                          specimen_date[1],
                          units="days")),
      as.integer(specimen_date)
    ),
    by = group_vars
  ]
  x[,
    tmp.diff := data.table::fifelse(is.na(tmp.diff),0L,tmp.diff)
  ]

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
    tmp.window_end := tmp.dateNum + window,
    keyby = group_vars
  ]
  x[,
    tmp.window_cmax := cummax(tmp.window_end),
    keyby = group_vars
  ]



  ## create an index to group records sequentially and overlapping in time
  x[,
    episode := paste0(
      .GRP,
      ".",
      .N,
      ".",
      c(0,cumsum(tmp.window_start > tmp.window_cmax))[-.N]
    ),
    keyby = group_vars
  ][]

  ## cleanup and remove temporary vars
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)
}
