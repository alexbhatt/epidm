#' @title HES/SUS Episode Date Cleaning
#'
#' Correcting for missing end dates on HES/SUS episodes
#'
#' @import data.table
#' @importFrom data.table .I .N .GRP ':='
#'
#' @param x a data frame; will be converted to a data.table
#' @param patient_group_vars a vector containing any variables to be used for record grouping, minimum is a patient identifier
#' @param spell_start_date Inpatient provider spell or episode admission date
#' @param spell_end_date Inpatient provider spell or episode discharge  date
#' @param discharge_destination CDS discharge destination code
#' @param drop.tmp a logical to drop all tmp values used, default to TRUE
#'
#' @return a data.table with cleaned start and end dates, and an indicator proxy_missing where the value has changed
#' @export
#'
#' @examples
#'
#' proxy_test <- data.frame(
#'   id = c(
#'     rep(3051, 4),
#'     rep(7835,3),
#'     rep(9891,3),
#'     rep(1236,3)
#'   ),
#'   provider = c(
#'     rep("QKJ", 4),
#'     rep("JSD",3),
#'     rep("YJG",3),
#'     rep("LJG",3)
#'   ),
#'   spell_start = as.Date(c(
#'     "2020-07-03", "2020-07-14", "2020-07-23", "2020-08-05",
#'     "2020-11-01", "2020-11-13", "2020-12-01",
#'     "2020-03-28", "2020-04-06", "2020-04-09",
#'     "2020-10-06", "2020-11-05", "2020-12-25"
#'   )),
#'   spell_end = as.Date(c(
#'     "2020-07-11", "2020-07-22", "2020-07-30", "2020-07-30",
#'     "2020-11-11", NA, "2020-12-03",
#'     "2020-03-28", NA, "2020-04-09",
#'     "2020-10-06", "2020-11-05", NA
#'   )),
#'   disdest = c(
#'     19, 19, 51, 19,
#'     19, 19, 19,
#'     51, 98, 19,
#'     19, 19, 98
#'   )
#' )
#'
#'
#' proxy_episode_dates(
#'   x=proxy_test,
#'   patient_group_vars = c('id','provider'),
#'   spell_start_date = 'spell_start',
#'   spell_end_date = 'spell_end',
#'   discharge_destination = 'disdest'
#' )[]

proxy_episode_dates <- function(x,
                                patient_group_vars,
                                spell_start_date,
                                spell_end_date,
                                discharge_destination,
                                drop.tmp = TRUE) {


  ## convert object if its not already
  if(data.table::is.data.table(x)==FALSE) {
    x <- data.table::as.data.table(x)
  }


  ## just arrange the data
  data.table::setorderv(x,c(eval(patient_group_vars),spell_start_date))

  ## counter columns to make life easier
  x[,
    c('tmp.spell.N',
      'tmp.spell.n',
      'tmp.spell_start',
      'tmp.spell_end'
      ) := .(
        .N,
        seq(1:.N),
        get(spell_start_date),
        get(spell_end_date)
        ),
    by = patient_group_vars
  ]

    ## DATE CLEANUP ##############################################################
  #   check to see if dates are missing, and decide how to replace
  #   0 = unchanged
  #   1 = final episode, no date available, use today
  #   2 = patient still in hospital, so connect to the next episode
  #   3 = patient discharged, so dont connect to the next episode
  #   4 = if the dates overlap, use the start date of the next

  x[,
    proxy_missing := data.table::fcase(
      is.na(get(spell_end_date)) & tmp.spell.n == tmp.spell.N, 1,
      is.na(get(spell_end_date)) & tmp.spell.n < tmp.spell.N &
        get(discharge_destination) %in% c("51","98"), 2,
      is.na(get(spell_end_date)) & tmp.spell.n < tmp.spell.N &
        !get(discharge_destination) %in% c("51","98"), 3,
      !is.na(get(spell_end_date)) &
        get(spell_end_date) < get(spell_start_date), 4,
      default = 0
    )
  ]

  #   replace the dates based on the proxy_missing flag
  #   use the x[,c(var)=.(val)] syntax to allow overwriting of existing col
  #   using a functional argument for the varname
  x[,
    c(spell_end_date,
      spell_start_date
      ) := .(
      data.table::fcase(
        proxy_missing==0, get(spell_end_date),
        proxy_missing==1, Sys.Date(),
        proxy_missing==2, data.table::shift(get(spell_start_date),n=1,type="lead"),
        proxy_missing==3, data.table::shift(get(spell_start_date),n=1,type="lead")-1,
        proxy_missing==4, tmp.spell_start),
      data.table::fifelse(
        proxy_missing==4,
        tmp.spell_end,
        tmp.spell_start
        )
    ),
    by = patient_group_vars
  ]

  ## cleanup and remove temp columns
  if(drop.tmp==TRUE){
    tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
    x[,
      (tmpcols) := NULL
    ]
  }

  return(x)
}



