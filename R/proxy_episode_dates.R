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
#' @param admission_method CDS admission method code
#' @param admission_source CDS admission source code
#' @param spell_end_date Inpatient provider spell or episode discharge  date
#' @param discharge_destination CDS discharge destination code
#' @param patient_classification CDS patient classification code
#'
#' @return a data.table with cleaned start and end dates, a cip_indx and new cip window dates
#' @export
#'
#' @examples
#'
#' cip_test <- data.frame(
#'   id = c('465','465','465','465','8418','8418','8418','8418','8418','8418','8418','8418','26443','26443','26443','33299','33299','33299','33299','33299','33299','33299','33299','33299','33299','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','52635','78915','78915','78915'),
#'   provider = c('X1T','X1T','X1T','X1T','KHA','KHA','KHA','KHA','KHA','KHA','KHA','KHA','BX2','BX2','BX2','PXH','PXH','PXH','PXH','PXH','PXH','PXH','PXH','PXH','PXH','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','9HA','YYT','YYT','YYT','YYT','YYT','YYT','YYT','YYT','YYT','YYT','YYT','ABX','ABX','ABX'),
#'   spell_start = as.Date(c('2020-03-07','2020-03-07','2020-03-25','2020-04-03','2020-01-25','2020-01-26','2020-07-14','2020-08-02','2020-08-12','2020-08-19','2020-08-19','2020-11-19','2019-11-12','2020-04-17','2020-04-23','2020-07-03','2020-01-17','2020-02-07','2020-03-20','2020-04-27','2020-06-21','2020-07-02','2020-10-17','2020-11-27','2021-01-02','2019-12-31','2020-01-02','2020-01-14','2020-01-16','2020-02-07','2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25','2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-03-13','2020-03-14','2020-02-04','2020-02-07','2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25','2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-04-16','2020-04-24','2020-05-13')),
#'   spell_end = as.Date(c('2020-03-07','2020-03-25','2020-04-02','2020-04-27','2020-01-25','2020-01-27','2020-07-17','2020-08-07','2020-08-14','2020-08-19','2020-08-22','2020-12-16','2020-04-17','2020-04-23','2020-05-20','2020-07-24','2020-01-28','2020-02-07','2020-03-23','2020-04-29','2020-06-21','2020-07-03','2020-11-27','2021-01-02','2021-01-10','2019-12-31','2020-01-11','2020-01-14','2020-02-04','2020-02-07','2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25','2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-03-13','2020-03-30','2020-02-07','2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25','2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-03-13','2020-04-24','2020-05-13','2020-06-11')),
#'   dis_dest = c('51','51','51','54','51','19','19','19','19','51','19','79','51','51','79','65','19','19','19','19','19','29','98','51','79','19','19','19','51','19','19','19','51','51','51','19','19','51','51','19','51','51','51','51','51','51','51','51','51','51','51','51','29','54','19'),
#' )
#'
#' proxy_episode_dates(x=cip_test,
#'   patient_group_vars = c('id','provider'),
#'   spell_start_date = 'spell_start',
#'   spell_end_date = 'spell_end',
#'   discharge_destination = 'dis_dest'
#' )

proxy_episode_dates <- function(x,
                       patient_group_vars,
                       spell_start_date,
                       spell_end_date,
                       discharge_destination) {


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
        get(discharge_destination)=="98", 2,
      is.na(get(spell_end_date)) & tmp.spell.n < tmp.spell.N &
        get(discharge_destination)!="98", 3,
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
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)
}



