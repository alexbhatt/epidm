
#' Continuous Inpatient Spells
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#'
#' A continuous inpatient (CIP) spell is a continuous period of care within the NHS, regardless of any transfers which may take place. It can therefore be made up of one or more provider spells. A CIP spell starts when a decision has been made to admit the patient, and a consultant has taken responsibility for their care. The spell ends when the patient dies or is discharged from hospital
#'
#' @seealso http://content.digital.nhs.uk/media/11859/Provider-Spells-Methodology/pdf/Spells_Methodology.pdf
#'
#' @import data.table
#' @importFrom data.table .I .N .GRP ':='
#'
#'
#' @param x a data frame; will be converted to a data.table
#' @param group_vars a vector containing any variables to be used for
#'   record grouping, minimum is a patient identifier
#' @param spell_start_date Inpatient provider spell or episode admission date
#' @param admission_method CDS admission method code
#' @param admission_source CDS admission source code
#' @param spell_end_date Inpatient provider spell or episode discharge  date
#' @param discharge_destination CDS discharge destination code
#' @param patient_classification CDS patient classification code
#' @param .forceCopy default FALSE; TRUE will force data.table to take a copy
#'   instead of editing the data without reference
#'
#' @return a data.table with cleaned start and end dates, a cip_indx
#'   and new cip window dates
#' @export
#'
#' @examples
#'
#' cip_test <- data.frame(
#'   id = c('465','465','465','465','8418','8418','8418',
#'          '8418','8418','8418','8418','8418','26443',
#'          '26443','26443','33299','33299','33299','33299',
#'          '33299','33299','33299','33299','33299','33299',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','52635','52635','52635',
#'          '52635','52635','52635','78915','78915','78915'),
#'   provider = c('X1T','X1T','X1T','X1T','KHA','KHA','KHA',
#'                'KHA','KHA','KHA','KHA','KHA','BX2','BX2',
#'                'BX2','PXH','PXH','PXH','PXH','PXH','PXH',
#'                'PXH','PXH','PXH','PXH','9HA','9HA','9HA',
#'                '9HA','9HA','9HA','9HA','9HA','9HA','9HA',
#'                '9HA','9HA','9HA','9HA','9HA','9HA','YYT',
#'                'YYT','YYT','YYT','YYT','YYT','YYT','YYT',
#'                'YYT','YYT','YYT','ABX','ABX','ABX'),
#'   spell_start = as.Date(c(
#'     '2020-03-07','2020-03-07','2020-03-25','2020-04-03','2020-01-25',
#'     '2020-01-26','2020-07-14','2020-08-02','2020-08-12','2020-08-19',
#'     '2020-08-19','2020-11-19','2019-11-12','2020-04-17','2020-04-23',
#'     '2020-07-03','2020-01-17','2020-02-07','2020-03-20','2020-04-27',
#'     '2020-06-21','2020-07-02','2020-10-17','2020-11-27','2021-01-02',
#'     '2019-12-31','2020-01-02','2020-01-14','2020-01-16','2020-02-07',
#'     '2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25',
#'     '2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-03-13',
#'     '2020-03-14','2020-02-04','2020-02-07','2020-02-11','2020-02-14',
#'     '2020-02-18','2020-02-21','2020-02-25','2020-02-28','2020-03-09',
#'     '2020-03-11','2020-03-12','2020-04-16','2020-04-24','2020-05-13')),
#'   spell_end = as.Date(c(
#'     '2020-03-07','2020-03-25','2020-04-02','2020-04-27','2020-01-25',
#'     '2020-01-27','2020-07-17','2020-08-07','2020-08-14','2020-08-19',
#'     '2020-08-22','2020-12-16','2020-04-17','2020-04-23','2020-05-20',
#'     '2020-07-24','2020-01-28','2020-02-07','2020-03-23','2020-04-29',
#'     '2020-06-21','2020-07-03','2020-11-27','2021-01-02','2021-01-10',
#'     '2019-12-31','2020-01-11','2020-01-14','2020-02-04','2020-02-07',
#'     '2020-02-11','2020-02-14','2020-02-18','2020-02-21','2020-02-25',
#'     '2020-02-28','2020-03-09','2020-03-11','2020-03-12','2020-03-13',
#'     '2020-03-30','2020-02-07','2020-02-11','2020-02-14','2020-02-18',
#'     '2020-02-21','2020-02-25','2020-02-28','2020-03-09','2020-03-11',
#'     '2020-03-12','2020-03-13','2020-04-24','2020-05-13','2020-06-11')),
#'   adm_meth = c('21','81','21','81','21','21','11','21','21','21','21',
#'                '21','21','81','21','81','21','21','21','21','21','21',
#'                '21','13','13','12','22','12','2D','13','13','13','13',
#'                '13','13','13','13','13','13','13','21','81','81','81',
#'                '81','81','13','81','81','13','13','13','21','11','81'),
#'   adm_src = c('19','51','19','51','19','51','19','51','19','19','19',
#'               '51','19','51','19','51','19','19','19','19','19','19',
#'               '19','51','19','19','19','19','19','19','19','19','19',
#'               '19','19','19','51','51','51','51','19','51','51','51',
#'               '51','51','51','51','51','51','51','51','19','51','51'),
#'   dis_meth = c('1','1','1','1','1','1','1','1','1','1','1','4','1','1',
#'                '4','1','1','1','1','1','1','1','8','1','4','1','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','1','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','1','2'),
#'   dis_dest = c('51','51','51','54','51','19','19','19','19','51','19',
#'                '79','51','51','79','65','19','19','19','19','19','29',
#'                '98','51','79','19','19','19','51','19','19','19','51',
#'                '51','51','19','19','51','51','19','51','51','51','51',
#'                '51','51','51','51','51','51','51','51','29','54','19'),
#'   patclass = c('1','1','1','1','1','1','1','1','1','1','1','1','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','2','1','2',
#'                '1','2','2','2','2','2','2','2','2','2','2','2','1','1',
#'                '1','1','1','1','1','1','1','1','1','1','1','1','1')
#' )
#'
#' cip_spells(x=cip_test,
#'   group_vars = c('id','provider'),
#'   patient_classification = 'patclass',
#'   spell_start_date = 'spell_start',
#'   admission_method = 'adm_meth',
#'   admission_source = 'adm_src',
#'   spell_end_date = 'spell_end',
#'   discharge_destination = 'dis_dest'
#' )

cip_spells <- function(x,
                       group_vars,
                       spell_start_date,
                       admission_method,
                       admission_source,
                       spell_end_date,
                       discharge_destination,
                       patient_classification,
                       .forceCopy = FALSE) {

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  ## Needed to prevent RCMD Check fails
  ## recommended by data.table
  ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  # cip_indx <-
  #   tmp.spellN <-
  #   tmp.cip2daydiff <- tmp.cipTransfer <- tmp.cipExclude <-
  #   tmp.dateNumStart <- tmp.dateNumEnd <- tmp.regular_attender <-
  #   tmp.windowNext <- tmp.windowCmax <-
  #   NULL

  ## just arrange the data
  data.table::setorderv(x,c(eval(group_vars),spell_start_date))

  ## counter columns to make life easier
  x[,
    tmp.spellN := seq_len(.N),
    by = group_vars
  ]

  ## CIP CRITERIA ##############################################################
  # difference between admission and discharge is <2 days
  x[,
    tmp.cip2daydiff := as.numeric(
      difftime(
        data.table::shift(get(spell_start_date),n=1,type = "lead"),
        get(spell_end_date),
        units="days")
    ) %in% c(0,1,2),
    by = group_vars
  ]

  # a transfer has taken place (based on these criteria)
  # used the simple criteria, as we dont need to determine transfer type (1,2,3)
  x[,
    tmp.cipTransfer :=
      get(discharge_destination) %in%
      c("49", "50", "51", "52", "53", "84") |
      data.table::shift(get(admission_source),n=1,type="lead") %in%
      c("49", "50", "51", "52", "53", "87") |
      data.table::shift(get(admission_method),n=1,type="lead") %in%
      c("2B", "81"),
    by = group_vars
  ]


  # exclusion criteria
  x[,
    tmp.cipExclude :=
      get(discharge_destination) %in% c("19") &
      data.table::shift(get(admission_source),
                        n=1,
                        type="lead") %in% c("51") &
      data.table::shift(get(admission_method),
                        n=1,
                        type="lead") %in% c("21"),
    by = group_vars
  ]

  ## call the other epidm function to clean the dates.
  x <- epidm::proxy_episode_dates(x = x,
                                  group_vars = group_vars,
                                  spell_start_date = spell_start_date,
                                  spell_end_date = spell_end_date,
                                  discharge_destination = discharge_destination,
                                  .dropTmp = FALSE)

  ## setup requirement variables
  x[,tmp.dateNumStart := as.numeric(get(spell_start_date))]
  x[,tmp.dateNumEnd := as.numeric(get(spell_end_date))]
  x[,tmp.regular_attender := as.character(patient_classification) %in% c("3","4")]


  # group records using tmp.cip_valid
  x[,
    tmp.cip_valid :=
      tmp.cip2daydiff &
      tmp.cipTransfer &
      !tmp.cipExclude &
      !tmp.regular_attender
  ]

  x[,
    tmp.cip_valid := data.table::fcase(
      tmp.cip_valid==TRUE, TRUE,
      data.table::shift(tmp.cip_valid,n=1,type="lag"), TRUE,
      is.na(tmp.cip_valid), FALSE,
      default = FALSE
    ),
    by = group_vars
  ]

  ## GROUP UP THE TIME CHUNKS ##################################################
  ## +2 to tmp.windowCmax to allow for up to 2-day window in line with tmp.cip2daydiff

  x[,
    tmp.windowNext := data.table::fifelse(
      tmp.cip_valid,
      data.table::shift(tmp.dateNumStart,
                        n=1,type="lead",
                        fill = data.table::last(tmp.dateNumStart)),
      (tmp.spellN+1)^3
    ),
    by = group_vars
  ]
  x[,
    tmp.windowCmax := data.table::fifelse(
      tmp.cip_valid,
      cummax(tmp.dateNumEnd),
      tmp.spellN),
    by = group_vars
  ]

  x[,
    cip_indx := paste0(
      .GRP,
      ".",
      .N,
      ".",
      c(0,cumsum(tmp.windowNext > tmp.windowCmax))[-.N]
    ),
    by = group_vars
  ]


  x[,
    c('cip_spell_start',
      'cip_spell_end')
    :=
      .(
        min(get(spell_start_date)),
        max(get(spell_end_date))
      ),
    by = cip_indx]


  ## cleanup and remove temp columns
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)
}
