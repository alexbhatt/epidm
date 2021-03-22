#'
#'  Respeciate unspecified samples
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Some samples within SGSS are submitted by laboratories as "GENUS SP" or
#' "GENUS UNNAMED". However, they may also have a fully identified sample taken
#' from the same site within a recent time period.  This function captures
#' species  from another sample within X-days of an unspeciated isolate.
#'
#' @import data.table
#' @importFrom stringr str_detect
#'
#'
#' @param x a data.frame or data.table object
#' @param group_vars the minimum grouping set of variables for like samples in a character vector; suggest c('patient_id','specimen_type','genus')
#' @param species a character containing the column with the organism species name
#' @param date a character containing the column with the specimen/sample date
#' @param window an integer representing the number of days for which you will allow a sample to be respeciated
#'
#' @return a data.table with a recharacterised specimen column
#' @export
#'
#' @examples
#' df <- data.frame(
#' ptid = c(round(runif(25,1,5))),
#' spec = sample(c("KLEBSIELLA SP",
#'                 "KLEBSIELLA UNNAMED",
#'                 "KLEBSIELLA PNEUMONIAE",
#'                 "KLEBEIELLA OXYTOCA"),
#'               25,replace = T),
#' type = "BLOOD",
#' date = sample(seq.Date(Sys.Date()-21,Sys.Date(),"day"),25,replace = T)
#' )
#'
#' respeciate_generic(x=df,
#'                    group_vars=c('ptid','type'),
#'                    species='spec',
#'                    date='date',
#'                    window = 14)[]

respeciate_generic <- function(x,
                               group_vars,
                               species,
                               date,
                               window=c(0:Inf)
                               ) {

  ## convert data.frame to data.table or take a copy
  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  ## Needed to prevent RCMD Check fails
  ## recommended by data.table
  ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  tmp.dayR <- tmp.spFlag <- tmp.respecType <- tmp.genus <- NULL

  ## NSE for group_vars
  group_vars <- substitute(group_vars)

  ## steup some helpful vars first
  x[,
    c(
      'tmp.genus',
      'tmp.spFlag'
      ) := .(
        gsub("([A-Za-z]+).*", "\\1", get(species)),
        data.table::fifelse(
          stringr::str_detect(get(species), " SP$|UNNAMED$|SPECIES$"),
          1,
          0)
      )
    ]


  ## set a static key and order for the table
  data.table::setorderv(x, c(eval(group_vars),date))


  ## loop counters
  i <- 1
  respecCount <- sum(x$tmp.spFlag,na.rm=T)

  while(respecCount!=0) {

    ## check for unspeciated samples X days of either side

    ## calculate days and if it needs to be amended
    x[,
      c(
        'tmp.dayR',
        'tmp.spFlag'
      ) := .(
        as.numeric(difftime(
          get(date),
          data.table::shift(get(date),type="lag"),
          units="days")
          ),
        data.table::fifelse(
          stringr::str_detect(get(species),
                              " SP$|UNNAMED$|SPECIES$"),
          1,0)

      ),
      by = group_vars
    ][,
      tmp.dayR := data.table::fifelse(is.na(tmp.dayR),0,tmp.dayR)
      ]

    ## does the speciated sample come from before or after
    x[tmp.dayR %in% c(0:window),
      tmp.respecType := data.table::fcase(
        tmp.spFlag==1 & data.table::shift(tmp.spFlag,type="lag")==0, 1,
        tmp.spFlag==1 & data.table::shift(tmp.spFlag,type="lead")==0,2,
        default = 0
      ),
      by = group_vars
    ]

    ## okay; recode the organism based on the flags
    x[,
      c(species) := .(
        data.table::fcase(
          tmp.spFlag==0,  get(species),
          tmp.spFlag==1 & tmp.respecType==1, data.table::shift(get(species),type="lag"),
          tmp.spFlag==1 & tmp.respecType==2, data.table::shift(get(species),type="lead"),
          default = get(species)[1]
        )
      )
    ]

    respecCount <- sum(x$tmp.respecType %in% c(1,2),na.rm=T)

    print(paste0(window,
                 "-day round ",i,": ",respecCount,
                 " SP or UNNAMMED isolates respeciated"))

    ## next loop
    i <- i + 1

  }

  ## remove the keys
  data.table::setkey(x, NULL)

  ## cleanup and remove temp columns
  tmpcols <- grep("^tmp.",colnames(x),value=TRUE)
  x[,
    (tmpcols) := NULL
  ]

  return(x)
}


