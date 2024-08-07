#' Inpatient Codes cleanup
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'
#' When HES/SUS ICD/OPCS codes are provided in wide format
#'   you may want to clean them up into long for easier analysis.
#'   This function helps by reshaping long as a separate table.
#'   Ensuring they're separate allows you to retain source data, and aggregate
#'   appropriately later.
#'
#' @import data.table
#'
#'
#' @param x a data.frame or data.table containing inpatient data
#' @param field_strings a vector or string containing the regex for the the columns
#' @param patient_id_vars a vector containing colnames used to identify a patient episode or spell
#' @param type a string to denote if the codes are diagnostic or procedural
#' @param .forceCopy default FALSE; TRUE will force data.table to take a copy
#'   instead of editing the data without reference
#'
#' @return a separate table with codes and id in long form
#'
#' @examples
#' inpatient_test <- data.frame(
#' id = c(1053L,5487L,8180L,528L,1085L,344L,2021L,2040L,
#'        6504L,10867L,12411L,7917L,2950L,2812L,7757L,12227L,2675L,
#'        8548L,536L,11830L,12708L,10421L,5503L,2494L,14001L),
#' spell_id = c("dwPDw","iSpUq","qpgk5","8vrJ1","BAur9","l6LZk",
#'              "KJllb","tgZID","fJkh8","Y9IPv","DAlUZ",
#'              "9Ooc4","hUxGn","wtMG9","dw3dO","cz3fI",
#'              "gdxZK","npplb","tynBh","Uu0Sd","gV1Ac",
#'              "vOpA1","ttlcD","Fqo29","ivTmN"),
#' primary_diagnosis_code = c("K602","U071-","I501","U071  ","J22X","J189",
#'                            "J189","I951","N130","U071","K510 D",NA,
#'                            "G409-","C780","N185","J955","K573","U071",
#'                            "I330","L309","M513","U071","A419","U071",
#'                            "N185-"),
#' secondary_diagnosis_code_1 = c("K641","J128-","I489","J128  ","Q348","F059",
#'                                "R296","R296","N131","J128","M0750A",NA,
#'                                "R401-","C782","Z491","C321","D125","J128",
#'                                "B952","J459","M4780","B972","N390","J128",
#'                                "Z491-"),
#' secondary_diagnosis_code_2 = c("E039","B972-","I10X","L031  ","Z115","I509",
#'                                "F051","I251","K862","B972","K590-",NA,
#'                                "E876-","C798","N085","Z938","I209","B972",
#'                                "I214","Z880","M8588","R296","B962","B972",
#'                                NA),
#' secondary_diagnosis_code_3 = c("I422","J9691","E119","I489  ","D509","I489",
#'                                "D509","I252","T391","J440","R21X-",NA,
#'                                "R945-","E119","M310","I480","I252","J9690",
#'                                "E111",NA,"Z115","R410","J181","Z518",NA),
#' secondary_diagnosis_code_4 = c(NA,"I10X-","E669","E109  ","K219","Z921","I251",
#'                                "I259","R458","B972","F200-",NA,"E039-",
#'                                "I10X",NA,"I500","F171","I489","E162",NA,
#'                                "I480","M2551","L892","E86X",NA),
#' secondary_diagnosis_code_5 = c(NA,"E119-","J449","F03X  ",NA,"Z518","I252",
#'                                "I209","C61X","A419","R761-",NA,"E119-",
#'                                "K219",NA,"Z115","F329","N179","N179",NA,
#'                                "H353","Z638","L033","R54X",NA),
#' secondary_diagnosis_code_6 = c(NA,NA,"Z966","I10X  ",NA,"N179","N183","Z115",
#'                                "K627","N390",NA,NA,"J459-","M4780",NA,
#'                                "Z900",NA,"I10X","R34X",NA,"I951","I10X",
#'                                "D510","F059",NA),
#' secondary_diagnosis_code_7 = c(NA,NA,"Z854","I679  ",NA,"N183","Z951","M190",
#'                                "R634","L031",NA,NA,"I10X-","M512",NA,
#'                                "Z921",NA,"E119","I959",NA,"H903","I678",
#'                                "K639","F03X",NA),
#' secondary_diagnosis_code_8 = c(NA,NA,"Z864","J459  ",NA,"E115","E119","N183",
#'                                "E111","E871",NA,NA,"R51X-","H409",NA,
#'                                "Z870",NA,NA,"J90X",NA,"M199","J459",
#'                                "N133","F29X",NA),
#' secondary_diagnosis_code_9 = c(NA,NA,"Z921","R296  ",NA,"L97X","I10X","M4806",
#'                                "E114","S099",NA,NA,"Q070-","H544",NA,
#'                                NA,NA,NA,"I501",NA,"K811","F03X","J90X",
#'                                "N189",NA),
#' secondary_diagnosis_code_10 = c(NA,NA,NA,"Z921  ",NA,"L089","Z921","N40X",
#'                                 "G590","R296",NA,NA,"E668-","Z858",NA,NA,NA,
#'                                 NA,"I489",NA,"K219","G20X","N202",
#'                                 "F719",NA),
#' secondary_diagnosis_code_11 = c(NA,NA,NA,"Z515  ",NA,"R02X","Z507","Z864",
#'                                 "E162","I489",NA,NA,"G473-","Z923",NA,NA,NA,
#'                                 NA,"I447",NA,"J459","E119","L031",
#'                                 "Z960",NA),
#' secondary_diagnosis_code_12 = c(NA,NA,NA,"Z501  ",NA,"B370","K579","Z955",
#'                                 "E46X","Z921",NA,NA,"R600-","Z926",NA,NA,NA,
#'                                 NA,"E86X",NA,"I10X",NA,"J981","Z922",
#'                                 NA),
#' secondary_diagnosis_code_13 = c(NA,NA,NA,"Z507  ",NA,"E039","M109",NA,"I259",
#'                                 "K709",NA,NA,"M1999","Z895",NA,NA,NA,NA,
#'                                 "R33X",NA,"J40X",NA,"E119",NA,NA),
#' secondary_diagnosis_code_14 = c(NA,NA,NA,NA,NA,NA,"J459",NA,"N131","Z864",NA,
#'                                 NA,"R468-","Z902",NA,NA,NA,NA,"R296",
#'                                 NA,NA,NA,"I739",NA,NA),
#' secondary_diagnosis_code_15 = c(NA,NA,NA,NA,NA,NA,"Z880",NA,"K862","Z501",NA,
#'                                 NA,"Z115-","Z971",NA,NA,NA,NA,"R468",
#'                                 NA,NA,NA,"N183",NA,NA),
#' secondary_diagnosis_code_16 = c(NA,NA,NA,NA,NA,NA,"Z867",NA,"T391","Z505",NA,
#'                                 NA,"Z501-","Z878",NA,NA,NA,NA,"R31X",
#'                                 NA,NA,NA,"I489",NA,NA),
#' secondary_diagnosis_code_17 = c(NA,NA,NA,NA,NA,NA,"Z864",NA,"R458","Z518",NA,
#'                                 NA,"Z507-","Z958",NA,NA,NA,NA,"Z115",
#'                                 NA,NA,NA,"M549",NA,NA),
#' secondary_diagnosis_code_18 = c(NA,NA,NA,NA,NA,NA,"F03X",NA,"C61X",NA,NA,NA,
#'                                 NA,"Z867",NA,NA,NA,NA,"I252",NA,NA,
#'                                 NA,"I252",NA,NA),
#' secondary_diagnosis_code_19 = c(NA,NA,NA,NA,NA,NA,NA,NA,"K627",NA,NA,NA,NA,
#'                                 "Z864",NA,NA,NA,NA,"I259",NA,NA,NA,
#'                                 "I259",NA,NA),
#' secondary_diagnosis_code_20 = c(NA,NA,NA,NA,NA,NA,NA,NA,"R634",NA,NA,NA,NA,
#'                                 "Z880",NA,NA,NA,NA,"I10X",NA,NA,NA,
#'                                 "E669",NA,NA),
#' secondary_diagnosis_code_21 = c(NA,NA,NA,NA,NA,NA,NA,NA,"E111",NA,NA,NA,NA,
#'                                 "Z800",NA,NA,NA,NA,"I352",NA,NA,NA,
#'                                 "Z867",NA,NA),
#' secondary_diagnosis_code_22 = c(NA,NA,NA,NA,NA,NA,NA,NA,"E114",NA,NA,NA,NA,
#'                                 "Z801",NA,NA,NA,NA,"R15X",NA,NA,NA,
#'                                 "Z896",NA,NA),
#' secondary_diagnosis_code_23 = c(NA,NA,NA,NA,NA,NA,NA,NA,"G590",NA,NA,NA,NA,
#'                                 NA,NA,NA,NA,NA,"R32X",NA,NA,NA,
#'                                 "Z960",NA,NA),
#' secondary_diagnosis_code_24 = c(NA,NA,NA,NA,NA,NA,NA,NA,"E162",NA,NA,NA,NA,
#'                                 NA,NA,NA,NA,NA,"R418",NA,NA,NA,
#'                                 "Z874",NA,NA),
#' primary_procedure_code = c("H289",NA,"K634",NA,"X292",NA,NA,NA,NA,NA,
#'                            "H251",NA,"U051","L913","X403",NA,"H231",
#'                            "U071","M473","X384",NA,NA,NA,NA,"X403"),
#' primary_procedure_date = c("20170730",NA,"20201202",NA,"20170914",NA,NA,NA,
#'                            NA,NA,"20210105",NA,"20170724",
#'                            "20210111","20171114",NA,"20170622","20210104",
#'                            "20171013","20170313",NA,NA,NA,NA,
#'                            "20171107"),
#' secondary_procedure_code_1 = c("H626",NA,"Y534",NA,"U297",NA,NA,NA,NA,NA,
#'                                "Z286",NA,"Y981","Y031",NA,NA,"Z286",
#'                                "Y981",NA,NA,NA,NA,NA,NA,NA),
#' secondary_procedure_date_1 = c("20170730",NA,"20201202",NA,"20170928",NA,NA,NA,
#'                                NA,NA,"20210105",NA,"20170724",
#'                                "20210111",NA,NA,"20170622","20210104",NA,NA,NA,
#'                                NA,NA,NA,NA),
#' secondary_procedure_code_2 = c("H444",NA,"Z941",NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                "U212",NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA,NA,NA),
#' secondary_procedure_date_2 = c("20170730",NA,"20201202",NA,NA,NA,NA,NA,NA,NA,
#'                                NA,NA,"20170729",NA,NA,NA,NA,NA,NA,
#'                                NA,NA,NA,NA,NA,NA),
#' secondary_procedure_code_3 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Y973",
#'                                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA),
#' secondary_procedure_date_3 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                "20170729",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA,NA),
#' secondary_procedure_code_4 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Y982",
#'                                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA),
#' secondary_procedure_date_4 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                "20170729",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA,NA),
#' secondary_procedure_code_5 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"Z926",
#'                                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA),
#' secondary_procedure_date_5 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                "20170729",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA,NA),
#' secondary_procedure_code_6 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,"O161",
#'                                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA),
#' secondary_procedure_date_6 = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                "20170729",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
#'                                NA,NA)
#' )
#'
#' inpatient_codes(x=inpatient_test,
#'                 field_strings='diagnosis',
#'                 patient_id_vars = c('id','spell_id'),
#'                 type = 'icd10')
#'
#' inpatient_codes(x=inpatient_test,
#'                 field_strings=c('procedure_code','procedure_date'),
#'                 patient_id_vars = c('id','spell_id'),
#'                 type = 'opcs')
#' @export

inpatient_codes <- function(x,
                            field_strings,
                            patient_id_vars,
                            type = c('icd9','icd10','opcs'),
                            .forceCopy=FALSE) {

  ## convert object if its not already
  if(.forceCopy) {
    x <- data.table::copy(x)
  } else {
    data.table::setDT(x)
  }

  ## Needed to prevent RCMD Check fails
  ## recommended by data.table
  ## https://cran.r-project.org/web/packages/data.table/vignettes/datatable-importing.html
  # order_n <- NULL

  ## capture the fields of interest
  ## icd just have the codes
  if(type %in% c('icd9','icd10')){
    fields <- grep(field_strings[1],colnames(x),ignore.case=TRUE,value=TRUE)
    sel <- c(patient_id_vars,fields)
  }

  ## opcs have a date and a procedural code
  if(type=='opcs'){
    fields <- grep(field_strings[1],colnames(x),ignore.case=TRUE,value=TRUE)
    dates <- grep(field_strings[2],colnames(x),ignore.case=TRUE,value=TRUE)
    sel <- c(patient_id_vars,fields,dates)
  }


  ## create a vector of the chosen colnames
  ## use the .. syntax to select the items within the vector
  ## reassignment is required to subset the columns
  x <- x[,..sel]

  ## icd10 and opcs are 4digit codes, so clean em up; dont need the subcodes
  x[,
    (fields) := lapply(.SD,function(x) substr(x,1,4)),
    .SDcols = fields
  ]


  if(type %in% c('icd9','icd10')){
    ## reshape the data from wide to long so we can manipulate it better
    x <- data.table::melt(
      data = x,
      id.vars = patient_id_vars,
      measure_vars = fields,
      variable.name = 'order',
      value.name = type,
      na.rm = TRUE,
      variable.factor = FALSE
    )

    ## order the dataset, makes your life easier
    setorderv(x,c(eval(patient_id_vars)))

  x[,
    order_n := seq_len(.N),
    by = c(eval(patient_id_vars))
    ]
  }
  if(type=='opcs'){
    ## needs to separate out the dates and codes for each
    x <- data.table::melt(
      data = x,
      id.vars = patient_id_vars,
      measure = list(fields,dates),
      variable.name = 'order_n',
      value.name = c(type,'date'),
      na.rm = TRUE,
      variable.factor = FALSE
    )

    ## order the dataset, makes your life easier
    setorderv(x,c(eval(patient_id_vars),'date','order_n'))
  }

  ## drop duplicates
  x <- unique(x,
              by = c(eval(patient_id_vars),type))

  return(x)

}
