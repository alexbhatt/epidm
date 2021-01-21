
#' Continuous Inpatient Spells
#'
#' A continuous inpatient (CIP) spell is a continuous period of care within the NHS, regardless of any transfers which may take place. It can therefore be made up of one or more provider spells. A CIP spell starts when a decision has been made to admit the patient, and a consultant has taken responsibility for their care. The spell ends when the patient dies or is discharged from hospital
#'
#' @seealso http://content.digital.nhs.uk/media/11859/Provider-Spells-Methodology/pdf/Spells_Methodology.pdf
#'
#' @import dplyr
#' @importFrom rlang .data :=
#'
#' @param .data a data frame, tibble; direct or piped (%>%)
#' @param patient_group_vars a vector containing any variables to be used for record grouping, minimum is a patient identifier
#' @param spell_start_date Inpatient provider spell or episode admission date
#' @param admission_method CDS admission method code
#' @param admission_source CDS admission source code
#' @param spell_end_date Inpatient provider spell or episode discharge  date
#' @param discharge_destination CDS discharge destination code
#' @param patient_classification CDS patient classification code
#'
#' @return
#' @export
#'
#' @examples
#'
#' ex <- dplyr::tribble(
#'   ~id, ~provider, ~spell_start, ~spell_end, ~adm_meth, ~adm_src, ~dis_meth, ~dis_dest, ~patclass,
#'   465,    "X1T",  "2020-03-07", "2020-03-07", "21", "19", "1","51",1,
#'   465,    "X1T",  "2020-03-07", "2020-03-25", "81", "51", "1","51",1,
#'   465,    "X1T",  "2020-03-25", "2020-04-02", "21", "19", "1","51",1,
#'   465,    "X1T",  "2020-04-03", "2020-04-27", "81", "51", "1","54",1,
#'   8418,   "KHA",  "2020-01-25", "2020-01-25", "21", "19", "1","51",1,
#'   8418,   "KHA",  "2020-01-26", "2020-01-27", "21", "51", "1","19",1,
#'   8418,   "KHA",  "2020-07-14", "2020-07-17", "11", "19", "1","19",1,
#'   8418,   "KHA",  "2020-08-02", "2020-08-07", "21", "51", "1","19",1,
#'   8418,   "KHA",  "2020-08-12", "2020-08-14", "21", "19", "1","19",1,
#'   8418,   "KHA",  "2020-08-19", "2020-08-19", "21", "19", "1","51",1,
#'   8418,   "KHA",  "2020-08-19", "2020-08-22", "21", "19", "1","19",1,
#'   8418,   "KHA",  "2020-11-19", "2020-12-16", "21", "51", "4","79",1,
#'   26443,  "BX2",  "2019-11-12", "2020-04-17", "21", "19", "1","51",1,
#'   26443,  "BX2",  "2020-04-17", "2020-04-23", "81", "51", "1","51",1,
#'   26443,  "BX2",  "2020-04-23", "2020-05-20", "21", "19", "4","79",1,
#'   33299,  "PXH",  "2020-07-03", "2020-07-24", "81", "51", "1","65",1,
#'   33299,  "PXH",  "2020-01-17", "2020-01-28", "21", "19", "1","19",1,
#'   33299,  "PXH",  "2020-02-07", "2020-02-07", "21", "19", "1","19",1,
#'   33299,  "PXH",  "2020-03-20", "2020-03-23", "21", "19", "1","19",1,
#'   33299,  "PXH",  "2020-04-27", "2020-04-29", "21", "19", "1","19",1,
#'   33299,  "PXH",  "2020-06-21", "2020-06-21", "21", "19", "1","19",1,
#'   33299,  "PXH",  "2020-07-02", "2020-07-03", "21", "19", "1","29",1,
#'   33299,  "PXH",  "2020-10-17", "2020-11-27", "21", "19", "8","98",1,
#'   33299,  "PXH",  "2020-11-27", "2021-01-02", "13", "51", "1","51",1,
#'   33299,  "PXH",  "2021-01-02", "2021-01-10", "13", "19", "4","79",1,
#'   52635,  "9HA",  "2019-12-31", "2019-12-31", "12", "19", "1","19",2,
#'   52635,  "9HA",  "2020-01-02", "2020-01-11", "22", "19", "1","19",1,
#'   52635,  "9HA",  "2020-01-14", "2020-01-14", "12", "19", "1","19",2,
#'   52635,  "9HA",  "2020-01-16", "2020-02-04", "2D", "19", "1","51",1,
#'   52635,  "9HA",  "2020-02-07", "2020-02-07", "13", "19", "1","19",2,
#'   52635,  "9HA",  "2020-02-11", "2020-02-11", "13", "19", "1","19",2,
#'   52635,  "9HA",  "2020-02-14", "2020-02-14", "13", "19", "1","19",2,
#'   52635,  "9HA",  "2020-02-18", "2020-02-18", "13", "19", "1","51",2,
#'   52635,  "9HA",  "2020-02-21", "2020-02-21", "13", "19", "1","51",2,
#'   52635,  "9HA",  "2020-02-25", "2020-02-25", "13", "19", "1","51",2,
#'   52635,  "9HA",  "2020-02-28", "2020-02-28", "13", "19", "1","19",2,
#'   52635,  "9HA",  "2020-03-09", "2020-03-09", "13", "51", "1","19",2,
#'   52635,  "9HA",  "2020-03-11", "2020-03-11", "13", "51", "1","51",2,
#'   52635,  "9HA",  "2020-03-12", "2020-03-12", "13", "51", "1","51",2,
#'   52635,  "9HA",  "2020-03-13", "2020-03-13", "13", "51", "1","19",2,
#'   52635,  "9HA",  "2020-03-14", "2020-03-30", "21", "19", "1","51",1,
#'   52635,  "YYT",  "2020-02-04", "2020-02-07", "81", "51", "1","51",1,
#'   52635,  "YYT",  "2020-02-07", "2020-02-11", "81", "51", "1","51",1,
#'   52635,  "YYT",  "2020-02-11", "2020-02-14", "81", "51", "1","51",1,
#'   52635,  "YYT",  "2020-02-14", "2020-02-18", "81", "51", "1","51",1,
#'   52635,  "YYT",  "2020-02-18", "2020-02-21", "81", "51", "1","51",1,
#'   52635,  "YYT",  "2020-02-21", "2020-02-25", "13", "51", "1","51",1,
#'   52635,  "YYT",  "2020-02-25", "2020-02-28", "81", "51", "1","51",1,
#'   52635,  "YYT",  "2020-02-28", "2020-03-09", "81", "51", "1","51",1,
#'   52635,  "YYT",  "2020-03-09", "2020-03-11", "13", "51", "1","51",1,
#'   52635,  "YYT",  "2020-03-11", "2020-03-12", "13", "51", "1","51",1,
#'   52635,  "YYT",  "2020-03-12", "2020-03-13", "13", "51", "1","51",1,
#'   78915,  "ABX",  "2020-04-16", "2020-04-24", "21", "19", "1","29",1,
#'   78915,  "ABX",  "2020-04-24", "2020-05-13", "11", "51", "1","54",1,
#'   78915,  "ABX",  "2020-05-13", "2020-06-11", "81", "51", "2","19",1
#' ) %>%
#'   dplyr::mutate_at(dplyr::vars(dplyr::contains("spell")),~as.Date(.))
#'
#' ex %>% cip_spells(
#'   patient_group_vars = c(id,provider),
#'   patient_classification = patclass,
#'   spell_start_date = spell_start,
#'   admission_method = adm_meth,
#'   admission_source = adm_src,
#'   spell_end_date = spell_end,
#'   discharge_destination = dis_dest
#' )

cip_spells <- function(.data,
                       patient_group_vars,
                       spell_start_date,
                       admission_method,
                       admission_source,
                       spell_end_date,
                       discharge_destination,
                       patient_classification) {


  ## setup requirement variables
  .data <- .data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      dateNum_start = as.numeric({{spell_start_date}}),
      dateNum_end = as.numeric({{spell_end_date}})+2,
      regular_attender = as.character({{patient_classification}}) %in% c("3","4")
      ) %>%
    dplyr::group_by(dplyr::across({{patient_group_vars}})) %>%
    dplyr::mutate(
      spell_count = dplyr::n(),
      spell_counter = 1:dplyr::n()
      ) %>%

  ## CIP CRITERIA ##############################################################
  # difference between admission and discharge is <2 days
    dplyr::mutate(cip_2daydiff =
                    as.numeric(
                      difftime(
                        dplyr::lead({{spell_start_date}}),
                        {{spell_end_date}},
                        units="days")
                    ) %in% c(0,1,2)
    ) %>%

  # a transfer has taken place (based on these criteria)
  # used the simple criteria, as we dont need to determine transfer type (1,2,3)
    dplyr::mutate(cip_transfer =
                    {{discharge_destination}} %in%
                    c("49", "50", "51", "52", "53", "84") |
                    dplyr::lead({{admission_source}}) %in%
                    c("49", "50", "51", "52", "53", "87") |
                    dplyr::lead({{admission_method}}) %in%
                    c("2B", "81")
    ) %>%

  # exclusion criteria
    dplyr::mutate(cip_exclude =
                    {{discharge_destination}} %in% c("19") &
                    dplyr::lead({{admission_source}}) %in% c("51") &
                    dplyr::lead({{admission_method}}) %in% c("21")
    ) %>%

  ## DATE CLEANUP ##############################################################
    dplyr::ungroup() %>%
    dplyr::mutate(
      proxy_missing = dplyr::case_when(
        is.na({{spell_end_date}}) & spell_counter == spell_count ~ 1,
        is.na({{spell_end_date}}) & spell_counter < spell_count &
          {{discharge_destination}}=="98" ~ 2,
        is.na({{spell_end_date}}) & spell_counter < spell_count &
          {{discharge_destination}}!="98" ~ 3,!is.na({{spell_end_date}}) &
          {{spell_end_date}}<{{spell_start_date}} ~ 4,
        TRUE ~ 0)) %>%
    dplyr::mutate(
      {{spell_end_date}} := dplyr::case_when(
        proxy_missing==0 ~ {{spell_end_date}},
        proxy_missing==1 ~ Sys.Date(),
        proxy_missing==2 ~ dplyr::lead({{spell_start_date}}),
        proxy_missing==3 ~ dplyr::lead({{spell_start_date}})-1,
        proxy_missing==4 ~ {{spell_start_date}},
        TRUE ~ {{spell_end_date}})
    ) %>%

  # group records using cip_valid
    dplyr::mutate(cip_valid =
                    cip_2daydiff &
                    cip_transfer &
                    !cip_exclude &
                    !regular_attender
                  ) %>%
    dplyr::group_by(dplyr::across({{patient_group_vars}})) %>%
    dplyr::mutate(cip_valid =
                    dplyr::case_when(
                      lag(cip_valid) ~ TRUE,
                      is.na(cip_valid) ~ FALSE,
                      TRUE ~ cip_valid
                      )
                  ) %>%

  ## GROUP UP THE TIME CHUNKS ##################################################
  ## +2 to window_cmax to allow for up to 2-day window in line with cip_2daydiff

    dplyr::arrange(dateNum_start,.by_group=T) %>%
    dplyr::mutate(
      window_next = ifelse(
        cip_valid,
        dplyr::lead(dateNum_start,default = dplyr::last(dateNum_start)),
        (spell_counter+1)^3),
      window_cmax = ifelse(
        cip_valid,
        cummax(dateNum_end),
        spell_counter),
      cip_indx = paste0(
        dplyr::cur_group_id(),
        ".",
        spell_count,
        ".",
        c(0,cumsum(window_next > window_cmax))[-dplyr::n()]
      )
    ) %>%
    dplyr::group_by(cip_indx,.add=TRUE) %>%
    dplyr::arrange(dateNum_start,.by_group=T) %>%
    dplyr::mutate(
      cip_spell_start = min({{spell_start_date}}),
      cip_spell_end = max({{spell_end_date}})
    ) %>%
    dplyr::ungroup() %>%
     dplyr::select(-c(
       spell_count,spell_counter,
       window_next,window_cmax,
       dateNum_start,dateNum_end,
       cip_exclude,cip_transfer,cip_2daydiff,
       regular_attender
      ))

  return(.data)
}



