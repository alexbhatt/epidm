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
#' @import dplyr
#' @param data a data.frame or tibble containing the line list
#' @param patient_id the column as a character containing the unique patient id
#' @param spec_date the column as a date variable containing the specimen date
#' @param species the column as a character containing the species name
#' @param spec_type the column as a character containing the specimen type
#' @param episode_window an integer to specify the length of the episode
#' @param episode_type a choice of static or rolling for how the window is calculated
#' 
#' @examples 
#' dat <- structure(list(pat_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L), 
#' sp_date = structure(c(18262, 18263, 18281, 18282, 18262, 18263, 18281), 
#'   class = "Date"), 
#' species = c("E. coli", "E. coli", "E. coli", "E. coli", "E. coli", "E. coli", 
#'   "E. coli"), 
#'   spec_type = c("Blood", "Blood", "Blood", "Blood", "Blood", "Blood", 
#'   "Blood")), row.names = c(NA, -7L), class = "data.frame")
#'   
#' monomicrobial_episode(data = dat, patient_id = "pat_id", spec_date = "sp_date",
#'   spec_type = "spec_type", species = "species", episode_window = 14, 
#'   episode_type = "static")
#' 
#'
#' @export

monomicrobial_episode <- function(data,
                                  patient_id,
                                  spec_date,
                                  species,
                                  spec_type,
                                  episode_window = c(1:365),
                                  episode_type = c("static", "rolling")) {


  ## create an episode counter
  i <- 1

  ## NOTE: in code, episode_window-1 becusae 0 days is counting wise, day 1
  episode_window <- episode_window - 1

  ## clearly not the way to deal with NSE, but it works.
  data <- data %>% dplyr::ungroup() %>%
    dplyr::rename(
      date = spec_date,
      id = patient_id,
      species = species,
      spec_type = spec_type
    ) %>%
    dplyr::mutate(episode = 0)


  ## LOOP #######################################################################
  ## this will loop until every isolate has been assigned an episode based on the episode_type
  while (with(data, min(episode, na.rm = T)) == 0) {
    ## day_s static episode window
    ## day_r rolling episode window
    ## day_l lag window for rolling episode

    data <- data %>%
      dplyr::ungroup() %>%
      dplyr::group_by(id, spec_type, species, episode) %>%
      dplyr::arrange(id, spec_type, species, date) %>%
      dplyr::mutate(ep_n = seq(1:n()),
                    ep_n_max = max(ep_n, na.rm = T)) %>%
      dplyr::mutate(day_s = as.numeric(difftime(date,
                                                date[1],
                                                units = "days"))) %>%
      dplyr::mutate(
        day_r = as.numeric(difftime(date, lag(date), units = "days")),
        day_l = lag(day_r),
        day_r_max = (ep_n - 1) * episode_window
      ) %>%
      dplyr::arrange(id, species, spec_type, date) %>%
      dplyr::ungroup()

    ## group episodes based on specimen type, organism, and person
    ## tag ungrouped records which are X day_s apart

    ## ROLLING episodes #########################################################
    ##all isolates within X days of the last reported isolate per episode
    if (episode_type == "rolling") {
      data <- data %>%
        dplyr::group_by(id, spec_type, species, ep_n_max) %>%
        dplyr::arrange(id, spec_type, species, ep_n) %>%
        dplyr::mutate(
          r_ = if_else(day_r %in% c(NA, 0:episode_window), T, F),
          s_ = if_else(day_s <= day_r_max, T, F),
          l_ = if_else(day_l %in% c(NA, 0:episode_window), T, F)
        ) %>%
        dplyr::ungroup(episode) %>%
        dplyr::mutate(
          episode = case_when(
            episode == 0 & ep_n_max == 1 & r_ == T & l_ == T ~ i,
            episode == 0 & r_ == T & s_ == T & l_ == T ~ i,
            TRUE ~ episode
          )
        ) %>%
        dplyr::mutate(episode = if_else(episode == i &
                                          lag(episode) == 0 & ep_n > 1,
                                        0, episode))

    }

    ## STATIC episodes ##########################################################
    ## all isolates within X days of the first reported isolate per episode
    if (episode_type == "static") {
      data <- data %>%
        dplyr::arrange(id, species, spec_type, date) %>%
        dplyr::mutate(episode = if_else(episode == 0 &
                                          day_s %in% c(0:episode_window),
                                        i,
                                        episode))
    }

    ## EPISODE SUMMARY DATA #####################################################
    ## some episode summary data post loop
    data <- data %>% ungroup() %>%
      dplyr::group_by(id, species, episode) %>%
      dplyr::mutate(
        episode_n = seq(1:n()),
        episode_isolate_count = n(),
        episode_date_min = min(date, na.rm = T),
        episode_date_max = max(date, na.rm = T),
        episode_length = difftime(episode_date_max,
                                  episode_date_min,
                                  unit = "days")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(episode = ifelse(episode == 0 &
                                       day_s == 0 & ep_n == 1,
                                     NA, episode)) %>%
      dplyr::arrange(id, species, episode, date)


    print(paste(episode_window + 1, "day", episode_type, "episode round", i))

    ## add 1 to the episode counter
    i <- i + 1

  }


  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id, species) %>%
    dplyr::mutate(episode = if_else(is.na(episode),
                                    max(episode, na.rm = T) + 1,
                                    episode)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-day_r_max, -ep_n_max, -day_s, -day_r, -day_l) %>%
    dplyr::arrange(id, species, date)

  if (episode_type == "rolling") {
    data <- data %>% select(-r_, -s_, -l_)
  }

  ## because i cant figure out the NSE way to fix the date errors this seems to work.
  ## yes I know its bad practice, but unless someone can show me the correct way this is how its gonna be
  names(data)[names(data) == "date"] <- quo_name(spec_date)
  names(data)[names(data) == "species"] <- quo_name(species)
  names(data)[names(data) == "id"] <- quo_name(patient_id)
  names(data)[names(data) == "spec_type"] <- quo_name(spec_type)

  return(data)
}

polymicrobial_episode <- function(data,
                                  patient_id,
                                  species,
                                  spec_date,
                                  spec_type) {

  ## NSE fix part 1
  data <- data %>% dplyr::ungroup() %>%
    dplyr::rename(
      date = spec_date,
      id = patient_id,
      species = species,
      spec_type = spec_type
    )

  ## Create some flags for the data; identify possible polymicrobial groupings
  data <- data %>%
    dplyr::group_by(id, spec_type, date) %>%
    dplyr::arrange(id, spec_type, date, species) %>%
    dplyr::mutate(poly_group = case_when(
      species != lag(species) |
        species != lead(species) ~ T,
      TRUE ~ FALSE
    )) %>%
    dplyr::group_by(id, spec_type, poly_group) %>%
    dplyr::mutate(poly_n = seq(1:n())) %>%
    dplyr::arrange(date, species) %>%
    dplyr::mutate(
      poly_org_count = max(poly_n, na.rm = T),
      poly_episode = ifelse(poly_org_count != 1 &
                              poly_group, 0, NA),
      poly_days = NA
    ) %>%
    dplyr::ungroup()

  ## loop up to group the records into sequential polymicrobial same day episodes
  i <- 1
  print(paste(sum(data$poly_episode == 0, na.rm = T), "polymicrobial records"))

  while (min(data$poly_episode, na.rm = T) == 0) {
    print(paste0("Episode ", i, ": same-day polymicrobial"))
    data <- data  %>%
      dplyr::mutate(poly_sort = poly_episode) %>%
      dplyr::group_by(id, spec_type, poly_sort) %>%
      dplyr::arrange(date, species) %>%
      dplyr::mutate(poly_days = as.integer(difftime(date, date[1]))) %>%
      dplyr::mutate(poly_episode = if_else(poly_days == 0 &
                                             poly_episode == 0 & poly_group,
                                           i, poly_episode)) %>%
      dplyr::ungroup()

    print(paste(sum(data$poly_episode == 0, na.rm = T), "records remaining"))

    i <- i + 1
  }

  ## post loop summary data
  data <- data %>%
    dplyr::select(-poly_sort, -poly_days) %>%
    dplyr::group_by(id, spec_type, poly_episode) %>%
    dplyr::mutate(
      poly_n = seq(1:n()),
      poly_org_count = max(poly_n, na.rm = T),
      poly_n = ifelse(is.na(poly_episode), NA, poly_n),
      episode_org_count = poly_org_count
    ) %>%
    dplyr::ungroup()

  ## NSE fix part 2
  names(data)[names(data) == "date"] <- quo_name(spec_date)
  names(data)[names(data) == "species"] <- quo_name(species)
  names(data)[names(data) == "id"] <- quo_name(patient_id)
  names(data)[names(data) == "spec_type"] <- quo_name(spec_type)

  return(data)
}
