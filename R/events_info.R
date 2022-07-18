#' Searches time ocurrence and other information of specific events
#'
#' @param data event dataframe previously loaded
#' @param events one or more events to search (options: SHOT, GOAL, FREE KICK, CORNER KICK)
#'
#' @return dataframe with searched events and their specific info
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @importFrom janitor clean_names
#' @export
#'
events_info <- function(data, events = c("SHOT", "GOAL", "FREE KICK", "CORNER KICK")){

        data <- data %>%
                janitor::clean_names() %>%
                dplyr::mutate(event = ifelse(type == "SHOT" & stringr::str_detect(subtype, '-GOAL'),
                                             "GOAL", ifelse(type == "SET PIECE",  subtype, type)))

        data %>%
                dplyr::filter(event %in% events) %>%
                dplyr::select(c(team, type, subtype, period, start_time = start_time_s, end_time = end_time_s,
                                start_frame, end_frame, event))
}
