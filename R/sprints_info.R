#' Get the start/end time of the sprints that every player made
#'
#' @param data dataframe with the already processed tracking data in long format
#' @param team team to graph "home" or "away"
#'
#' @return dataframe with the results
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
sprints_info <- function(data, team = c("home", "away")){

        target_team = match.arg(team)

        data = data %>% filter(team == target_team)

        conv = data %>%
               filter(team == target_team & !is.na(speed)) %>%
               mutate(is_sprint = ifelse(speed >= RUN_SPRINT_THRESHOLD, 1, 0)) %>%
               group_by(player, team) %>%
               summarise(conv = round(convolve(is_sprint, rep(1, SPRINTS_WINDOW), type = "open"), 0)) %>%
               mutate(start = ifelse(conv == SPRINTS_WINDOW & lag(conv) != SPRINTS_WINDOW, 1, 0),
                      end = ifelse(conv == 0 & lag(conv, SPRINTS_WINDOW) == SPRINTS_WINDOW, 1, 0),
                      n = 1:n()) %>%
               select(-conv)

        conv
}
