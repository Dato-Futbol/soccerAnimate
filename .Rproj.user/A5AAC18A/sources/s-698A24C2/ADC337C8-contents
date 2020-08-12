#' A function
#'
#' @param provider A character variable to set the name of the tracking data provider which defines the data format
#' @param home_team_file A character variable to set the filename which contains the data of the home team
#' @param away_team_file A character variable to set the filename which contains the data of the away team
#'
#' @return The dataframe with all tidy data joined (long format)
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @export
#'
get_tidy_data <- function(provider = "Metrica", home_team_file, away_team_file) {

        if(provider == "Metrica"){
                # home team
                track_home <- read_csv(home_team_file, skip=2)

                player_names_home <- track_home %>%
                        dplyr::select_at(dplyr::vars(starts_with(c("Player", "Ball")))) %>%
                        names()

                track_home_long <- track_home %>%
                        dplyr::rename_at(dplyr::vars(starts_with(c("Player", "Ball"))), funs(paste0(., "_px"))) %>%
                        dplyr::rename_at(dplyr::vars(starts_with("X")), funs(paste0(player_names_home, "_py"))) %>%
                        tidyr::pivot_longer(cols = starts_with(c("Player", "Ball")),
                                            names_to = c("Player", ".value"),
                                            names_pattern = "Player(.*)_p(.)") %>%
                        dplyr::mutate(team = "home")

                # away team
                track_away <- read_csv(away_team_file, skip=2)

                player_names_away <- track_away %>%
                        dplyr::select_at(dplyr::vars(starts_with(c("Player", "Ball")))) %>%
                        names()

                track_away_long <- track_away %>%
                        dplyr::rename_at(dplyr::vars(starts_with(c("Player", "Ball"))), funs(paste0(., "_px"))) %>%
                        dplyr::rename_at(dplyr::vars(starts_with("X")), funs(paste0(player_names_away, "_py"))) %>%
                        tidyr::pivot_longer(cols = starts_with(c("Player", "Ball")),
                                            names_to = c("Player", ".value"),
                                            names_pattern = "Player(.*)_p(.)") %>%
                        dplyr::mutate(team = "away")

                track_data_long <- track_home_long %>%
                        dplyr::bind_rows(track_away_long) %>%
                        dplyr::mutate(Period = as.integer(Period),
                                      Frame = as.integer(Frame)) %>%
                        dplyr::rename("Time" = "Time [s]")

                track_data_long

        }else{
                message("Currently only the data format of Metrica Sport provider is supported")
        }
}
