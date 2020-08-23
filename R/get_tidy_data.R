#' Prepares data
#'
#' @description
#' Reads and converts both the home and away team rawdata to tidy data (long format) and join them.
#' Currently only Metrica Sport's data format is supported
#'
#' @param home_team_file set the filename which contains the data of the home team
#' @param away_team_file set the filename which contains the data of the away team
#' @param provider set the name of the tracking data provider which defines the data format
#' @param convert_coord A boolean variable to set if the coordinates should to be converted to meters
#'
#' @return The dataframe with all tidy data joined (long format)
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @export
#'
get_tidy_data <- function(home_team_file, away_team_file, provider = "Metrica", convert_coord = T) {

        if(provider == "Metrica"){
                # home team
                track_home <- suppressWarnings(read_csv(home_team_file, skip=2))

                player_names_home <- track_home %>%
                        dplyr::select_at(dplyr::vars(starts_with("Player"))) %>%
                        names()

                track_home_long <- track_home %>%
                        dplyr::select(-c(32, 33)) %>%
                        dplyr::rename_at(dplyr::vars(starts_with("Player")), list(~paste0(., "_px"))) %>%
                        dplyr::rename_at(dplyr::vars(starts_with("X")), list(~paste0(player_names_home, "_py"))) %>%
                        tidyr::pivot_longer(cols = starts_with("Player"),
                                            names_to = c("Player", ".value"),
                                            names_pattern = "Player(.*)_p(.)") %>%
                        dplyr::mutate(Team = "Home",
                                      is_gk = ifelse(Player == gsub("Player", "", player_names_home[1]), T, F))

                # away team
                track_away <- suppressWarnings(read_csv(away_team_file, skip=2))

                player_names_away <- track_away %>%
                        dplyr::select_at(dplyr::vars(starts_with("Player"))) %>%
                        names()

                track_away_long <- track_away %>%
                        dplyr::select(-c(32, 33)) %>%
                        dplyr::rename_at(dplyr::vars(starts_with("Player")), list(~paste0(., "_px"))) %>%
                        dplyr::rename_at(dplyr::vars(starts_with("X")), list(~paste0(player_names_away, "_py"))) %>%
                        tidyr::pivot_longer(cols = starts_with("Player"),
                                            names_to = c("Player", ".value"),
                                            names_pattern = "Player(.*)_p(.)") %>%
                        dplyr::mutate(Team = "Away",
                                      is_gk = ifelse(Player == gsub("Player", "", player_names_away[1]), T, F))

                # ball
                track_ball_long <- track_home %>%
                        dplyr::mutate(Player = "", Team = "Ball") %>%
                        dplyr::rename("x" = "Ball", "y" = "X33") %>%
                        dplyr::mutate(is_gk = F) %>%
                        dplyr::select(names(track_home_long))


                #bind rows
                track_data_long <- track_home_long %>%
                        dplyr::bind_rows(track_away_long) %>%
                        dplyr::bind_rows(track_ball_long) %>%
                        dplyr::rename("Time" = "Time [s]") %>%
                        dplyr::mutate(Period = as.integer(Period),
                                      Frame = as.integer(Frame),
                                      Second = floor(Time))

                if (convert_coord){
                        track_data_long <- track_data_long %>%
                                dplyr::mutate(y = 68 * (1 - y),
                                              x = ifelse(Period == 1, 105 * x, 105 * (1 - x)))
                }

                track_data_long

        }else{
                message("Currently only the data format of Metrica Sport provider is supported")
        }
}
