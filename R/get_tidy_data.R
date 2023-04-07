#' Prepares data
#'
#' @description
#' Reads and converts both the home and away team rawdata to tidy data (long format) and join them.
#' Currently only Metrica Sports data format is supported
#'
#' @param home_team_file set the filename which contains the data of the home team
#' @param away_team_file set the filename which contains the data of the away team
#' @param provider set the name of the tracking data provider which defines the data format
#' @param add_velocity A boolean variable to set if the players velocities should to be added
#' @param pitch_long long of the pitch in meters
#' @param pitch_width width of the pitch in meters
#'
#' @return The dataframe with all tidy data joined (long format)
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom readr read_csv
#' @importFrom janitor clean_names
#' @export
#'
get_tidy_data <- function(home_team_file, away_team_file, provider = "Metrica",
                          add_velocity = T, pitch_long = 105, pitch_width = 68) {

        if(provider == "Metrica"){

                track_home = suppressWarnings(read_csv(home_team_file, skip = 2)) %>% janitor::clean_names()
                track_away = suppressWarnings(read_csv(away_team_file, skip = 2)) %>% janitor::clean_names()

                # home team
                track_home_long = read_clean_process("home", track_home)

                # away team
                track_away_long = read_clean_process("away", track_away)

                # ball
                track_ball_long = read_clean_process("ball", track_home)

                #bind rows
                track_data_long = track_home_long %>%
                                  dplyr::bind_rows(track_away_long) %>%
                                  dplyr::bind_rows(track_ball_long) %>%
                                  dplyr::rename("time" = "time_s") %>%
                                  dplyr::mutate(period = as.integer(period),
                                                frame = as.integer(frame),
                                                second = floor(time),
                                                y = (1 - y), # invert Y axis
                                                # convert to meters based on pitch dim + invert coordinates for period 2:
                                                y = ifelse(period == 1, pitch_width * y, pitch_width * (1 - y)),
                                                x = ifelse(period == 1, pitch_long * x, pitch_long * (1 - x)))

                if (add_velocity){

                        temp = track_data_long %>%
                               filter(!is.nan(x) & team != "ball") %>%
                               group_by(player) %>%
                               mutate(dx = x - lag(x, MS_LAG_SMOOTH),
                                      dy = y - lag(y, MS_LAG_SMOOTH),
                                      v_mod = sqrt(dx^2 + dy^2),
                                      speed = v_mod/(MS_DT * MS_LAG_SMOOTH),
                                      speed = ifelse(speed > PLAYER_MAX_SPEED, PLAYER_MAX_SPEED, speed))

                        track_data_long = track_data_long %>%
                                          filter(!is.nan(x) & team == "ball") %>%
                                          mutate(dx = NA, dy =NA, v_mod = NA, speed = NA) %>%
                                          bind_rows(temp)

                }

                track_data_long %>% mutate(player = gsub("_", "", player))

        }else{
                message("Currently only the data format of Metrica Sports provider is supported")
        }
}
