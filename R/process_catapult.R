#' Preprocess Catapult data
#'
#' @description
#' Reads Catapult data on CSV (already on long format) and process it.
#'
#' @param home_data_file set the data file path of the Home team
#' @param away_data_file set the data file path of the Away team
#'
#' @return The processed dataframe ready to do visualizations
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom geosphere lengthLine
#' @importFrom purrr map2
#' @export
#'

process_catapult <- function(home_data_file, away_data_file, ref_p1 = NULL, ref_p2 = NULL, ref_p3 = NULL, ref_p4 = NULL){

  options(digits = 10, digits.secs = 1)
  home_data = read_csv(home_data_path) %>% mutate(team = 'home')
  away_data = read_csv(away_data_path) %>% mutate(team = 'away')
  raw_data = bind_rows(home_data, away_data)

  points_long = matrix(c(ref_p1[2], ref_p2[2], ref_p1[1], ref_p2[1]), nrow = 2)
  pitch_long = round(lengthLine(points_long), 0)

  points_width = matrix(c(ref_p2[2], ref_p3[2], ref_p2[1], ref_p3[1]), nrow = 2)
  pitch_width = round(lengthLine(points_width), 0)

  dist_ref_p1 = map2(raw_data$long, raw_data$lat, ~lengthLine(matrix(c(ref_p1[2], .x, ref_p1[1], .y), nrow = 2))) %>% unlist(use.names = F)
  dist_ref_p4 = map2(raw_data$long, raw_data$lat, ~lengthLine(matrix(c(ref_p4[2], .x, ref_p4[1], .y), nrow = 2))) %>% unlist(use.names = F)

  data_scaled = raw_data %>%
                mutate(dist_ref_p1 = dist_ref_p1,
                       dist_ref_p4 = dist_ref_p4,
                       angle_p4 = acos((pitch_width^2 + dist_ref_p4^2 - dist_ref_p1^2)/(2*pitch_width*dist_ref_p4)),
                       x_scaled = sin(angle_p4)*dist_ref_p4,
                       y_scaled = cos(angle_p4)*dist_ref_p4,
                       ms = floor(seconds*10),
                       ts_ms = paste0(ts, ".", ms))

  data = data_scaled %>%
         mutate(frame = group_indices_(data_scaled, .dots = c('ts', 'ms'))) %>%
         ungroup() %>%
         mutate(is_gk = F) %>%
         arrange(ts, ms) %>%
         select(period = period_name, frame, time = ts_ms, player = jersey, x = x_scaled, y = y_scaled, team, is_gk) %>%
         mutate(time = frame/10,
                second = round(time, 0))

  data

}
