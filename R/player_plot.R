#' Creates a 2D static plot marking the selected frames of a specific player
#'
#' @description Creates a 2D static plot marking the selected frames of a specific player
#'
#' @param tidy_data the processed dataframe ready to do visualizations. It could be obtained using the function get_tidy_data().
#' @param target_player the player whose selected frames will be marked
#' @param start_frames vector with the selected frames focused on when specific actions start
#' @param end_frames vector with the selected frames focused on when specific actions end
#' @param pitch_long long of the pitch in meters
#' @param pitch_width width of the pitch in meters
#' @param pitch_fill colour used to fill the pitch
#' @param pitch_lines_col colour used for lines of the pitch
#' @param action_col colour used for the frames marking related to a specific action
#' @param label_col colour used for labels showing the Time when actions start
#' @param export_png should the plot be exported as png file? (T/F)
#'
#' @return plot with the selected frames of a player on Plots panel and/or as png output file
#' @import dplyr
#' @import purrr
#' @import ggrepel
#' @import ggplot2
#' @import ggforce
#' @importFrom magrittr %>%
#'
#' @export
#'
player_plot <- function(tidy_data, target_player, start_frames, end_frames = NULL,
                        pitch_long = 105, pitch_width = 68,
                        pitch_fill = "#74a9cf", pitch_lines_col = "lightgrey",
                        action_name = "sprints", action_col = "purple", label_col = "white",
                        export_png = F){

        data_player = tidy_data %>%
                      filter(!is.nan(x) & !is.nan(y) & player == target_player)

        data_starts = data_player %>%
                      filter(frame %in% start_frames)

        action_frames = map2(start_frames, end_frames, ~c(seq(.x, .y, 1))) %>% unlist(use.names = F)

        data_action = data_player %>%
                      filter(frame %in% action_frames)

        sp = get_pitch(pitch_fill, pitch_lines_col, pitch_long, pitch_width)

        p = sp +
                geom_point(data = data_action,
                           aes(x = x, y = y), col = action_col,
                           alpha = 0.8, size = 0.5,
                           inherit.aes = T) +
                geom_point(data = data_starts,
                           aes(x = x, y = y), fill = action_col,
                           col = "black", shape = 21, stroke = 1, alpha = 0.8, size = 2,
                           inherit.aes = T) +
                geom_text_repel(data = data_starts, aes(x = x, y = y, label = round(time, 0)),
                                size = 3, col = label_col,
                                inherit.aes = T) +
                theme(legend.position = "none") +
                labs(title = paste0("Player: ", target_player, " / Action: ", action_name),
                     subtitle = "Labels show Time [s] when actions start")

        print(p)

        if (export_png){
                ggsave(paste0(action_name, "_player_", target_player, ".png"), width = 12, height = 8, units = "in")
        }

}
