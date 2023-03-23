#' Creates 2D soccer animations
#'
#' @description Creates 2D animations using soccer tracking data allowing you export it as gif file
#'
#' @param tidy_data the processed dataframe ready to do visualizations. It could be obtained using either get_tidy_data() or process_catapult() functions.
#' @param ini_time the time in seconds of tracking data to consider as initial time of the animation
#' @param end_time the time in seconds of tracking data to consider as ending time of the animation
#' @param method four different approaches to visualize: base (default), convexhull, voronoi, delaunay
#' @param pitch_long long of the pitch in meters
#' @param pitch_width width of the pitch in meters
#' @param pitch_fill colour used to fill the pitch
#' @param pitch_lines_col colour used for lines of the pitch
#' @param home_team_col colour used to fill the players of the home team
#' @param away_team_col colour used to fill the players of the away team
#' @param title graph title
#' @param subtitle graph subtitle
#' @param provider set the name of the tracking data provider which defines the data format
#' @param show_anim should the animation be shown on the viewer? (T/F)
#' @param export_gif should the animation be exported as gif file? (T/F)
#' @param gif_name the name of the exported gif file
#'
#' @return animated gif on viewer and/or as output file
#' @importFrom deldir deldir
#' @import dplyr
#' @import gganimate
#' @importFrom ggforce geom_voronoi_tile
#' @importFrom ggforce geom_delaunay_tile
#' @import ggplot2
#' @import gifski
#' @importFrom grDevices chull
#' @importFrom magrittr %>%
#' @export
#'
soccer_animate <- function(tidy_data, ini_time, end_time, method = "base",
                           pitch_long = 105, pitch_width = 68,
                           pitch_fill = "#74a9cf", pitch_lines_col = "lightgrey",
                           home_team_col = "white", away_team_col= "#dd3497",
                           title = "", subtitle = "",  provider = "Metrica",
                           show_anim = T, export_gif= F, gif_name = "animation"){

        if(end_time >= ini_time){

                data <- tidy_data %>%
                        dplyr::filter(!is.nan(x) & !is.nan(y) & second >= ini_time & second <= end_time)

                ball_data = data %>% filter(team == "ball")

                if(nrow(ball_data) == 0){
                        ball_data_temp = data %>% head(1) %>% mutate(across(everything(), ~NA)) %>%
                                         mutate(team = "ball", x = 0, y = 0,
                                                time = min(data$time, na.rm = T), second = min(data$second, na.rm = T))
                        data = bind_rows(data, ball_data_temp)
                        ball_col = "transparent"

                }else{ball_col = "darkblue"}

                sp <- get_pitch(pitch_fill, pitch_lines_col, pitch_long, pitch_width)

                if (provider == "Metrica"){

                        if (method == "base"){

                                anim <- sp
                        }

                        if (method == "convexhull"){

                                hull_data <- data %>%
                                        dplyr::filter(team != "ball" & is_gk == F) %>%
                                        dplyr::group_by(time, team) %>%
                                        dplyr::slice(chull(x, y))

                                anim <- sp +
                                        geom_polygon(data = hull_data,
                                                     aes(x = x, y = y, fill = factor(team)),
                                                     alpha=0.3, inherit.aes = T)
                        }

                        if (method %in% c("voronoi", "delaunay")){

                                vor_data <- data %>%
                                        dplyr::filter(team != "ball") %>%
                                        mutate(x = ifelse(x > 105, 105, ifelse(x < 0, 0, x)),
                                               y = ifelse(y > 68, 68, ifelse(y < 0, 0, y)))

                                if (method == "delaunay"){
                                        anim <- sp +
                                                geom_delaunay_tile(data = vor_data,
                                                                   mapping = aes(x = x, y = y, fill = factor(team), group = -1L),
                                                                   colour = 'black', alpha = 0.3, bound = c(0, pitch_long, 0, pitch_width), inherit.aes = T)
                                }

                                if (method == "voronoi"){
                                        anim <- sp +
                                                geom_voronoi_tile(data = vor_data,
                                                                   mapping = aes(x = x, y = y, fill = factor(team), group = -1L),
                                                                   colour = 'black', alpha = 0.3, bound = c(0, pitch_long, 0, pitch_width), inherit.aes = T)
                                }
                        }

                        anim <- anim +
                                geom_point(data = data,
                                           aes(x = x, y = y, fill = factor(team), size = factor(team)),
                                           col = "black", shape = 21, stroke = 1, alpha = 0.8,
                                           inherit.aes = T) +
                                geom_text(data = data, aes(label = player, x = x, y = y, col = team),
                                          inherit.aes = T) +
                                transition_time(time) +
                                theme(legend.position = "none") +
                                scale_size_manual(values = c(8,4,8)) +
                                scale_fill_manual(values = c(away_team_col, ball_col, home_team_col)) +
                                scale_colour_manual(values = c("white", ball_col, "black"))

                        if (title != "" | subtitle != ""){
                                anim <- anim +
                                        labs(title = title, subtitle = subtitle) +
                                        theme(plot.title = element_text(colour = "black", face = "bold", size = 14, family = "Helvetica"),
                                              plot.subtitle = element_text(colour = "black", face = "plain", size = 12, family = "Helvetica"))
                        }

                        fs = case_when(provider == "Metrica" ~ 25L)

                        a <- gganimate::animate(anim,
                                                width = 900, height = 600,
                                                nframes = length(unique(data$second))*fs, fps = fs)

                        if (export_gif){
                                print(a)
                                gganimate::anim_save(paste0(gif_name, ".gif"))
                        } else{
                                if (show_anim){
                                        print(a)
                                }
                        }

                } else{
                       message("Currently only the data format of the Metrica Sports provider is supported.
                               If you have a dataset either from a different provider or with another format,
                               please create an issue here: https://github.com/Dato-Futbol/soccerAnimate/issues")
                }
        } else{
                message("Ending time should to be either equal or higher than initial time")
        }

}
