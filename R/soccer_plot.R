#' Creates a 2D static plot
#'
#' @description Creates a 2D static plot of a unique frame from soccer tracking data
#'
#' @param tidy_data the processed dataframe ready to do visualizations. It could be obtained using either get_tidy_data() or process_catapult() functions.
#' @param target_frame the unique frame of the tracking data to visualize
#' @param method four different approaches to visualize: base, convexhull, voronoi, delaunay
#' @param pitch_long long of the pitch in meters
#' @param pitch_width width of the pitch in meters
#' @param pitch_fill colour used to fill the pitch
#' @param pitch_lines_col colour used for lines of the pitch
#' @param home_team_col colour used to fill the players of the home team
#' @param away_team_col colour used to fill the players of the away team
#' @param title graph title
#' @param subtitle graph subtitle
#' @param provider set the name of the tracking data provider which defines the data format
#' @param export_png should the plot be exported as png file? (T/F)
#' @param png_name the name of the exported gif file
#'
#' @return soccer plot on Plots panel and/or as output file
#' @importFrom deldir deldir
#' @import dplyr
#' @importFrom ggforce geom_voronoi_tile
#' @importFrom ggforce geom_delaunay_tile
#' @import ggplot2
#' @importFrom grDevices chull
#' @importFrom magrittr %>%
#' @importFrom utils head
#'
#' @export
#'
soccer_plot <- function(tidy_data, target_frame, method = "base",
                        pitch_long = 105, pitch_width = 68,
                        pitch_fill = "#74a9cf", pitch_lines_col = "lightgrey",
                        home_team_col = "white", away_team_col= "#dd3497",
                        provider = "Metrica", export_png= F, png_name = "plot",
                        title = "", subtitle = ""){

        frames <- unique(tidy_data$frame)

        if(target_frame %in% frames){

                data = tidy_data %>%
                       dplyr::filter(!is.nan(x) & !is.nan(y) & frame == target_frame) %>%
                       mutate(vx = dx/(MS_DT*MS_LAG_SMOOTH),
                              vy = dy/(MS_DT*MS_LAG_SMOOTH))

                ball_data = data %>% filter(team == "ball")

                if(nrow(ball_data) == 0){
                        ball_data_temp = data %>% head(1) %>% mutate(across(everything(), ~NA)) %>% mutate(team = "ball")
                        data = bind_rows(data, ball_data_temp)
                }

                sp = get_pitch(pitch_fill, pitch_lines_col, pitch_long, pitch_width)

                if (provider == "Metrica"){

                        if (method == "base"){

                                p = sp
                        }

                        if (method == "convexhull"){

                                hull_data = data %>%
                                            dplyr::filter(team != "ball" & is_gk == F) %>%
                                            dplyr::group_by(time, team) %>%
                                            dplyr::slice(chull(x, y))

                                p = sp +
                                    geom_polygon(data = hull_data,
                                                 aes(x = x, y = y, fill = team),
                                                 alpha=0.3, inherit.aes = T)
                        }

                        if (method %in% c("voronoi", "delaunay")){

                                vor_data = data %>%
                                           dplyr::filter(team != "ball") %>%
                                           mutate(x = ifelse(x > 105, 105, ifelse(x < 0, 0, x)),
                                                  y = ifelse(y > 68, 68, ifelse(y < 0, 0, y)))

                                if (method == "delaunay"){
                                        p = sp +
                                            geom_delaunay_tile(data = vor_data,
                                                               mapping = aes(x = x, y = y, fill = team, group = -1L),
                                                               colour = 'black', alpha = 0.3, bound = c(0, pitch_long, 0, pitch_width), inherit.aes = T)
                                }

                                if (method == "voronoi"){
                                        p = sp +
                                            geom_voronoi_tile(data = vor_data,
                                                              mapping = aes(x = x, y = y, fill = team, group = -1L),
                                                              colour = 'black', alpha = 0.3, bound = c(0, pitch_long, 0, pitch_width), inherit.aes = T)
                                }
                        }

                        fill_colors = c("home" = home_team_col, "away" = away_team_col, "ball" = "darkblue")

                        p = p +
                            geom_segment(data = data %>% dplyr::filter(team != "ball"),
                                         aes(x = x, y = y,
                                             xend = x + vx, yend = y + vy),
                                         arrow = arrow(length = unit(0.005, "npc"), ends = "last"),
                                         inherit.aes = T) +
                            geom_point(data = data,
                                       aes(x = x, y = y, fill = team, size = team),
                                       col = "black", shape = 21, stroke = 1, alpha = 0.9,
                                       inherit.aes = T) +
                            geom_text(data = data,
                                      aes(label = player, x = x, y = y, col = team), size = 3,
                                      inherit.aes = T) +
                            theme(legend.position = "none") +
                            scale_size_manual(values = c(7, 3, 7)) +
                            scale_fill_manual(values = fill_colors) +
                            scale_colour_manual(values = c("white", "darkblue", "black"))

                        if (title != "" | subtitle != ""){
                                p = p +
                                    labs(title = title, subtitle = subtitle) +
                                    theme(plot.title = element_text(colour = "black", face = "bold", size = 14),
                                          plot.subtitle = element_text(colour = "black", face = "plain", size = 12))
                        }

                        print(p)

                        if (export_png){
                                ggsave(paste0(png_name, ".png"), width = 12, height = 8, units = "in")
                        }

                } else{
                        message("Currently only the data format of the Metrica Sports provider is supported.
                                If you have a dataset either from a different provider or with another format,
                                please create an issue here: https://github.com/Dato-Futbol/soccerAnimate/issues")
                }
        } else{
                message("The input frame is not present in the data")
        }

}
