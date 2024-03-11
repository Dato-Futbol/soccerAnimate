#' Creates a 2D team visualization on a team level with avg. positioning splitting by ON/OFF possesion
#'
#' @description Creates a 2D team visualization on a team level with avg. positioning splitting by ON/OFF possesion
#'
#' @param tidy_data the processed dataframe ready to do visualizations. It could be obtained using either get_tidy_data() or process_catapult() functions.
#' @param events_path path where the CSV file with eventing data is located
#' @param team
#' @param pitch_long long of the pitch in meters
#' @param pitch_width width of the pitch in meters
#' @param pitch_fill colour used to fill the pitch
#' @param pitch_lines_col colour used for lines of the pitch
#' @param target_team_col colour used to fill the border of players circles of the target team (top position on the graph)
#' @param opp_team_col colour used to fill the border of players circles of the opponent team (bottom position on the graph)
#' @param on_ball_col colour used to fill the players when their team was ON possession
#' @param off_ball_col colour used to fill the players when their team was OFF possession
#' @param export_png should the plot be exported as png file? (T/F)
#' @param png_name the name of the exported gif file
#' @param annotation 'NA' by default. If "depth" is setted, vertical lines with lowest defender and highest forward will be added
#'
#' @return soccer plot on 'Plots' panel and/or as output PNG file
#' @import dplyr
#' @import tidyr
#' @import gt
#' @import gtExtras
#' @import ggplot2
#' @import patchwork
#' @importFrom magrittr %>%
#'
#' @export
#'
soccer_plot_poss <- function(tidy_data, events_path, team = c("Home", "Away"),
                             pitch_long = 105, pitch_width = 68,
                             pitch_fill = "#74a9cf", pitch_lines_col = "lightgrey",
                             target_team_col = "#7209b7", opp_team_col = "#06d6a0",
                             on_ball_col = "white", off_ball_col = "#dd3497",
                             export_png = F, png_name = "plot", annotation = "NA"){

        target_team = match.arg(team)
        opp_team = setdiff(unique(ed$Team), target_team)

        ed = readr::read_csv(events_path) %>% dplyr::filter(!Type %in% c("CHALLENGE", "CARD"))


        # target team graph
        p1 = get_poss_graph(tidy_data, ed, target_team, target_team_col, on_ball_col, off_ball_col,
                            pitch_fill, pitch_lines_col, pitch_long, pitch_width)

        # opp team graph
        p2 = get_poss_graph(tidy_data, ed, opp_team, opp_team_col, on_ball_col, off_ball_col,
                            pitch_fill, pitch_lines_col, pitch_long, pitch_width)

        # tables for stats
        gt1_table = gt_grob(p1$stats %>% ungroup() %>% select(-team) %>%
                                    pivot_longer(cols = -poss) %>%
                                    pivot_wider(names_from = poss) %>%
                                    mutate(dif_percent = round((ON-OFF)/OFF*100, 1),
                                           dif_percent = ifelse(dif_percent > 0 , paste0("+", dif_percent), dif_percent),
                                           name = gsub("_pos", "", name),
                                           name = str_to_title(gsub("_", " ", name))) %>%
                                    gt() %>%
                                    gt_theme_538() %>%
                                    cols_label(name = "metric name",
                                               dif_percent = "diff. [%]") %>%
                                    cols_align(align = "center",
                                               columns = everything()) %>%
                                    fmt_number(decimals = 1) %>%
                                    tab_options(table.font.size = 28))

        gt2_table = gt_grob(p2$stats %>% ungroup() %>% select(-team) %>%
                                    pivot_longer(cols = -poss) %>%
                                    pivot_wider(names_from = poss) %>%
                                    mutate(dif_percent = round((ON-OFF)/OFF*100, 1),
                                           dif_percent = ifelse(dif_percent > 0 , paste0("+", dif_percent), dif_percent),
                                           name = gsub("_pos", "", name),
                                           name = str_to_title(gsub("_", " ", name))) %>%
                                    gt() %>%
                                    gt_theme_538() %>%
                                    cols_label(name = "metric name",
                                               dif_percent = "diff. [%]") %>%
                                    cols_align(align = "center",
                                               columns = everything()) %>%
                                    fmt_number(decimals = 1) %>%
                                    tab_options(table.font.size = 28))


        graphs = (p1$graph + inset_element(gt1_table, left = 0.72, bottom = 0.35, right = 0.955, top = 0.65)) /
                 (p2$graph + inset_element(gt2_table, left = 0.72, bottom = 0.35, right = 0.955, top = 0.65))


        if (export_png){
                ggsave(paste0(png_name, ".png"), width = 9, height = 12, units = "in")
        }

        graphs
}
