#' Clean raw data and process it to get the long format
#'
#' @param venue home, away or ball case
#' @param tracking_data imported raw tracking data with Metrica Sports format
#'
#' @return dataframe with the processed tracking data in long format
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @noRd
#' @keywords internal
read_clean_process = function(venue, tracking_data){

  ball_column = which(str_detect("ball", names(tracking_data)))

  if(venue == "ball"){

    output = tracking_data %>%
              dplyr::mutate(player = "", team = "ball") %>%
              dplyr::rename("x" = "ball", "y" = ball_column + 1) %>%
              dplyr::mutate(is_gk = F) %>%
              dplyr::select(LONG_FORMAT_COLUMNS)
  } else {

    player_names = tracking_data %>%
                  dplyr::select(starts_with("player")) %>%
                  names()

    output = tracking_data %>%
            dplyr::select(-c(ball_column, ball_column + 1)) %>% # removing ball info
            dplyr::rename_with(~paste0(., "_px"), starts_with("player")) %>%
            dplyr::rename_with(~paste0(player_names, "_py"), starts_with("x")) %>%
            tidyr::pivot_longer(cols = starts_with("player"),
                                names_to = c("player", ".value"),
                                names_pattern = "player(.*)_p(.)") %>%
            dplyr::mutate(team = venue,
                          is_gk = ifelse(player == gsub("player", "", player_names[1]), T, F))

  }

  output

}



#' Draws a soccer pitch as a ggplot background layer
#'
#' @param pitch_fill colour used to fill the pitch
#' @param pitch_col colour used for lines of the pitch
#' @param pitch_long long of the pitch in meters, default value = 105 meters
#' @param pitch_width width of the pitch in meters, default value = 68 meters
#'
#' @return ggplot object
#' @import ggplot2
#' @importFrom ggforce geom_circle
#' @importFrom ggforce geom_arc
#' @noRd
#' @keywords internal
#'
get_pitch <- function(pitch_fill = "#74a9cf", pitch_col = "lightgrey", pitch_long = 105, pitch_width = 68){

        ggplot() +
                theme_bw() +
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.y = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_rect(fill = pitch_fill),
                      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")) +
                scale_x_continuous(limits = c(-5, 110), expand = c(0, 0)) +
                scale_y_continuous(limits = c(-5, 73), expand = c(0, 0)) +
                geom_rect(aes(xmin = 0, xmax = pitch_long, ymin = 0, ymax = pitch_width), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 13.85, ymax = 54.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 88.5, xmax = pitch_long, ymin = 13.85, ymax = 54.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 24.85, ymax = 43.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 99.5, xmax = pitch_long, ymin = 24.85, ymax = 43.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = -2, xmax = 0, ymin = 30.34, ymax = 37.66), fill = "transparent", col = pitch_col) +
                geom_rect(aes(xmin = pitch_long, xmax = 107, ymin = 30.34, ymax = 37.66), fill = "transparent", col = pitch_col) +
                geom_circle(aes(x0 = 52.5, y0 = 34, r = 9.15), fill = pitch_fill, col = pitch_col) +
                geom_segment(aes(x = 52.5, xend = 52.5, y = 0, yend = pitch_width), col = pitch_col) +
                geom_point(aes(x = 11, y = 34), col = pitch_col) +
                geom_point(aes(x = 94, y = 34), col = pitch_col) +
                geom_point(aes(x = 52.5, y = 34), col = pitch_col) +
                geom_arc(aes(x0 = 11, y0 = 34, r = 9.15, start = 37*pi/180, end = 143*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 94, y0 = 34, r = 9.15, start = -37*pi/180, end = -143*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = 90*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 0, y0 = pitch_width, r = 1, start = 90*pi/180, end = 180*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = pitch_long, y0 = pitch_width, r = 1, start = 180*pi/180, end = 270*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = pitch_long, y0 = 0, r = 1, start = 270*pi/180, end = 360*pi/180), col = pitch_col)
}



#'
#'
#' @param tidy_data the processed dataframe ready to do visualizations. It could be obtained using either get_tidy_data() or process_catapult() functions.
#' @param ed
#' @param target_team
#' @param team_col colour used to fill the border of players circles of the target team (top position on the graph)
#' @param on_ball_col colour used to fill the players when their team was ON possession
#' @param off_ball_col colour used to fill the players when their team was OFF possession
#' @param pitch_fill colour used to fill the pitch
#' @param pitch_lines_col colour used for lines of the pitch
#' @param pitch_long long of the pitch in meters
#' @param pitch_width width of the pitch in meters
#' @param annotation 'NA' by default. If "depth" is setted, vertical lines with lowest defender and highest forward will be added
#'
#' @return a list of 2 objects: the graph and the stats for the target team
#' @import dplyr
#' @import tidyr
#' @importFrom stats sd
#' @import glue
#' @import ggplot2
#' @importFrom utils head
#' @importFrom magrittr %>%
#'
#' @noRd
#' @keywords internal
get_poss_graph = function(tidy_data, ed, target_team, team_col,
                          on_ball_col, off_ball_col, pitch_fill, pitch_lines_col,
                          pitch_long, pitch_width, annotation = "NA"){

        frames_ranges = poss_frames(ed, team = target_team)

        data = tidy_data %>%
                filter(!is.nan(x) & !is.nan(y) & team == tolower(target_team))

        players_samples = data %>%
                group_by(player) %>%
                summarise(n = n()) %>%
                arrange(desc(n))

        # by default 11 players who played most of the time
        data = data %>%
                filter(player %in% c(players_samples %>% head(11) %>% pull(player)))

        poss_data = get_poss_data(frames_ranges$poss_start, frames_ranges$poss_end, data, target_team)

        poss_off_data = data %>%
                filter(team == tolower(target_team) & !frame %in% poss_data$frame)

        avg_pos = poss_data %>% mutate(poss = "ON") %>%
                bind_rows(poss_off_data %>% mutate(poss = "OFF")) %>%
                group_by(team, player, is_gk, poss) %>%
                summarise(avg_x = mean(x),
                          avg_y = mean(y))

        avg_pos_wide = avg_pos %>%
                pivot_wider(id_cols =  c(team, player, is_gk),
                            names_from = poss,
                            values_from = c(avg_x, avg_y))

        title_text = glue("<strong><span style='color:{on_ball_col}'>ON</span></strong></b> / <strong><span style='color:{off_ball_col}'>OFF</strong></b> ball avg. positioning - <strong><span style='color:{team_col}'>{target_team} Team")

        sp = get_pitch(pitch_fill, pitch_lines_col, pitch_long, pitch_width) +
                ggtitle(label = title_text) +
                theme(plot.title = element_markdown(face = "bold", size = 14))

        #if (provider %in% c("Metrica")){

        hull_data = avg_pos %>%
                dplyr::filter(is_gk == F) %>%
                dplyr::group_by(poss) %>%
                dplyr::slice(chull(avg_x, avg_y))

        hull_center = hull_data %>%
                dplyr::group_by(poss) %>%
                summarise(xmean = mean(avg_x), ymean = mean(avg_y))


        # direction of play evaluation
        direction = get_direction_of_play(avg_pos)

        if(direction == "right_to_left"){

                hull_data = hull_data %>%
                        mutate(avg_x = pitch_long - avg_x,
                               avg_y = pitch_width - avg_y)

                avg_pos_wide = avg_pos_wide %>%
                        mutate(avg_x_OFF = pitch_long - avg_x_OFF,
                               avg_x_ON = pitch_long - avg_x_ON,
                               avg_y_OFF = pitch_width - avg_y_OFF,
                               avg_y_ON = pitch_width - avg_y_ON)

                avg_pos = avg_pos %>%
                        mutate(avg_x = pitch_long - avg_x,
                               avg_y = pitch_width - avg_y)

        }

        # get some stats
        stats = get_team_stats(hull_data)

        spread = avg_pos %>%
                dplyr::filter(is_gk == F) %>%
                left_join(stats %>% select(poss, centroid_x, centroid_y, team), by = c("poss", "team")) %>%
                mutate(distance = sqrt((centroid_x - avg_x)^2 + (centroid_y - avg_y)^2)) %>%
                group_by(team, poss) %>%
                summarise(spread = sd(distance))

        stats = stats %>%
                left_join(spread, by = c("team", "poss"))

        p = sp +
                geom_polygon(data = hull_data,
                             aes(x = avg_x, y = avg_y, fill = factor(poss)),
                             alpha=0.4, inherit.aes = T)

        p <- p +
                geom_segment(data = avg_pos_wide,
                             aes(x = avg_x_OFF, y = avg_y_OFF,
                                 xend = avg_x_ON, yend = avg_y_ON),
                             size = 0.5, linetype = 2,
                             arrow = arrow(length = unit(0.2,"cm"), type = "closed")) +
                geom_point(data = avg_pos,
                           aes(x = avg_x, y = avg_y, fill = factor(poss)),
                           col = team_col, shape = 21, stroke = 1, alpha = 0.8, size = 6,
                           inherit.aes = T) +
                geom_text(data = avg_pos, aes(label = player, x = avg_x, y = avg_y, col = poss), size = 3,
                          inherit.aes = T) +
                theme(legend.position = "none",
                      plot.background = element_rect(fill = "black"),
                      text = element_text(color = "grey60")) +
                scale_fill_manual(values = c(off_ball_col, on_ball_col)) +
                scale_colour_manual(values = c("white", "black"))

        p = p + annotate("segment", x = pitch_long/2 + -10, xend = pitch_long/2 + 10, y = -3.5, yend = -3.5,
                         arrow = arrow(type = "closed", length = unit(0.07, "inches")), col = pitch_lines_col) +
                annotate("text", x = pitch_long/2, y = -1.5, label = "Direction of play", col = pitch_lines_col)


        if(annotation == "depth"){

                stats_off = stats %>% filter(poss == "OFF")
                stats_on = stats %>% filter(poss == "ON")

                p +
                        geom_segment(aes(x = stats_off$last_def_x_pos, y = 0, xend = stats_off$last_def_x_pos, yend = pitch_width),
                                     size = 0.4, col = off_ball_col, linetype = 2) +
                        geom_segment(aes(x = stats_on$last_def_x_pos, y = 0, xend = stats_on$last_def_x_pos, yend = pitch_width),
                                     size = 0.2, col = on_ball_col, linetype = 2) +
                        geom_segment(aes(x = stats_off$high_forw_x_pos, y = 0, xend = stats_off$high_forw_x_pos, yend = pitch_width),
                                     size = 0.4, col = off_ball_col, linetype = 2) +
                        geom_segment(aes(x = stats_on$high_forw_x_pos, y = 0, xend = stats_on$high_forw_x_pos, yend = pitch_width),
                                     size = 0.2, col = on_ball_col, linetype = 2)

        }else{p}

        return(list(graph = p, stats = stats))

}



#'
#'
#' @param ed
#' @param team target team
#'
#' @return frames when the target team was ON and OFF possession
#' @import dplyr
#' @importFrom janitor clean_names
#'
#' @noRd
#' @keywords internal
poss_frames = function(ed, team = c("Home", "Away")){

        target_team = match.arg(team)

        poss_start_events = c('PASS', 'RECOVERY', 'SET PIECE', 'SHOT')
        poss_change_events = c("BALL LOST", "BALL OUT", "SHOT", "PASS")

        DELAY_FIX = 0.1

        poss = ed %>%
                filter(!Type %in% c("CHALLENGE", "CARD")) %>%
                janitor::clean_names() %>%
                arrange(period, start_time_s) %>%
                mutate(inverted_time_ball_recovery = ifelse(type == "BALL LOST" & lag(type) == "RECOVERY" & team != lag(team), 1, 0),
                       start_time_s = ifelse(inverted_time_ball_recovery == 1, lag(start_time_s) - DELAY_FIX, start_time_s)) %>%
                arrange(period, start_time_s) %>%
                mutate(wrong_ball_lost = ifelse(type == "BALL LOST" & subtype == "THEFT" &
                                                        ((team != lead(team) & team != lag(team)) |
                                                                 (team == lead(team) & lead(type) == "RECOVERY")), 1, 0),
                       wrong_recover = ifelse((type == "RECOVERY" & lead(type) == "BALL LOST" & lag(type) == "FAULT RECEIVED" &
                                                       team != lag(team) & team == lead(team)) |
                                                      (lag(type) == "RECOVERY" & type == "BALL LOST" & lag(type, 2) == "FAULT RECEIVED" &
                                                               team != lag(team, 2) & team == lag(team)), 1, 0)) %>%
                filter(wrong_ball_lost != 1) %>%
                filter(wrong_recover != 1) %>%
                mutate(wrong_ball_out = ifelse(type == "BALL OUT" & team != lead(team) & team != lag(team), 1, 0)) %>%
                filter(wrong_ball_out != 1) %>%
                mutate(other_ball_lost_to_remove = ifelse(team != lag(team) & team == lead(team) & type == "BALL LOST" & lead(type) == "SET PIECE", 1, 0)) %>%
                filter(other_ball_lost_to_remove != 1) %>%
                dplyr::mutate(poss_start = ifelse((team != lag(team) &
                                                           type %in% poss_start_events &
                                                           (lag(type) %in% c(poss_change_events, "RECOVERY") | lag(subtype) %in% poss_change_events)) |
                                                          (subtype == "KICK OFF" & !is.na(subtype)) &
                                                          !(type == "RECOVERY" & lag(team) == lead(team) & team != lead(team)), 1, 0),
                              poss_end = ifelse(team != lead(team) & lead(type) %in% poss_start_events &
                                                        (type %in% c(poss_change_events, "RECOVERY") | subtype %in% (poss_change_events)), 1, 0),
                              poss_start = ifelse(lag(type) == "BALL OUT" & !is.na(lag(type)), 1, poss_start),
                              poss_end = ifelse(type == "BALL OUT", 1, poss_end)) %>%
                dplyr::select(c(team, type, subtype, period, start_time = start_time_s, end_time = end_time_s,
                                poss_start, poss_end, start_frame, end_frame)) %>%
                mutate(poss_end = ifelse(poss_end == 0 & team != lead(team) & !is.na(lead(team)), 1, poss_end),
                       poss_start = ifelse(poss_start == 0 & team != lag(team) & !is.na(lag(team)), 1, poss_start),
                       poss_end = ifelse(poss_end == 1 & team == lead(team) & !is.na(lead(team)), 0, poss_end),
                       poss_start = ifelse(poss_start == 1 & team == lag(team) & !is.na(lag(team)), 0, poss_start),
                       frame = ifelse(type == "BALL OUT", start_frame, end_frame))

        unique_frame_cases = poss %>%
                filter(poss_start == 1 & poss_end == 1 & team == target_team) %>%
                mutate(frame = start_frame,
                       poss_end = 0,
                       start_time = start_time - DELAY_FIX/100)

        poss_processed = poss %>%
                mutate(frame = ifelse(poss_start == 1 & poss_end == 1, end_frame, frame),
                       poss_start = ifelse(poss_start == 1 & poss_end == 1, 0, poss_start)) %>%
                bind_rows(unique_frame_cases) %>%
                arrange(start_time)

        output = tibble(team = target_team,
                        poss_start = poss_processed %>% filter(team == target_team & poss_start == 1) %>% pull("frame"),
                        poss_end = poss_processed %>% filter(team == target_team & poss_end == 1) %>% pull("frame"))

        output

}



#'
#'
#' @param frames_start
#' @param frames_end
#' @param data
#' @param target_team
#'
#' @return
#' @import dplyr
#'
#' @noRd
#' @keywords internal
get_poss_data = function(frames_start, frames_end, data, target_team){

        map2_df(frames_start, frames_end, ~(data %>%
                                           filter(team == tolower(target_team)) %>%
                                           filter(between(frame, .x, .y))))
}




#'
#'
#' @param avg_pos
#' @param pitch_long
#'
#' @return
#' @import dplyr
#'
#' @noRd
#' @keywords internal
get_direction_of_play = function(avg_pos, pitch_long = 105){

        out = ifelse(avg_pos %>% filter(is_gk == TRUE & poss == "OFF") %>% pull("avg_x") > (pitch_long/2), "right_to_left", "left_to_right")
        return(out)
}




#'
#'
#' @param hull_data
#'
#' @return
#' @import dplyr
#'
#' @noRd
#' @keywords internal
get_team_stats = function(hull_data){

        stats = hull_data %>%
                group_by(team, poss) %>%
                summarise(last_def_x_pos = round(min(avg_x), 1),
                          high_forw_x_pos = round(max(avg_x), 1),
                          depth = round(max(avg_x) - min(avg_x), 1),
                          amplitude = round(max(avg_y) - min(avg_y), 1),
                          area = round(get_specific_stat(avg_x, avg_y, "area"), 1),
                          centroid_x = round(get_specific_stat(avg_x, avg_y)[1], 1),
                          centroid_y = round(get_specific_stat(avg_x, avg_y)[2], 1))

}




#'
#'
#' @param avg_x
#' @param avg_y
#' @param stat
#'
#' @return
#' @import dplyr
#' @importFrom deldir deldir
#' @importFrom grDevices xy.coords
#'
#' @noRd
#' @keywords internal
get_specific_stat = function(avg_x, avg_y, stat = c("centroid", "area")){

        dd = deldir::deldir(xy.coords(avg_x, avg_y))
        stat = match.arg(stat)

        if(stat == "centroid"){

                out = with(dd$summary, sapply(list(x, y), weighted.mean, del.wts))
        }

        if(stat == "area"){

                out = dd$del.area
        }

        return(out)
}



#'
#'
#' @param avg_x
#' @param avg_y
#' @param stat
#'
#' @return
#' @import png
#' @import gt
#' @importFrom grid rasterGrob
#'
#' @noRd
#' @keywords internal
gt_grob <- function(gt_object, ...){

        out_name = file.path(tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".png"))

        gtsave(gt_object, out_name, ...)

        in_png = png::readPNG(out_name)

        on.exit(file.remove(out_name), add=TRUE)

        grid::rasterGrob(in_png)

}

