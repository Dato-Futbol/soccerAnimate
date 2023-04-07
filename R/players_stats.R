#' Calculate multiple players stats based on tracking data.
#' It includes Minutes played, distance, average/max speed in addition to distance
#' splitted by different speed ranges.
#'
#' @param data dataframe with the already processed tracking data in long format
#' @param speed_ranges character to set what kind of speed ranges has to be considered:
#' a) "laurie": walking, jogging, running, sprinting;
#' b) "perf": low speed activity, high speed running, very high speed running and sprinting
#'
#' @return dataframe with the results
#' @import dplyr
#' @importFrom magrittr %>%
#' @export
#'
players_stats <- function(data, speed_ranges = c("laurie", "perf")){

        ranges = match.arg(speed_ranges)

        if(ranges == "laurie"){

                T1 = WALK_JOG_THRESHOLD
                T2 = JOG_RUN_THRESHOLD
                T3 = RUN_SPRINT_THRESHOLD
                range_names = c("walking_km", "jogging_km", "running_km", "sprinting_km")

        }

        if(ranges == "perf"){

                T1 = LOW_HIGH_SPEED_THRESHOLD
                T2 = HIGH_VERY_HIGH_SPEED_THRESHOLD
                T3 = VERY_HIGH_SPEED_SPRINT_THRESHOLD
                range_names = c("low_speed_act_km", "high_speed_run_km", "very_high_speed_run_km", "sprinting_km")
        }

        output = data %>%
                filter(team != "ball") %>%
                group_by(team, player, is_gk) %>%
                summarise(n_samples = n(),
                          distance_km = sum(v_mod/MS_LAG_SMOOTH, na.rm = T)/1000,
                          avg_speed_m_s = mean(speed, na.rm = T),
                          !!range_names[1] := sum(ifelse(speed < T1, v_mod, 0), na.rm = T)/MS_LAG_SMOOTH/1000,
                          !!range_names[2] := sum(ifelse(speed >= T1 & speed < T2, v_mod, 0), na.rm = T)/MS_LAG_SMOOTH/1000,
                          !!range_names[3] := sum(ifelse(speed >= T2 & speed < T3, v_mod, 0), na.rm = T)/MS_LAG_SMOOTH/1000,
                          !!range_names[4] := sum(ifelse(speed >= T3, v_mod, 0), na.rm = T)/MS_LAG_SMOOTH/1000) %>%
                mutate(minutes_played = n_samples*MS_DT/60,
                       avg_speed_km_h = avg_speed_m_s/1000*3600) %>%
                arrange(team, desc(minutes_played))


        output %>%
        select(team, player, minutes_played, distance_km, avg_speed_m_s, avg_speed_km_h, range_names, is_gk)

}
