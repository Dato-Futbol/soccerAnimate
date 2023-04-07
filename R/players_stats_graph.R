#' Create a graph to compare players stats for a specific team
#'
#' @param data dataframe with the players stats
#' @param team team to graph "home" or "away"
#' @param var variable to graph, default value = "ranges" for speed ranges
#' @param export_gif should the graph be exported as PNG file? (T/F)
#'
#' @return dataframe with the results
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom frocats fct_reorder
#' @export
#'
players_stats_graph <- function(data, team = c("home", "away"), var = "ranges", export_png = F){

        target_team = match.arg(team)

        data = data %>% filter(team == target_team)

        if(var != "ranges"){

                p = ggplot(data, aes(x = fct_reorder(player, get(var)), y = get(var))) +
                        geom_bar(stat = "identity") +
                        geom_text(aes(label = round(get(var), 1)), hjust = 1.5, col = "white") +
                        coord_flip() +
                        scale_y_continuous(breaks = seq(0, max(data[var]) +1, 1)) +
                        theme(plot.margin = margin(1, 0.5, 0.5, 0.5, unit = "cm")) +
                        labs(x = "Player\n", y = paste0("\n", gsub("_", " ", var)),
                             title = paste0("Player rank based on ", gsub("_", " ", var), " - ", target_team, " team"))

                if(export_png){
                        ggsave(filename = paste0(target_team, "_team_player_rank_", var, ".png"), plot = p, width = 12, height = 9)
                }

        }

        if(var == "ranges"){

                if(sum(str_detect(names(data), "walking"))){
                        range_names = c("walking_km", "jogging_km", "running_km", "sprinting_km")
                } else {range_names = c("low_speed_act_km", "high_speed_run_km", "very_high_speed_run_km", "sprinting_km")}

                data = data %>% pivot_longer(cols = range_names)
                data$name = factor(data$name, levels = range_names)
                data$player = paste("#", data$player, "(", round(data$minutes_played, 0), ")")

                p = ggplot(data, aes(x = fct_reorder(player, distance_km), y = value, fill = name)) +
                    geom_bar(stat = "identity", alpha = 0.8) +
                    coord_flip() +
                    scale_fill_manual(values = c("#118ab2", "#06d6a0", "#ffd166", "#ef476f")) +
                    scale_y_continuous(breaks = seq(0, max(data$distance_km) +1, 1)) +
                    theme_bw() +
                    theme(plot.margin = margin(1, 0.5, 0.5, 0.5, unit = "cm")) +
                    labs(x = "Player\n", y = "\nDistance [km]", fill = "Speed range",
                         title = paste0("# Player (minutes played) distance [km] in different speed ranges", " - ", target_team, " team"))

                if(export_png){
                        ggsave(filename = paste0(target_team, "_team_player_distance_by_speed_ranges.png"), plot = p, width = 12, height = 9)
                }

        }

        p
}
