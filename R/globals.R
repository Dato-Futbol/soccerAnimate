utils::globalVariables(c(".", "period", "frame", "x", "y", "player", "time",
                         "event_data", "type", "subtype", "event",
                         "second", "team", "is_gk"))

LONG_FORMAT_COLUMNS = c("period", "frame", "time_s", "player", "x", "y", "team", "is_gk")
MS_DT = 0.04 # Metrica Sports delta time (frequency sample)
MS_LAG_SMOOTH = 20 # n° samples as smoothing windows
PLAYER_MAX_SPEED = 12 #[m/s]
