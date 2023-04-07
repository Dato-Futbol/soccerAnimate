utils::globalVariables(c(".", "period", "frame", "x", "y", "player", "time",
                         "event_data", "type", "subtype", "event",
                         "second", "team", "is_gk"))

LONG_FORMAT_COLUMNS = c("period", "frame", "time_s", "player", "x", "y", "team", "is_gk")
MS_DT = 0.04 # Metrica Sports delta time
MS_LAG_SMOOTH = 20 # n° samples as smoothing windows
PLAYER_MAX_SPEED = 12 #[m/s]

WALK_JOG_THRESHOLD = 2
JOG_RUN_THRESHOLD = 4
RUN_SPRINT_THRESHOLD = 7

LOW_HIGH_SPEED_THRESHOLD = 4
HIGH_VERY_HIGH_SPEED_THRESHOLD = 5.5
VERY_HIGH_SPEED_SPRINT_THRESHOLD = 7

SPRINTS_WINDOW = 1/MS_DT
