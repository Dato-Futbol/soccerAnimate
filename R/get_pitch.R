#' Draws a soccer pitch as a ggplot background layer
#'
#' @param pitch_fill colour used to fill the pitch
#' @param pitch_col colour used for lines of the pitch
#'
#' @return ggplot object
#' @import ggplot2
#' @importFrom ggforce geom_circle
#' @importFrom ggforce geom_arc
#' @NoRd
#' @keywords internal
#' @export
#'

get_pitch <- function(pitch_fill = "#74a9cf", pitch_col = "lightgrey"){

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
                #ggtitle("Soccer pitch", "with ggplot2") +
                geom_rect(aes(xmin = 0, xmax = 105, ymin = 0, ymax = 68), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 0, xmax = 16.5, ymin = 13.85, ymax = 54.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 88.5, xmax = 105, ymin = 13.85, ymax = 54.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 24.85, ymax = 43.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = 99.5, xmax = 105, ymin = 24.85, ymax = 43.15), fill = pitch_fill, col = pitch_col) +
                geom_rect(aes(xmin = -2, xmax = 0, ymin = 30.34, ymax = 37.66), fill = "transparent", col = pitch_col) +
                geom_rect(aes(xmin = 105, xmax = 107, ymin = 30.34, ymax = 37.66), fill = "transparent", col = pitch_col) +
                geom_circle(aes(x0 = 52.5, y0 = 34, r = 9.15), fill = pitch_fill, col = pitch_col) +
                geom_segment(aes(x = 52.5, xend = 52.5, y = 0, yend = 68), col = pitch_col) +
                geom_point(aes(x = 11, y = 34), col = pitch_col) +
                geom_point(aes(x = 94, y = 34), col = pitch_col) +
                geom_point(aes(x = 52.5, y = 34), col = pitch_col) +
                geom_arc(aes(x0 = 11, y0 = 34, r = 9.15, start = 37*pi/180, end = 143*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 94, y0 = 34, r = 9.15, start = -37*pi/180, end = -143*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0, end = 90*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 0, y0 = 68, r = 1, start = 90*pi/180, end = 180*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 105, y0 = 68, r = 1, start = 180*pi/180, end = 270*pi/180), col = pitch_col) +
                geom_arc(aes(x0 = 105, y0 = 0, r = 1, start = 270*pi/180, end = 360*pi/180), col = pitch_col)
}
