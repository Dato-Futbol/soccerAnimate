% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soccer_plot.R
\name{soccer_plot}
\alias{soccer_plot}
\title{Creates a 2D static plot}
\usage{
soccer_plot(
  tidy_data,
  target_frame,
  method = "base",
  pitch_long = 105,
  pitch_width = 68,
  pitch_fill = "#74a9cf",
  pitch_lines_col = "lightgrey",
  home_team_col = "white",
  away_team_col = "#dd3497",
  provider = "Metrica",
  export_png = F,
  png_name = "plot",
  title = "",
  subtitle = ""
)
}
\arguments{
\item{tidy_data}{the processed dataframe ready to do visualizations. It could be obtained using either get_tidy_data() or process_catapult() functions.}

\item{target_frame}{the unique frame of the tracking data to visualize}

\item{method}{four different approaches to visualize: base, convexhull, voronoi, delaunay}

\item{pitch_long}{long of the pitch in meters}

\item{pitch_width}{width of the pitch in meters}

\item{pitch_fill}{colour used to fill the pitch}

\item{pitch_lines_col}{colour used for lines of the pitch}

\item{home_team_col}{colour used to fill the players of the home team}

\item{away_team_col}{colour used to fill the players of the away team}

\item{provider}{set the name of the tracking data provider which defines the data format}

\item{export_png}{should the plot be exported as png file? (T/F)}

\item{png_name}{the name of the exported gif file}

\item{title}{graph title}

\item{subtitle}{graph subtitle}
}
\value{
soccer plot on Plots panel and/or as output file
}
\description{
Creates a 2D static plot of a unique frame from soccer tracking data
}
