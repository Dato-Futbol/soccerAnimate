% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/soccer_animate.R
\name{soccer_animate}
\alias{soccer_animate}
\title{Creates 2D soccer animations}
\usage{
soccer_animate(
  tidy_data,
  ini_time,
  end_time,
  method = "base",
  pitch_long = 105,
  pitch_width = 68,
  pitch_fill = "#74a9cf",
  pitch_lines_col = "lightgrey",
  home_team_col = "white",
  away_team_col = "#dd3497",
  title = "",
  subtitle = "",
  provider = "Metrica",
  show_anim = T,
  export_gif = F,
  gif_name = "animation"
)
}
\arguments{
\item{tidy_data}{the processed dataframe ready to do visualizations. It could be obtained using either get_tidy_data() or process_catapult() functions.}

\item{ini_time}{the time in seconds of tracking data to consider as initial time of the animation}

\item{end_time}{the time in seconds of tracking data to consider as ending time of the animation}

\item{method}{four different approaches to visualize: base (default), convexhull, voronoi, delaunay}

\item{pitch_long}{long of the pitch in meters}

\item{pitch_width}{width of the pitch in meters}

\item{pitch_fill}{colour used to fill the pitch}

\item{pitch_lines_col}{colour used for lines of the pitch}

\item{home_team_col}{colour used to fill the players of the home team}

\item{away_team_col}{colour used to fill the players of the away team}

\item{title}{graph title}

\item{subtitle}{graph subtitle}

\item{provider}{set the name of the tracking data provider which defines the data format}

\item{show_anim}{should the animation be shown on the viewer? (T/F)}

\item{export_gif}{should the animation be exported as gif file? (T/F)}

\item{gif_name}{the name of the exported gif file}
}
\value{
animated gif on viewer and/or as output file
}
\description{
Creates 2D animations using soccer tracking data allowing you export it as gif file
}
