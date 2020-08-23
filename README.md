# soccerAnimate
An R package to create 2D animations of soccer tracking data

## How to install it?
```
# install.packages("remotes")
remotes::install_github("Dato-Futbol/soccerAnimate")
```

## How to use it?

Version 0.1.0 (23-08-2020) allows you to do the following tasks:

### 1) To get and process the data
*get_tidy_data()* function reads, tidies and joins the rawdata of both the Home and Away teams.
Currently only data from the provider Metrica Sport is supported. Even you could download the open tracking/event data following [this link](https://github.com/metrica-sports/sample-data), it is also possible to get the processed data using the function *get_tidy_data()* with the URLs of rawdata directly like the example:

```
library(soccerAnimate)
home_team_file <- "https://raw.githubusercontent.com/metrica-sports/sample-data/master/data/Sample_Game_1/Sample_Game_1_RawTrackingData_Home_Team.csv"
away_team_file <- "https://raw.githubusercontent.com/metrica-sports/sample-data/master/data/Sample_Game_1/Sample_Game_1_RawTrackingData_Away_Team.csv"
td <- get_tidy_data(home_team_file, away_team_file)
```
(If you have another public dataset to share please let me know in order to adapt the code)

### 2) 


