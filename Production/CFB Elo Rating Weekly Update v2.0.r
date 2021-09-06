# Libraries and Themes ----------------------------------------------------
rm(list = ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(jsonlite)
library(stringr)
library(lubridate)
library(gt)
library(webshot)
library(data.table)
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")

# New Season Regression Factor
regress <- (.95)
# k-factor
k <- 70
# home-field advantage (in elo points)
home_field_advantage <- 55
# Conference adjustors
g5 <- 1200
d3 <- 900
neutral_adjust <- 0

# Expected Score and Updated Elo Rating Functions -------------------------

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

calc_new_elo_rating <- function(team_rating, actual_score, expected_score, k){
  return(team_rating + k * (actual_score - expected_score))
}

# Pull in Games Data ------------------------------------------------------

base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data

games.master = tibble()
for (j in 2021) {
  for (i in 1:20) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    games <- cfbd_api(full_url_games, my_key)
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

#Select variables we want
cfb_games <- games.master %>% select(id, season, week, season_type, home_team, home_conference, away_team, away_conference, home_points, away_points, start_date, neutral_site) %>% 
  mutate(date=ymd_hms(start_date)) %>%
  select(-start_date)

# Get Week 1 epiweek for referencing later weeks
week_1_epiweek <- cfb_games %>% filter(week == min(week)) %>% slice_min(date) %>% pull(date) %>% unique() %>% epiweek() %>% as.integer()

# Add game outcome for home team
cfb_games <- cfb_games %>% mutate(game_outcome_home = 
                case_when(
                  home_points > away_points ~ 1,
                  home_points < away_points ~ 0,
                  TRUE ~ 0.5
)
)

# Figure out what week the first week of postseason play should be
postseason_start_week <- cfb_games %>% filter(season_type == "regular", week == max(week)) %>% 
  pull(week) %>% unique() %>% as.integer() + 1L

postseason_start_epiweek <- cfb_games %>% filter(season_type == "postseason") %>% slice_min(date) %>% 
  pull(date) %>% unique() %>% epiweek() %>% as.integer()

postseason_start_date <- cfb_games %>% filter(season_type == "postseason") %>% slice_min(date) %>% 
  pull(date) %>% unique()

if(is_empty(postseason_start_epiweek) == F){
### Adjust postseason games to correct week
cfb_games <- cfb_games %>%
  mutate(week = if_else(season_type == "postseason", as.integer(postseason_start_week + difftime(date, postseason_start_date, units = "weeks") %>% floor() %>% as.integer()), as.integer(week))) 
}
# The only problem with this solution is there could be some gap weeks if there happens
# to be more than 1 epiweek between postseason games.if that ends up being the case, 
# we can re-mutate week as a "rank" of weeks, to ensure no gaps

# Predict Upcoming Week Outcomes ------------------------------------------
upcoming.games <- tibble(cfb_games)
# Save a version of this year's games for later
lastweek.games <- upcoming.games
# Read in historic Elo ratings
elo_ratings <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv",
                     encoding = "UTF-8")
                        
elo_conf <- elo_ratings %>% 
  mutate(conference_class = case_when(conference %in% power_5 ~ 1500,
                                    conference %in% group_of_5 ~ g5,
                                    conference %in% "FBS Independents" ~ 1500,
                                    TRUE ~ d3))

# Regress ratings if it's a new season
if (now()-max(elo_ratings$date) > 90){
  preseason_elo <- elo_conf %>% group_by(team) %>% 
    slice(which.max(date)) %>% 
    mutate(elo_rating = elo_rating*(regress)+conference_class*(1-regress),
           conference = conference,
           week = 0,
           season=j,
           date=ymd_hms(paste0(lubridate::year(today()),"-08-15 00:00:00"))) %>% 
    select(-conference_class)
  elo_ratings <- elo_ratings %>% 
    bind_rows(preseason_elo)
  fwrite(preseason_elo, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/Production/elo_ratings_historic.csv", append = TRUE, col.names = FALSE)
}

# Filter only games that haven't been rated yet
elo_max_date <- elo_ratings$date %>% max()
games_to_rate <- upcoming.games %>% filter(date >= elo_max_date+hours(1)) %>% filter(is.na(home_points) == F & is.na(away_points) == F)
game_weeks <- games_to_rate$date %>% as.Date() %>% unique()

# Determine whether to run update -----------------------------------------

# Get latest date of games in elo table
elo_last_update <- data.table::fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv", encoding = "UTF-8") %>% 
  dplyr::slice_max(order_by = date, n = 1) %>% 
  dplyr::select(week, season, date) %>% 
  unique()

# Get all games after the latest datetime in Elo, then take earliest week
games_update <- games.master %>% 
  filter(start_date > elo_last_update$date,
         week == min(week))

# If there are any games incomplete (home or away points == NA), do  not update)
if(any(is.na(games_update$home_points)) | any(is.na(games_update$away_points))){
  
  message("Week not complete yet, aborting update.")
  
} else {

if(is_empty(game_weeks) == F){
for(i_date in 1:length(game_weeks)){
  message("Game Day ", game_weeks[i_date])

# Reread clean upcoming.games
upcoming.games <- tibble(cfb_games)

# Get most updated rating for each team
current_elo_ratings <- elo_ratings %>% group_by(team) %>% slice_max(order_by = date, n = 1)

# Take just team and rating
current_elo_ratings_only <- current_elo_ratings %>% select(team, elo_rating)

# Get start of season elo for each team only
preseason_elo_ratings <- elo_ratings %>% group_by(team) %>% slice_max(order_by = season, n = 1) %>% 
  filter(week == 0) %>% 
  select(team, elo_rating)

# Mutate week of elo_ratings for joining purposes
elo_ratings_tmp <- elo_ratings %>% 
  mutate(week = week + 1)

# If there are two games played in one week, take only the most recent rating
elo_ratings_tmp <- elo_ratings_tmp %>% 
  filter(season == j) %>% 
  group_by(team, season, week) %>% 
  slice_max(order_by = date, n = 1)

# Join cfb games with elo ratings for home and away teams by team name and date of rating/game
upcoming.games <- left_join(games_to_rate, elo_ratings_tmp, by = c("home_team" = "team", "week", "season")) %>% 
  rename(home_elo = elo_rating) %>% 
  rename(home_elo_date = date.y) %>% 
  rename(game_date = date.x)

upcoming.games <- left_join(upcoming.games, elo_ratings_tmp, by = c("away_team" = "team", "week", "season")) %>% 
  rename(away_elo = elo_rating) %>% 
  rename(away_elo_date = date)

upcoming.tmp <- upcoming.games %>% # Get most recent Elo Rating
  left_join(current_elo_ratings_only, by = c("home_team" = "team")) %>% 
  rename(most_recent_elo_home = elo_rating) %>% 
  mutate(most_recent_elo_home = case_when(is.na(most_recent_elo_home) == F ~ most_recent_elo_home,
                                          is.na(most_recent_elo_home) == T ~ case_when(home_conference %in% power_5 ~ 1500,
                                                                                       home_conference %in% group_of_5 ~ g5,
                                                                                       TRUE ~ d3))) %>% 
  left_join(current_elo_ratings_only, by = c("away_team" = "team")) %>% 
  rename(most_recent_elo_away = elo_rating) %>% 
  mutate(most_recent_elo_away = case_when(is.na(most_recent_elo_away) == F ~ most_recent_elo_away,
                                          is.na(most_recent_elo_away) == T ~ case_when(away_conference %in% power_5 ~ 1500,
                                                                                       away_conference %in% group_of_5 ~ g5,
                                                                                       TRUE ~ d3)))

# Calculate which game in the season this is for each team (game 1, game 2... etc.)
game_of_season <- cfb_games %>% 
  pivot_longer(cols = c(home_team,away_team), names_to = "home_away", values_to = "team") %>% 
  group_by(team) %>% mutate(game_of_season = rank(date)) %>% 
  select(id, team, game_of_season)

# Join with upcoming games to help decide what elo to use later
upcoming.tmp2 <- upcoming.tmp %>% 
  left_join(game_of_season, by = c("id", "home_team" = "team")) %>% 
  rename(home_game_of_season = game_of_season) %>% 
  left_join(game_of_season, by = c("id", "away_team" = "team")) %>% 
  rename(away_game_of_season = game_of_season)

# Join with season starting elo
upcoming.tmp3 <- upcoming.tmp2 %>% 
  left_join(preseason_elo_ratings, by = c("home_team" = "team")) %>% 
  rename(home_preseason_elo = elo_rating) %>% 
  mutate(home_preseason_elo = case_when(is.na(home_preseason_elo) == F ~ home_preseason_elo,
                                        is.na(home_preseason_elo) == T ~ case_when(home_conference %in% power_5 ~ 1500,
                                                                                   home_conference %in% group_of_5 ~ g5,
                                                                                   TRUE ~ d3))) %>%
  left_join(preseason_elo_ratings, by = c("away_team" = "team")) %>% 
  rename(away_preseason_elo = elo_rating) %>% 
  mutate(away_preseason_elo = case_when(is.na(away_preseason_elo) == F ~ away_preseason_elo,
                                        is.na(away_preseason_elo) == T ~ case_when(away_conference %in% power_5 ~ 1500,
                                                                                   away_conference %in% group_of_5 ~ g5,
                                                                                   TRUE ~ d3)))

upcoming.tmp4 <- upcoming.tmp3 %>% # Use most recent elo rating for current/future games. If they don't have one, use the means by conference.
  mutate(home_elo = case_when(home_game_of_season == 1 & is.na(home_preseason_elo) == F ~ home_preseason_elo,
                              is.na(most_recent_elo_home) == F & is.na(home_elo) == T ~ most_recent_elo_home,
                              is.na(most_recent_elo_home) == T & is.na(home_elo) == T ~ case_when(home_conference %in% power_5 ~ 1500,
                                                                                                  home_conference %in% group_of_5 ~ g5,
                                                                                                  TRUE ~ d3),
                              TRUE ~ home_elo),
         home_elo_date = case_when(is.na(home_elo_date) == T ~ {elo_ratings %>% filter(season == max(season)) %>% summarise(min_date = min(date)) %>% pull(min_date)},
                                   is.na(home_elo_date) == F ~ home_elo_date)) %>% 
  mutate(away_elo = case_when(away_game_of_season == 1 & is.na(away_preseason_elo) == F ~ away_preseason_elo,
                              is.na(most_recent_elo_away) == F & is.na(away_elo) == T ~ most_recent_elo_away,
                              is.na(most_recent_elo_away) == T & is.na(away_elo) == T ~ case_when(away_conference %in% power_5 ~ 1500,
                                                                                                  away_conference %in% group_of_5 ~ g5,
                                                                                                  TRUE ~ d3),
                              TRUE ~ away_elo),
         away_elo_date = case_when(is.na(away_elo_date) == T ~ {elo_ratings %>% filter(season == max(season)) %>% summarise(min_date = min(date)) %>% pull(min_date)},
                                   is.na(away_elo_date) == F ~ away_elo_date))

# Get win prob
upcoming.games <- upcoming.tmp4 %>% 
  mutate(home_pred_win_prob = calc_expected_score(home_elo+if_else(neutral_site == F, home_field_advantage, neutral_adjust), away_elo), away_pred_win_prob = 1 - home_pred_win_prob)

rm(list = c("upcoming.tmp", "upcoming.tmp2", "upcoming.tmp3", "upcoming.tmp4"))
# Pull in Last Week Results and Update Elo --------------------------------

# Rename games table
cfb_games <- lastweek.games

# Calculate max week in elo so you can update the next week
week_of_elo_last_updated <- elo_ratings %>% filter(season == max(season)) %>% slice_max(order_by = week, n = 1) %>% pull(week) %>% unique()
week_of_games_just_played <- week_of_elo_last_updated + 1L
week_of_upcoming_games <- week_of_games_just_played + 1L

### Start calculation for the week
current_week <- upcoming.games %>% filter(as.Date(game_date) == game_weeks[i_date])

# only run if it's not preseason, and make sure the results are not calculated twice (week num of current week where games have been played doesn't equal the max week already in the elo_historic github csv file)
if(week_of_games_just_played > 0){# & !any(current_week %>% filter(!(is.na(home_points) & is.na(away_points))) %>% pull(week) == elo_ratings %>% filter(season == max(season)) %>% filter(week == max(week)) %>% pull(week) %>% unique())){ #& (length(current_week$home_points) != length(is.na(current_week$home_points)))){

#calculate new ratings after game
current_week <- current_week %>% mutate(new_home_rating = calc_new_elo_rating(home_elo, game_outcome_home, calc_expected_score((home_elo+if_else(neutral_site == F, home_field_advantage, neutral_adjust)), away_elo),k),
                                        new_away_rating = calc_new_elo_rating(away_elo, 1-game_outcome_home, calc_expected_score(away_elo, (home_elo+if_else(neutral_site == F, home_field_advantage, neutral_adjust))),k))

#keep track of predictions and actual results
k_optimization_temp <- current_week %>% mutate(HomeExpectedWin=calc_expected_score((home_elo+if_else(neutral_site == F, home_field_advantage, neutral_adjust)), away_elo)) %>% 
  select(game_outcome_home, HomeExpectedWin) %>% 
  rename(HomeWin = game_outcome_home) %>% 
  mutate(Year=j,k_val = k,
         regress_val = regress,
         home_field_val = home_field_advantage)

# k_optimization <- k_optimization %>% bind_rows(k_optimization_temp)

## Update Elo Table
# Only update if the game was played (there are home points and away points) by filtering out those games before updating
current_week <- current_week %>% filter(!is.na(home_points | away_points))

#home team elo update
updated_ratings_home <- current_week %>% select(home_team, home_conference, new_home_rating, week, season, game_date) %>% 
  rename(team=home_team) %>% 
  rename(conference = home_conference) %>% 
  rename(elo_rating = new_home_rating) %>% 
  rename(season = season) %>% 
  mutate(game_date = game_date) %>% 
  rename(date = game_date)

#away team elo update
updated_ratings_away <- current_week %>% select(away_team, away_conference, new_away_rating, week, season, game_date) %>% 
  rename(team=away_team) %>% 
  rename(conference = away_conference) %>% 
  rename(elo_rating = new_away_rating) %>% 
  rename(season = season) %>% 
  mutate(game_date = game_date) %>% 
  rename(date = game_date)

# Save updated home and away elo ratings for the completed week
elo_ratings_updated <- updated_ratings_home %>% 
  bind_rows(updated_ratings_away)

# Update elo ratings internally
elo_ratings <- elo_ratings %>% 
  bind_rows(updated_ratings_home) %>% 
  bind_rows(updated_ratings_away)

# Write new data to github
fwrite(elo_ratings_updated, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/Production/elo_ratings_historic.csv", append = TRUE, col.names = FALSE)
}
}
}
}
