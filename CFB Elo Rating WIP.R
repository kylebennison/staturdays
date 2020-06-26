# Libraries and Themes ----------------------------------------------------

library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(lubridate)

#Staturdays Colors

staturdays_col_list <- c(
  lightest_blue = "#5c6272",
  lighter_blue = "#4c5872",
  light_blue = "#394871",
  medium_blue = "#22345a",
  dark_blue = "#041e42",
  orange = "#de703b",
  sign = "#1e1e1e",
  white = "#FFFFFF"
)

staturdays_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (staturdays_col_list)
  
  staturdays_col_list[cols]
}

# Pull in Games Data ------------------------------------------------------

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data

games.master = data.frame()
for (j in 2000:2019) {
  for (i in 1:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

# Calculate Initial Elo Rating and set up table to store data
teams_elo_initial <- as_tibble(unique(c(unique(games.master$home_team),
                                        unique(games.master$away_team)))) %>% 
  mutate(elo_rating = 1500) %>% rename(team = value) %>% 
  mutate(week = 0, season = min(games.master$season), date = as.Date(ymd_hms(min(games.master$start_date))) - 7)

#Select variables we want
cfb_games <- games.master %>% select(id, season, week, season_type, home_team, away_team, home_points, away_points, start_date) %>% 
  mutate(date=ymd_hms(start_date)) %>%
  select(-start_date)

# Add game outcome for home team
cfb_games <- cfb_games %>% mutate(game_outcome_home = 
                case_when(
                  home_points > away_points ~ 1,
                  home_points < away_points ~ 0,
                  TRUE ~ 0.5
)
)

### Adjust postseason games to week 16
cfb_games <- cfb_games %>%
  mutate(week = if_else(season_type == "postseason", 16L, week))

# list of first playoff games each year, could left join cfb_games with this to create a new column to reference for calculating postseason week
postseason_start_date <- cfb_games %>% 
  filter(season_type == "postseason", id != 63847) %>% #filtered out that game since it was inaccurately marked as postseason and throws off the 2013 calculation
  group_by(season) %>% 
  slice_min(date) %>% 
  select(season, date)

temp_join <- left_join(cfb_games, postseason_start_date, by = "season")

temp_join <- unique(temp_join)

temp_join[,c("date.x", "date.y")] <- temp_join[,c("date.x", "date.y")] %>% lapply(as.Date) # Change datetime to just date (before it was counting seconds difference)

temp_join <- temp_join %>% 
  mutate(week = case_when(
    season_type == "postseason" ~ 16L + as.integer(floor((date.x - date.y)/7L)),
    TRUE ~ week)
  ) # This works now

cfb_games <- temp_join %>% select(-date.y) %>% rename(date = date.x)
rm(temp_join)

# Expected Score and Updated Elo Rating Functions -------------------------

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

calc_new_elo_rating <- function(team_rating, actual_score, expected_score, k=20){
  return(team_rating + k * (actual_score - expected_score))
}

# Update Elo Ratings each week --------------------------------------------

## All factors have been tested and optimized
# New Season Regression Factor
regress <- (.95)
# k-factor
k <- 85
# home-field advantage (in elo points)
home_field_advantage <- 55

# Make sure data is in the right order to run calculation by row
cfb_games <- cfb_games %>% 
  arrange(season, week, date)

#keep track of predictions 
k_optimization <- tibble(HomeWin=0, HomeExpectedWin=0, Year=0000, kval = k, regress_val = regress, home_field_val = home_field_advantage) 

# k=100 seems good .18, for regress - .176 for .9 (2010), and .179 (2000), test k again - .176 for 75 and 100, test home_field_adv - .176 for 50 and 65, 55 is min at .1758
for(regress in c(seq(.9, 1, by = 0.01))){
elo_ratings <- teams_elo_initial
message(paste0("Testing values: ", "hfa = ", home_field_advantage, " k =", k, " regress = ", regress))

#### updated for loop to speed up process ####
for(yr in c(2000:2019)){
  message(paste0("Calculating elo ratings for: "),yr)
  #regress Elo ratings before the first season of the year
  if(yr != min(cfb_games$season)){
  preseason_elo <- elo_ratings %>% group_by(team) %>% 
    slice(which.max(date)) %>% 
    mutate(elo_rating = elo_rating*(regress)+1500*(1-regress),
           week = 0,
           season=yr,
           date=ymd(paste0(yr,"-08-15")))
  elo_ratings <- elo_ratings %>% 
    bind_rows(preseason_elo)
  }
  
  for(wk in c(1:max(cfb_games[which(cfb_games$season == yr),]$week))){
    current_week <- cfb_games %>% filter(season==yr, week==wk)
    #if there are games that week
    if(nrow(current_week) != 0) {
      #bring in elo ratings
      current_elo_ratings <- elo_ratings %>% group_by(team) %>% 
        slice(which.max(date))
      current_week <- current_week %>% left_join(current_elo_ratings,
                                                 by=c("home_team"="team")) %>% 
        left_join(current_elo_ratings, by=c("away_team"="team")) %>% 
        rename(home_rating = elo_rating.x) %>% 
        rename(away_rating = elo_rating.y) %>% 
        rename(game_date = date.x) %>% 
        rename(home_rating_last_updated = date.y) %>%
        rename(away_rating_last_updated = date)
      
      #calculate new ratings after game
      current_week <- current_week %>% mutate(new_home_rating = calc_new_elo_rating(home_rating, game_outcome_home, calc_expected_score((home_rating+home_field_advantage), away_rating),k),
                                              new_away_rating = calc_new_elo_rating(away_rating, 1-game_outcome_home, calc_expected_score(away_rating, (home_rating+home_field_advantage)),k))
      
      #keep track of predictions and actual results
      k_optimization_temp <- current_week %>% mutate(HomeExpectedWin=calc_expected_score((home_rating+home_field_advantage), away_rating)) %>% 
        select(game_outcome_home, HomeExpectedWin) %>% 
        rename(HomeWin = game_outcome_home) %>% 
        mutate(Year=yr,k_val = k,
               regress_val = regress,
               home_field_val = home_field_advantage)
  
      k_optimization <- k_optimization %>% bind_rows(k_optimization_temp)
      
      #home team elo update
      updated_ratings_home <- current_week %>% select(home_team, new_home_rating, week.x, season.x, game_date) %>% 
        rename(team=home_team) %>% 
        rename(elo_rating = new_home_rating) %>% 
        rename(week = week.x) %>%
        rename(season = season.x) %>% 
        rename(date = game_date)
      
      #away team elo update
      updated_ratings_away <- current_week %>% select(away_team, new_away_rating, week.x, season.x, game_date) %>% 
        rename(team=away_team) %>% 
        rename(elo_rating = new_away_rating) %>% 
        rename(week = week.x) %>%
        rename(season = season.x) %>% 
        rename(date = game_date)
      
      elo_ratings <- elo_ratings %>% 
        bind_rows(updated_ratings_home) %>% 
        bind_rows(updated_ratings_away)
    }
    
  }
}
}

#Calculates the brier score
k_optimization %>% mutate(error=(HomeWin-HomeExpectedWin)^2) %>% 
  filter(Year>=2010) %>% 
  group_by(k_val, home_field_val, regress_val) %>% 
  summarise(e=mean(error)) %>% 
  View()

k_optimization %>% mutate(error=(HomeWin-HomeExpectedWin)^2) %>% 
  filter(Year>=2010) %>% 
  group_by(regress_val) %>% 
  summarise(e=mean(error)) %>% 
  ggplot(aes(x = regress_val, y = e)) +
  geom_line()

# Graphs ------------------------------------------------------------------

# Elo of the top 10 teams in average Elo all-time
elo_ratings %>% 
  filter(team %in% {elo_ratings %>% group_by(team) %>% summarise(avg_elo = mean(elo_rating)) %>% slice_max(order_by = avg_elo, n=10)}$team) %>% 
  ggplot(aes(date, elo_rating, colour = team)) +
  geom_line()

# Elo of Penn State
elo_ratings %>% 
  filter(team %in% "Penn State") %>% 
  ggplot(aes(date, elo_rating, colour = team)) +
  geom_line()

# Function that creates an Elo plot of two teams for a set date range
Elo_head_to_head <- function(team_a, team_b, start_season=min(elo_ratings$season), end_season=max(elo_ratings$season)){
  elo_ratings %>% 
    filter(team %in% c(team_a, team_b), season >= start_season & season <= end_season) %>% 
    ggplot(aes(date, elo_rating, colour = team)) +
    geom_line()
}

Elo_head_to_head("LSU", "Alabama", 2010, 2020)
