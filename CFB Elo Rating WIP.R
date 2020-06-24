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

#Calculates the brier score
#k_optimization %>% mutate(error=(errorXWin-errorXWin2)^2) %>% 
#  summarise(e=mean(error))

# Calculate Initial Elo Rating and set up table to store data
teams_elo_initial <- as_tibble(unique(c(unique(games.master$home_team),
                                        unique(games.master$away_team)))) %>% 
  mutate(elo_rating = 1500) %>% rename(team = value) %>% 
  mutate(week = 0, season = min(games.master$season), date = as.Date(ymd_hms(min(games.master$start_date))))

elo_ratings <- teams_elo_initial

# k_optimization <- tibble(errorXWin=0, errorXWin2=0) 

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

## Can use a for loop outside of this to test different k's and regressions, ex. for k in 1:40...
# New Season Regression Factor
regress <- (2/3)
# k-factor
k <- 20
# home-field advantage (in elo points)
home_field_advantage <- 65

# Make sure data is in the right order to run calculation by row
cfb_games <- cfb_games %>% 
  arrange(season, week, date)

# Loop calculation for each row
for(yr in 2000:2019){
  
  for(wk in 1:max(cfb_games[which(cfb_games$season == yr),]$week)){
    
    for(i in 1:nrow(cfb_games %>% filter(season == yr, week == wk))){
    
      cfb_games_week <- cfb_games %>% filter(season == yr, week == wk)
      if(nrow(cfb_games_week) != 0){ # Makes sure there were games that week (9/11/01), otherwise next if throws error
        # get teams
        home_team <- cfb_games_week$home_team[i]
        away_team <- cfb_games_week$away_team[i]
        # get actual scores
        home_score <- cfb_games_week$game_outcome_home[i]
        # get week
        game_week <- cfb_games_week$week[i]
        game_season <- cfb_games_week$season[i]
        game_date <- cfb_games_week$date[i]
        previous_game_date <- elo_ratings %>% 
          filter(team == home_team) %>% 
          arrange(desc(season), desc(week)) %>% 
          slice(1) %>% 
          pull(date)
        
        # get latest rating for each team
        home_rating <- elo_ratings %>% 
          filter(team == home_team) %>% 
          arrange(desc(season), desc(week)) %>% 
          slice(1) %>% 
          pull(elo_rating)
        away_rating <- elo_ratings %>% 
          filter(team == away_team) %>% 
          arrange(desc(season), desc(week)) %>% 
          slice(1) %>% 
          pull(elo_rating)
        
        # Regress Elo Rating towards mean if it's the first week of a new season - and place it under week 0 for that year for "preseason rankings"
        if((game_date - previous_game_date >= 90) & (game_season != min(cfb_games$season))){
          home_rating <- home_rating * (regress) + 1500 * (1 - regress)
          away_rating <- away_rating * (regress) + 1500 * (1 - regress)
        
        updated_ratings <- tibble(team = c(home_team , away_team),
                                  elo_rating = c(home_rating, away_rating),
                                  week = rep(0, 2),
                                  season = rep(game_season, 2),
                                  date = rep(game_date, 2))
        elo_ratings <- elo_ratings %>% 
          bind_rows(updated_ratings)
        }
        # Add home-field advantage (in elo points)
        home_rating_w_home_field <- home_rating + home_field_advantage
        
        # Get new ratings
        new_home_rating <- calc_new_elo_rating(home_rating, home_score, calc_expected_score(home_rating_w_home_field, away_rating))
        
        new_away_rating <- calc_new_elo_rating(away_rating, 1-home_score, calc_expected_score(away_rating, home_rating_w_home_field))
        
        # Join current ratings with new ratings
        
        updated_ratings <- tibble(team = c(home_team , away_team),
                                  elo_rating = c(new_home_rating, new_away_rating),
                                  week = rep(game_week, 2),
                                  season = rep(game_season, 2),
                                  date = rep(game_date, 2))
        elo_ratings <- elo_ratings %>% 
          bind_rows(updated_ratings)
      }
  }
}
}

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
