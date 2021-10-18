# Evaluate elo vs. moneyline for 2021


# Run elo_weekly_plots_tables_v2.1.r first, then this.


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
source("Production/source_everything.r")

# New Season Regression Factor
regress <- (.95)
# k-factor
k <- 80
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

calc_new_elo_rating <- function(team_rating, actual_score, expected_score, k=85){
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

## Update this value each week before running script
# week_of_games_just_played <- games.master %>% filter(is.na(home_points) == F & is.na(away_points) == F) %>% group_by(season) %>% slice_max(order_by = start_date, n = 1) %>% pull(week) %>% unique()
# week_of_upcoming_games <- week_of_games_just_played + 1L

#Select variables we want
cfb_games <- games.master %>% select(id, season, week, season_type, home_team, home_conference, away_team, away_conference, home_points, away_points, start_date, neutral_site) %>% 
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

# Read in historic Elo ratings
elo_ratings <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv",
                     encoding = "UTF-8")

# Rebuild upcoming.games with new Elo Ratings before updating all plots and tables --------
{
  upcoming.games <- tibble(cfb_games)
  # Save a version of this year's games for later
  lastweek.games <- upcoming.games
  
  elo_conf <- elo_ratings %>% 
    mutate(conference_class = case_when(conference %in% power_5 ~ 1500,
                                        conference %in% group_of_5 ~ g5,
                                        conference %in% "FBS Independents" ~ 1500,
                                        TRUE ~ d3))
  
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
  
  # Calculate max week in elo to tell plots/tables which week to show
  week_of_elo_last_updated <- elo_ratings %>% filter(season == max(season)) %>% pull(week) %>% max()
  week_of_upcoming_games <- upcoming.games %>% 
    filter(date >= lubridate::now()) %>% 
    slice_min(order_by = date, n = 1L) %>% 
    pull(week) %>% unique()
  
  # Join cfb games with elo ratings for home and away teams by team name and date of rating/game
  upcoming.games <- left_join(upcoming.games, elo_ratings_tmp, by = c("home_team" = "team", "week", "season")) %>% 
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
    mutate(home_pred_win_prob = calc_expected_score(home_elo + if_else(neutral_site == F, home_field_advantage, neutral_adjust), away_elo), away_pred_win_prob = 1 - home_pred_win_prob)
  
  rm(list = c("upcoming.tmp", "upcoming.tmp2", "upcoming.tmp3", "upcoming.tmp4"))
}

### Tables

# Table of Elo Ratings - If the game is already played, use the actual result, and if not then use the win probability to get expected wins for the season
home_stats <- upcoming.games %>% 
  group_by(home_team) %>% 
  mutate(elo = most_recent_elo_home) %>% 
  summarise(elo = max(elo), expected_wins = sum(case_when(is.na(home_points)==T ~ home_pred_win_prob,
                                                          TRUE ~ game_outcome_home)), 
            n_games = n())

away_stats <- upcoming.games %>% 
  group_by(away_team) %>% 
  mutate(elo = most_recent_elo_away) %>%
  summarise(elo = max(elo), expected_wins = sum(case_when(is.na(away_points)==T ~ away_pred_win_prob,
                                                          TRUE ~ (1-game_outcome_home))), 
            n_games = n())

# Right now, I am removing games where the opponent's elo is NA, so the expected_win value is NA, but this needs to be resolved. Only affects 3 teams.
joined_stats <- left_join(home_stats, away_stats, by = c("home_team" = "away_team")) %>% 
  left_join(current_elo_ratings_only, by = c("home_team" = "team")) %>% 
  rename(most_recent_elo = elo_rating) %>% 
  group_by(home_team) %>% 
  summarise(elo = most_recent_elo, expected_wins = sum(expected_wins.x, expected_wins.y, na.rm = T), n_games = sum(n_games.x, n_games.y), win_rate = expected_wins / n_games)

# Get just teams and conferences from 2019
conf_most_recent <- elo_conf %>% 
  group_by(team) %>% 
  filter(date == max(date)) %>% 
  select(team, conference) %>% 
  mutate(conference = if_else(is.na(conference) == T, "Non-FBS", conference))

# Add conference to joined_stats
joined_stats <- left_join(joined_stats, conf_most_recent, by = c("home_team" = "team"))


# Issue #63 work ----------------------------------------------------------

# Add previous week elo for each team

elo_ratings_last_week <- elo_ratings %>% 
  group_by(team) %>% 
  slice_max(order_by = date, n = 2L) %>% # Get two most recent rankings
  slice_min(order_by = date, n = 1L) %>% # choose the older of the two (last week's)
  ungroup() %>% 
  mutate(rank = rank(desc(elo_rating), ties.method = "min")) %>% 
  select(team, elo_rating, rank) %>% 
  rename(last_week_elo = elo_rating,
         last_week_rank = rank)

elo_w_last_week <- joined_stats %>% 
  ungroup() %>% 
  mutate(rank = rank(desc(elo), ties.method = "min")) %>% 
  left_join(elo_ratings_last_week, by = c("home_team" = "team")) %>% 
  mutate(elo_change = elo - last_week_elo,
         rank_change = -(rank - last_week_rank)) %>% # Take negative of rank change since going up in rank is bad
  rename(team = home_team)

# Stable

# Add in the result of the previous game "L Penn State 28-20", "W Auburn 28-20"

# Most recent home result for each team
home_top <- upcoming.games %>% 
  mutate(team = home_team) %>% 
  group_by(team) %>% 
  filter(week <= week_of_elo_last_updated) %>% 
  slice_max(order_by = game_date, n = 1L)

# Most recent away result for each team
away_top <- upcoming.games %>% 
  mutate(team = away_team) %>% 
  group_by(team) %>% 
  filter(week <= week_of_elo_last_updated) %>% 
  slice_max(order_by = game_date, n = 1L)

# Row-bind latest home and away games
bottom <- home_top %>% rbind(away_top) %>% 
  slice_max(order_by = game_date, n = 1L)

# Get last game result for each team
last_game_result <- bottom %>% 
  mutate(result = case_when(team == home_team & 
                              home_points > away_points ~ paste0("W ",
                                                                 away_team,
                                                                 " ",
                                                                 home_points,
                                                                 "-",
                                                                 away_points),
                            team == home_team & 
                              home_points < away_points ~ paste0("L ",
                                                                 away_team,
                                                                 " ",
                                                                 away_points,
                                                                 "-",
                                                                 home_points),
                            team == away_team & 
                              home_points < away_points ~ paste0("W ",
                                                                 home_team,
                                                                 " ",
                                                                 away_points,
                                                                 "-",
                                                                 home_points),
                            team == away_team & 
                              home_points > away_points ~ paste0("L ",
                                                                 home_team,
                                                                 " ",
                                                                 home_points,
                                                                 "-",
                                                                 away_points),
                            TRUE ~ "failed")) %>% 
  select(team, result)

# Join to elo table
joined_stats_final <- elo_w_last_week %>% 
  left_join(last_game_result, by = c("team")) %>% 
  mutate(image = case_when(rank_change > 0 ~ "Assets/green_up_arrow.png",
                           rank_change < 0 ~ "Assets/red_down_arrow.png",
                           rank_change == 0 ~ "Assets/no_change.png"))

# Stable


# Start profit/loss calcs -------------------------------------------------

betting_url <- paste0("https://api.collegefootballdata.com/lines?year=", j)
full_url_betting <- paste0(betting_url)

betting.master = data.frame()
full_url_betting_encoded <- URLencode(full_url_betting)
betting <- cfbd_api(full_url_betting_encoded, my_key)
betting <- as_tibble(betting)
betting <- unnest(betting, cols = c(lines))
betting.master = rbind(betting.master, betting)

# Need to summarise lines for teams with multiple lines
betting_consensus <- betting.master %>% 
  mutate(spread = as.double(spread),
         overUnder = as.double(overUnder)) %>%
  group_by(id, season, week, homeTeam, awayTeam,
           homeConference, awayConference, homeScore, awayScore,
           formattedSpread) %>% 
  summarise(consensus_spread = mean(spread, na.rm = TRUE),
            consensus_over_under = mean(overUnder, na.rm = TRUE),
            consensus_home_ml = mean(homeMoneyline, na.rm = TRUE),
            consensus_away_ml = mean(awayMoneyline, na.rm = TRUE)) %>% 
  filter(week < week_of_upcoming_games) %>% 
  rename(spread = consensus_spread,
         overUnder = consensus_over_under,
         homeMoneyline = consensus_home_ml,
         awayMoneyline = consensus_away_ml)

win_probs <- upcoming.games %>% 
  filter(week < week_of_upcoming_games) %>% 
  select(id, game_date, game_outcome_home, home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference) %>%
  arrange(desc(home_pred_win_prob))

win_probs_w_lines <- win_probs %>% 
  left_join(betting_consensus, by = "id")

# Join Lines and find any mismatches between Elo and the Lines
win_probs_w_lines <- win_probs_w_lines %>% 
  mutate(home_favorite = case_when(str_detect(win_probs_w_lines$formattedSpread, win_probs_w_lines$home_team) ~ T, # Search if home team is favored
                                   TRUE ~ F)) %>% 
  mutate(elo_different = case_when(is.na(spread) == TRUE ~ F, # Check if Elo agrees or disagrees
                                   (home_favorite == TRUE) & (home_pred_win_prob < 0.5) ~ T,
                                   (home_favorite == FALSE) & (home_pred_win_prob >= 0.5) ~ T,
                                   TRUE ~ F))

# Filter any games already played from win prob this week table (covers cases where teams play twice in one week (e.g. Week 0 and Week 1))
win_probs_w_lines <- win_probs_w_lines %>% 
  filter(game_date <= lubridate::now()) %>% 
  arrange(game_date, home_team) %>% # arrange by date first, then home team
  mutate(game_date = lubridate::with_tz(game_date, "America/New_York")) %>% # convert to Eastern timezone
  mutate(across(.cols = formattedSpread, .fns = ~ replace_na(.x, ""))) %>%  # replace NAs in spread with blanks for the table
  mutate(homeMoneyline = if_else(homeMoneyline < 0, 
                                 as.character(homeMoneyline),
                                 paste0("+", homeMoneyline)),
         awayMoneyline = if_else(awayMoneyline < 0, 
                                 as.character(awayMoneyline),
                                 paste0("+", awayMoneyline)))

win_probs_moneyline_1 <- win_probs_w_lines %>% 
  mutate(home_implied_odds = case_when(str_detect(homeMoneyline, "-") == TRUE ~ abs(as.integer(homeMoneyline))/(abs(as.integer(homeMoneyline)) + 100),
                                       str_detect(homeMoneyline, "-") == FALSE ~ 100/(abs(as.integer(homeMoneyline))+100),
                                       TRUE ~ 0),
         away_implied_odds = case_when(str_detect(awayMoneyline, "-") == TRUE ~ abs(as.integer(awayMoneyline))/(abs(as.integer(awayMoneyline)) + 100),
                                       str_detect(awayMoneyline, "-") == FALSE ~ 100/(abs(as.integer(awayMoneyline))+100),
                                       TRUE ~ 0)) %>% 
  filter(is.na(homeMoneyline) == FALSE)

# Calc expected value on Elo bets

expected_value_tbl <- win_probs_moneyline_1 %>% 
  mutate(home_diff = home_pred_win_prob - home_implied_odds,
         away_diff = away_pred_win_prob - away_implied_odds,
         home_win_10d_bet = if_else(str_detect(homeMoneyline, "-") == TRUE,
                                    (abs(as.double(homeMoneyline) - 100)) / (abs(as.double(homeMoneyline)) / 10),
                                    (as.double(homeMoneyline) + 100) / 10),
         away_win_10d_bet = if_else(str_detect(awayMoneyline, "-") == TRUE,
                                    (abs(as.double(awayMoneyline) - 100)) / (abs(as.double(awayMoneyline)) / 10),
                                    (as.double(awayMoneyline) + 100) / 10),
         home_exp_value = ((home_win_10d_bet - 10) * home_pred_win_prob) - (10 * (1-home_pred_win_prob)),
         away_exp_value = ((away_win_10d_bet - 10) * away_pred_win_prob) - (10 * (1-away_pred_win_prob)))

# Calc profit/loss for the year

expected_value_tbl %>% 
  filter(home_exp_value > 0 | away_exp_value > 0) %>% 
  mutate(profit = case_when(game_outcome_home == 1 & home_exp_value > 0 ~ home_win_10d_bet - 10, # Betting rules. Bet on home when home exp. value > 0
                            game_outcome_home == 0 & home_exp_value > 0 ~ -10,
                            game_outcome_home == 1 & away_exp_value > 0 ~ -10,
                            game_outcome_home == 0 & away_exp_value > 0 ~ away_win_10d_bet - 10,
                            TRUE ~ -Inf),
         wins = if_else(profit > 0, 1, 0),
         losses = 1 - wins) %>% 
  ungroup() %>% 
  summarise(p_and_l = sum(profit), n = n(),
            winning_bets = sum(wins),
            losing_bets = sum(losses))
