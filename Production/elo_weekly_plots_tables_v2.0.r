
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
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/WIP/dk_moneylines.R")
source("Production/source_everything.r")

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

# Rankings - all teams
elo_weekly_rankings <- joined_stats %>% 
  arrange(desc(elo)) %>% 
  mutate(row_num = row_number()) %>% 
  relocate(row_num) %>% 
  select(-n_games) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Elo Ratings and Expected Wins"),
             subtitle = "Expected Wins Based on head-to-head Elo Ratings") %>% 
  cols_label(row_num = "Rank", home_team = "Team", elo = "Elo Rating", expected_wins = "Expected Wins", win_rate = "Win Percentage", conference = "Conference") %>% 
  fmt_number(columns = c(elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_number(columns = c(expected_wins), decimals = 1, use_seps = FALSE) %>% 
  fmt_percent(columns = c(win_rate), decimals = 1) %>% 
  data_color(columns = c(elo, expected_wins), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = elo_weekly_rankings, 
       filename = paste0(year(today()), "_elo_weekly_rankings_", str_replace_all(now(), ":", "."), ".png"),
       path = "R Plots/")

# Top 25
elo_weekly_top_25 <- joined_stats %>% 
  arrange(desc(elo)) %>% 
  mutate(row_num = row_number()) %>% 
  relocate(row_num) %>% 
  select(-n_games) %>% 
  filter(row_num <= 25) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Elo Ratings and Expected Wins"),
             subtitle = "Expected Wins Based on head-to-head Elo Ratings") %>% 
  cols_label(row_num = "Rank", home_team = "Team", elo = "Elo Rating", expected_wins = "Expected Wins", win_rate = "Win Percentage", conference = "Conference") %>% 
  fmt_number(columns = c(elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_number(columns = c(expected_wins), decimals = 1, use_seps = FALSE) %>% 
  fmt_percent(columns = c(win_rate), decimals = 1) %>% 
  data_color(columns = c(elo, expected_wins), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = elo_weekly_top_25, 
       filename = paste0(year(today()), "_elo_weekly_top_25_", str_replace_all(now(), ":", "."), ".png"),
       path = "R Plots/")

# Weekly Win Probabilities and Bets ------------------------------------------------

betting_url <- paste0("https://api.collegefootballdata.com/lines?year=", j, "&")
full_url_betting <- paste0(betting_url, "week=", as.character(if_else(
  {upcoming.games %>% 
      filter(week == week_of_upcoming_games) %>% 
      pull(season_type) %>% 
      unique()} == "postseason", 
  "1&seasonType=postseason", 
  as.character(week_of_upcoming_games))))

betting.master = data.frame()
full_url_betting_encoded <- URLencode(full_url_betting)
betting <- cfbd_api(full_url_betting_encoded, my_key)
betting <- as_tibble(betting)
betting <- unnest(betting, cols = c(lines))
betting.master = rbind(betting.master, betting)

# Need to summarise lines for teams with multiple lines
betting_consensus <- betting.master %>% mutate(spread = as.double(spread)) %>%
  group_by(id) %>% 
  mutate(count_id = n()) %>% # Count number of lines per game
  filter(case_when(count_id > 1 ~ provider == "consensus", # If a team has multiple lines, get the consensus line
                   TRUE ~ provider == provider)) %>% 
  select(id, provider, c(spread:last_col(1)))

win_probs <- upcoming.games %>% 
  filter(week == week_of_upcoming_games) %>% 
  select(id, game_date, home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference) %>%
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
  filter(game_date >= lubridate::now()) %>% 
  arrange(game_date, home_team) %>% # arrange by date first, then home team
  mutate(game_date = lubridate::with_tz(game_date, "America/New_York")) %>% # convert to Eastern timezone
  mutate(across(.cols = formattedSpread, .fns = ~ replace_na(.x, ""))) # replace NAs in spread with blanks for the table

list_of_conferences <- win_probs_w_lines %>% pull(home_conference, away_conference) %>% unique()
for(i in 1:length(list_of_conferences)){
  
  conf_name <- list_of_conferences[i]
  
# Table of win probabilities for the week
win_probabilities_this_week <- win_probs_w_lines %>%
  filter(home_conference == conf_name | away_conference == conf_name) %>% 
  mutate(elo_different = if_else(elo_different == T, "Yes", "No")) %>% 
  select(game_date, home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference, formattedSpread, elo_different) %>%
  mutate(game_date = as.character(game_date)) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Win Probabilities"),
             subtitle = paste0(conf_name, " — Based on head-to-head Elo Ratings")) %>% 
  tab_spanner(label = "Start Time",
              columns = c(game_date)) %>% 
  tab_spanner(label = "Home", # Add a column spanning header
              columns = c(home_team,home_elo, home_pred_win_prob, home_conference)) %>% 
  tab_spanner(label = "Away", # Add a column spanning header
              columns = c(away_team, away_elo, away_pred_win_prob, away_conference)) %>% 
  tab_spanner(label = "Betting",
              columns = c(formattedSpread, elo_different)) %>% 
  cols_label(game_date = "Kickoff (Eastern)", home_team = "Team", home_elo = "Elo Rating", home_pred_win_prob = "Win Probability", home_conference = "Conference",
             away_team = "Team", away_elo = "Elo Rating", away_pred_win_prob = "Win Probability", away_conference = "Conference",
             formattedSpread = "Spread", elo_different = "Elo Mismatch?") %>% 
  fmt_percent(columns = c(home_pred_win_prob, away_pred_win_prob), decimals = 1) %>% 
  fmt_number(columns = c(home_elo, away_elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_datetime(columns = c(game_date), date_style = 6, time_style = 4) %>% 
  data_color(columns = c(home_pred_win_prob, away_pred_win_prob), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  data_color(columns = c(elo_different),
             colors = scales::col_factor(
               palette = c(staturdays_colors("white"), staturdays_colors("orange")),
               domain = NULL),
             alpha = 0.7) %>% 
  tab_style( # Add a weighted line down the middle
    style = list(
      cell_borders(
        sides = "left",
        color = staturdays_colors("dark_blue"),
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(home_team, away_team, formattedSpread)
      )
    )
  ) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = win_probabilities_this_week, 
       filename = paste0(year(today()), "_win_probabilities_this_week_", week_of_upcoming_games, "_", conf_name, "_", str_replace_all(now(), ":", "."), ".png"),
       path = "R Plots/")
}

# Get win probabilities for games played within 16 hours of now

win_probs_today <- win_probs_w_lines %>%
  filter(game_date <= now() + lubridate::hours(16) & game_date >= now()) %>% 
  mutate(elo_different = if_else(elo_different == T, "Yes", "No")) %>% 
  select(game_date, home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference, formattedSpread, elo_different) %>%
  mutate(game_date = as.character(game_date)) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Win Probabilities"),
             subtitle = paste0("Based on head-to-head Elo Ratings")) %>% 
  tab_spanner(label = "Start Time",
              columns = c(game_date)) %>% 
  tab_spanner(label = "Home", # Add a column spanning header
              columns = c(home_team,home_elo, home_pred_win_prob, home_conference)) %>% 
  tab_spanner(label = "Away", # Add a column spanning header
              columns = c(away_team, away_elo, away_pred_win_prob, away_conference)) %>% 
  tab_spanner(label = "Betting",
              columns = c(formattedSpread, elo_different)) %>% 
  cols_label(game_date = "Kickoff (Eastern)", home_team = "Team", home_elo = "Elo Rating", home_pred_win_prob = "Win Probability", home_conference = "Conference",
             away_team = "Team", away_elo = "Elo Rating", away_pred_win_prob = "Win Probability", away_conference = "Conference",
             formattedSpread = "Spread", elo_different = "Elo Mismatch?") %>% 
  fmt_percent(columns = c(home_pred_win_prob, away_pred_win_prob), decimals = 1) %>% 
  fmt_number(columns = c(home_elo, away_elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_datetime(columns = c(game_date), date_style = 6, time_style = 4) %>% 
  data_color(columns = c(home_pred_win_prob, away_pred_win_prob), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  data_color(columns = c(elo_different),
             colors = scales::col_factor(
               palette = c(staturdays_colors("white"), staturdays_colors("orange")),
               domain = NULL),
             alpha = 0.7) %>% 
  tab_style( # Add a weighted line down the middle
    style = list(
      cell_borders(
        sides = "left",
        color = staturdays_colors("dark_blue"),
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(home_team, away_team, formattedSpread)
      )
    )
  ) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data") # %>% # code to make columns and font sizes different
# cols_width(c(home_team, away_team, home_conference, away_conference, formattedSpread) ~ px(70), everything() ~ px(50)) %>% tab_options(table.font.size = 12)

gtsave(data = win_probs_today, 
       filename = paste0(year(today()), "_win_probabilities_this_week_", week_of_upcoming_games, "_today_", str_replace_all(now(), ":", "."), ".png"),
       path = "R Plots/")

# Moneyline vs. Elo Plot --------------------------------------------------

win_probs_moneyline_1 <- win_probs_w_lines %>% 
  mutate(home_implied_odds = case_when(str_detect(homeMoneyline, "-") == TRUE ~ abs(as.integer(homeMoneyline))/(abs(as.integer(homeMoneyline)) + 100),
                                       str_detect(homeMoneyline, "-") == FALSE ~ 100/(abs(as.integer(homeMoneyline))+100),
                                       TRUE ~ 0),
         away_implied_odds = case_when(str_detect(awayMoneyline, "-") == TRUE ~ abs(as.integer(awayMoneyline))/(abs(as.integer(awayMoneyline)) + 100),
                                       str_detect(awayMoneyline, "-") == FALSE ~ 100/(abs(as.integer(awayMoneyline))+100),
                                       TRUE ~ 0))

win_probs_moneyline_1 %>% 
  mutate(diff_from_vegas = abs(home_pred_win_prob - home_implied_odds)) %>% 
  ggplot(aes(x = home_pred_win_prob, y = home_implied_odds)) +
  geom_point() +
  geom_abline() +
  geom_label(aes(x = .75, y = .25), label = "Vegas Underconfident") +
  geom_label(aes(x = .25, y = .75), label = "Vegas Overconfident") +
  ggrepel::geom_text_repel(aes(label = if_else(diff_from_vegas > .2, paste0(away_team, " @ ", home_team), "")))

## Calculate biggest upsets week-over-week by win prob and change in Elo

home_wow_elo_change <- elo_ratings %>% 
  filter(season == max(season)) %>% 
  arrange(desc(date)) %>% 
  group_by(team) %>% 
  mutate(wow_change = ((elo_rating) - lag(elo_rating, n = 1, order_by = date))/(lag(elo_rating, n = 1, order_by = date)), previous_elo = lag(elo_rating, n =1, order_by = date)) %>% 
  slice_max(order_by = date, n = 1L) %>% 
  inner_join(upcoming.games, by = c("team" = "home_team", "week", "season")) %>% 
  filter(game_date < lubridate::now())

away_wow_elo_change <- elo_ratings %>% 
  filter(season == max(season)) %>% 
  arrange(desc(date)) %>% 
  group_by(team) %>% 
  mutate(wow_change = ((elo_rating) - lag(elo_rating, n = 1, order_by = date))/(lag(elo_rating, n = 1, order_by = date)), previous_elo = lag(elo_rating, n =1, order_by = date)) %>% 
  slice_max(order_by = date, n = 1L) %>% 
  inner_join(upcoming.games, by = c("team" = "away_team", "week", "season")) %>% 
  filter(game_date < lubridate::now())

wow_elo_change <- rbind(home_wow_elo_change, away_wow_elo_change) %>% 
  select(1:8, home_team, away_team, home_points, away_points, game_outcome_home, home_pred_win_prob, away_pred_win_prob) %>% 
  mutate(home_surprise = game_outcome_home - home_pred_win_prob, away_surprise = (1-game_outcome_home) - away_pred_win_prob)

wow_elo_change_top <- wow_elo_change %>% 
  arrange(desc(wow_change)) %>% 
  filter(week == week_of_elo_last_updated) %>% 
  select(-date, -home_surprise, -away_surprise, -conference, -game_outcome_home) %>% 
  ungroup() %>% 
  slice_max(order_by = wow_change, n = 10)

wow_elo_change_bottom <- wow_elo_change %>% 
  arrange((wow_change)) %>% 
  filter(week == week_of_elo_last_updated) %>% 
  select(-date, -home_surprise, -away_surprise, -conference, -game_outcome_home) %>% 
  ungroup() %>% 
  slice_min(order_by = wow_change, n = 10)

wow_elo_change_combined <- wow_elo_change_top %>% 
  rbind(wow_elo_change_bottom) %>% 
  arrange(desc(wow_change)) %>% 
  mutate(opponent = if_else(is.na(home_team)==T, away_team, home_team)) %>% 
  mutate(win_prob = if_else(is.na(home_team)==T, home_pred_win_prob, 1-home_pred_win_prob)) %>% 
  select(-home_team, -away_team, -home_pred_win_prob) %>% 
  distinct()

# Table of movers

wow_elo_change_tbl <- wow_elo_change_combined %>% 
  select(team, opponent, elo_rating, previous_elo, wow_change, win_prob) %>% 
  gt() %>% 
  tab_header(title = paste0(as.character(max(upcoming.games$season)), " Week ", as.character(week_of_elo_last_updated), " Biggest Elo Movers"),
             subtitle = "Largest changes in Elo") %>% 
  cols_label(team = "Team", elo_rating = "New Elo", previous_elo = "Old Elo", wow_change = "Pct. Change", opponent = "Opponent", win_prob = "Win Probability") %>% 
  fmt_number(columns = c(elo_rating, previous_elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_percent(columns = c(wow_change, win_prob), decimals = 1, use_seps = FALSE) %>% 
  data_color(columns = c(wow_change), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

if(week_of_elo_last_updated > 0){
  gtsave(data = wow_elo_change_tbl, 
         filename = paste0(year(today()), "_wow_elo_change_tbl_", week_of_elo_last_updated, "_", str_replace_all(now(), ":", "."), ".png"),
         path = "R Plots/")
}

# Latest Brier for the season
brier <- upcoming.games %>% summarise(brier = mean((game_outcome_home - home_pred_win_prob)^2))
elo_record <- upcoming.games %>% 
  filter(is.na(home_points) == F) %>% 
  mutate(correct = if_else(abs(home_pred_win_prob - game_outcome_home) < .5, 1, 0)) %>% 
  count(correct)
elo_record <- paste0(elo_record[2,2], "-", elo_record[1,2])

# Evaluate Elo
elo_brier_plot <- upcoming.games %>% 
  filter(is.na(home_points) == F) %>% 
  mutate(win_prob_bucket = round(home_pred_win_prob, 1)) %>% 
  group_by(win_prob_bucket) %>% 
  summarise(avg_actual_outcome = mean(game_outcome_home)) %>% 
  ggplot(aes(x = win_prob_bucket, y = avg_actual_outcome)) +
  geom_point(color = staturdays_colors("dark_blue"), size = 4, alpha = 0.7) +
  geom_abline(linetype = "dashed", color = staturdays_colors("orange")) +
  geom_label(aes(x = .75, y = .15, label = "Winning Less \nThan Expected"), 
             color = staturdays_colors("white"), fontface = "bold", size = 4, 
             fill = staturdays_colors("orange")) +
  geom_label(aes(x = .25, y = .85, label = "Winning More \nThan Expected"),
             color = staturdays_colors("white"), fontface = "bold", size = 4, 
             fill = staturdays_colors("orange")) +
  staturdays_theme +
  labs(title = paste0("Elo Predicted vs. Actual \nThrough Week ", week_of_elo_last_updated),
       subtitle = paste0("Brier Score of ", round(brier, 2)),
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = "Predicted Win Probability",
       y = "Actual Average Wins") +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

if(week_of_elo_last_updated > 0){
  ggsave(filename = paste0(year(now()), "_elo_brier_plot_", str_replace_all(now(), ":", "."), ".png"), 
         plot = elo_brier_plot,
         path = "R Plots/",
         dpi = 300, width = 200, height = 200, units = "mm")
}
# 
# # WIP ---------------------------------------------------------------------
# 
# # Do the below but use the cfbd moneyline instead
# 
# # Join in moneyline data from DraftKings ----------------------------------
# 
# games_df <- win_probs_w_lines %>% 
#   mutate(join_key_1 = paste0(home_team, away_team),
#          join_key_2 = paste0(away_team, home_team))
# 
# ml_top <- games_df %>% 
#   inner_join(ml_df, by = c("join_key_1" = "join_key")) %>% 
#   select(-c(join_key_2, join_key_1))
# 
# ml_bottom <- games_df %>% 
#   inner_join(ml_df, by = c("join_key_2" = "join_key")) %>% 
#   select(-c(join_key_2, join_key_1))
# 
# ml_joined <- rbind(ml_top, ml_bottom)
# 
# ml_clean <- ml_joined %>% 
#   mutate(implied_odds_home = if_else(label_favorite == home_team,
#                                    implied_odds_favorite,
#                                    implied_odds_underdog),
#          implied_odds_away = if_else(label_favorite == home_team,
#                                    implied_odds_underdog,
#                                    implied_odds_favorite))
# 
# ml_clean %>% 
#   ggplot(aes(x = home_pred_win_prob, y = implied_odds_home)) +
#   geom_point() +
#   geom_abline(linetype = 2) +
#   geom_text(aes(label = if_else(abs(home_pred_win_prob - implied_odds_home) > .1,
#                                 paste0(away_team, " @ \n", home_team),
#                                 ""))) +
#   labs(title = "Elo vs. Vegas Win Probabilities") +
#   annotate(geom = "label", x = .25, y = .75, label = "Vegas Overconfident") +
#   annotate(geom = "label", x = .75, y = .15, label = "Vegas Underconfident")
