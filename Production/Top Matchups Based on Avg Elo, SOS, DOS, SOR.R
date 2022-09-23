# Libraries and Themes ----------------------------------------------------
rm(list = ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(lubridate)
library(gt)
library(webshot)
library(data.table)
library(grid)

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

staturdays_palette <- c("#041e42", "#22345a", "#394871", "#4c5872", "#5c6272", "#de703b")

staturdays_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (staturdays_col_list)
  
  staturdays_col_list[cols]
}

staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("light_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          axis.title = element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.title = element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          panel.background = element_blank(),
                          panel.grid = element_line(color = "#d6d6d6"),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_line(color = "#d6d6d6")
)

# Logo
logo <- grid::rasterGrob(png::readPNG("C:/Users/Kyle/Documents/Kyle/Staturdays/Logo Final/4thdownmarkerlogo.png"), interpolate = T)

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC", "FBS Independents")
group_of_5 <- c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt")

# New Season Regression Factor
regress <- (.95)
# k-factor
k <- 85
# home-field advantage (in elo points)
home_field_advantage <- 55
# Conference adjustors
g5 <- 1200
d3 <- 500

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

games.master = data.frame()
for (j in 2020) {
  for (i in 1:20) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

#Select variables we want
cfb_games <- games.master %>% select(id, season, week, season_type, home_team, home_conference, away_team, away_conference, home_points, away_points, start_date) %>% 
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

if(is_empty(postseason_start_epiweek) == F){
  ### Adjust postseason games to correct week
  cfb_games <- cfb_games %>%
    mutate(week = if_else(season_type == "postseason", postseason_start_week + epiweek(date) - postseason_start_epiweek, week)) 
}

# Predict Upcoming Week Outcomes ------------------------------------------
upcoming.games <- tibble(cfb_games)
# Save a version of this year's games for later
lastweek.games <- upcoming.games
# Read in historic Elo ratings
elo_ratings <- read_csv(file = "https://raw.githubusercontent.com/kylebennison/staturdays/master/elo_ratings_historic.csv",
                        col_types = list(col_character(), col_character(), col_double(), col_integer(), col_integer(), col_date(format = "%Y-%m-%d")))

elo_conf <- elo_ratings %>% 
  mutate(conference_class = case_when(conference %in% power_5 ~ 1500,
                                      conference %in% group_of_5 ~ g5,
                                      conference %in% "FBS Independents" ~ 1500,
                                      TRUE ~ d3))

# Mutate week of elo_ratings for joining purposes
elo_ratings_tmp <- elo_ratings %>% 
  mutate(week = week + 1)

games.final <- tibble()
for(szn in 2020:2020){
  for(wk in 1:20){
    message(paste0("Running ", szn, " Week ", wk))
    
    # Get most updated rating for each team
    current_elo_ratings <- elo_ratings_tmp %>% filter(season <= szn, week <= wk) %>% 
      group_by(team) %>% 
      filter(date == max(date))
    
    # Take just team and rating
    current_elo_ratings_only <- current_elo_ratings %>% select(team, elo_rating)
    
    # Get start of season elo for each team only
    preseason_elo_ratings <- elo_ratings %>%
      filter(season == szn, week == 0) %>% 
      select(team, elo_rating)
    
    # Reset upcoming.games
    upcoming.games <- tibble(cfb_games)
    
    # Filter games just from this week
    upcoming.games <- upcoming.games %>% 
      filter(season == szn, week == wk)
    
    # Join cfb games with elo ratings for home and away teams by team name and date of rating/game
    upcoming.games <- left_join(upcoming.games, current_elo_ratings, by = c("home_team" = "team")) %>% 
      rename(home_elo = elo_rating) %>% 
      rename(home_elo_date = date.y) %>% 
      rename(game_date = date.x)
    
    upcoming.games <- left_join(upcoming.games, current_elo_ratings, by = c("away_team" = "team")) %>% 
      rename(away_elo = elo_rating) %>% 
      rename(away_elo_date = date)
    
    upcoming.tmp4 <- upcoming.games %>% # Use most recent elo rating for current/future games. If they don't have one, use the means by conference.
      mutate(home_elo = case_when(is.na(home_elo) == F ~ home_elo,
                                  is.na(home_elo) == T ~ case_when(home_conference %in% power_5 ~ 1500,
                                                                   home_conference %in% group_of_5 ~ g5,
                                                                   TRUE ~ d3),
                                  TRUE ~ home_elo),
             home_elo_date = case_when(is.na(home_elo_date) == T ~ {elo_ratings %>% filter(season == szn) %>% summarise(min_date = min(date)) %>% pull(min_date)},
                                       is.na(home_elo_date) == F ~ home_elo_date)) %>% 
      mutate(away_elo = case_when(is.na(away_elo) == F ~ away_elo,
                                  is.na(away_elo) == T ~ case_when(away_conference %in% power_5 ~ 1500,
                                                                   away_conference %in% group_of_5 ~ g5,
                                                                   TRUE ~ d3),
                                  TRUE ~ away_elo),
             away_elo_date = case_when(is.na(away_elo_date) == T ~ {elo_ratings %>% filter(season == max(szn)) %>% summarise(min_date = min(date)) %>% pull(min_date)},
                                       is.na(away_elo_date) == F ~ away_elo_date))
    
    # Get win prob
    upcoming.games <- upcoming.tmp4 %>% 
      mutate(home_pred_win_prob = calc_expected_score(home_elo+home_field_advantage, away_elo), away_pred_win_prob = 1 - home_pred_win_prob)
    
    rm(list = c("upcoming.tmp4"))
    
    games.final <- games.final %>% rbind(upcoming.games)
  }
}

top_avg_elo_matchups_2020 <- games.final %>%
  ungroup() %>% 
  filter(season == 2020, home_conference %in% power_5) %>% 
  mutate(avg_game_elo = (home_elo + away_elo)/2, elo_diff = abs(home_elo - away_elo)) %>% 
  slice_max(order_by = avg_game_elo, n = 10)

top_avg_elo_matchups_2020_tbl <- top_avg_elo_matchups_2020 %>% 
  select(week.x, home_team, away_team, home_elo, away_elo, avg_game_elo, game_date) %>% 
  mutate(game_date = as.Date(game_date)) %>% 
  arrange(desc(avg_game_elo)) %>% 
  mutate(row_num = row_number(), matchup = paste0(away_team, " @ ", home_team)) %>% 
  relocate(row_num, matchup) %>% 
  select(-home_team, -away_team, -(c(home_elo, away_elo))) %>% 
  gt() %>% 
  tab_header(title = "Top 10 Best Games of 2020",
             subtitle = "Based on highest combined Elo Ratings") %>% 
  cols_label(row_num = "Rank", matchup = "Matchup", week.x = "Week", avg_game_elo = "Elo Difference", game_date = "Date") %>% 
  fmt_number(vars(avg_game_elo), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(avg_game_elo), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = top_avg_elo_matchups_2020_tbl, 
       filename = paste0("top_avg_elo_matchups_2020_tbl_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

# Closest matchups

top_closest_elo_matchups_2020 <- games.final %>%
  ungroup() %>% 
  filter(season == 2020, home_conference %in% power_5) %>% 
  mutate(avg_game_elo = (home_elo + away_elo)/2, elo_diff = abs(home_elo - away_elo)) %>% 
  slice_min(order_by = elo_diff, n = 10)
  
top_closest_elo_matchups_2020_tbl <- top_closest_elo_matchups_2020 %>% 
  select(week.x, home_team, away_team, home_elo, away_elo, elo_diff, game_date) %>% 
  mutate(game_date = as.Date(game_date)) %>% 
  arrange(abs(elo_diff)) %>% 
  mutate(row_num = row_number(), matchup = paste0(away_team, " @ ", home_team)) %>% 
  relocate(row_num, matchup) %>% 
  select(-home_team, -away_team, -(c(home_elo, away_elo))) %>% 
  gt() %>% 
  tab_header(title = "Top 10 Closest Games of 2020",
             subtitle = "Based on closest Elo Ratings") %>% 
  cols_label(row_num = "Rank", matchup = "Matchup", week.x = "Week", elo_diff = "Elo Difference", game_date = "Date") %>% 
  fmt_number(vars(elo_diff), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(elo_diff), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = top_closest_elo_matchups_2020_tbl, 
       filename = paste0("closest_elo_matchups_2020_tbl_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

# Strength of Schedule, Difficulty of Schedule ----------------------------

lhs.tmp <- games.final %>% 
  filter(season == 2020) %>% 
  group_by(home_team) %>% 
  rename(opponent = away_team, team = home_team) %>% 
  summarise(avg_opponent_elo = mean(away_elo), elo = mean(home_elo), count = n())

rhs.tmp <- games.final %>% 
  filter(season == 2020) %>% 
  group_by(away_team) %>% 
  rename(opponent = home_team, team = away_team) %>% 
  summarise(avg_opponent_elo = mean(home_elo), elo = mean(away_elo), count = n())

team_strength_of_schedule <- left_join(lhs.tmp, rhs.tmp, by = "team") %>% 
  group_by(team) %>% 
  mutate(avg_opponent_elo = ((avg_opponent_elo.x*count.x)/(sum(count.x, count.y))) + ((avg_opponent_elo.y*count.y)/(sum(count.x, count.y))),
         count = sum(count.x, count.y),
         elo = elo.x) %>% 
  select(team, elo, avg_opponent_elo, count) %>% 
  arrange(desc(avg_opponent_elo)) %>% 
  ungroup() %>% 
  mutate(rank = row_number()) %>% 
  mutate(difference = elo - avg_opponent_elo)

team_strength_of_schedule_tbl <- team_strength_of_schedule %>% 
  select(-count) %>% 
  slice_max(order_by = avg_opponent_elo, n = 25) %>% 
  gt() %>% 
  tab_header(title = "2020 Strength of Schedule",
             subtitle = "Average Opponent Elo Rating") %>% 
  cols_label(team = "Team", elo = "Elo Rating", avg_opponent_elo = "Average Opponent Elo", difference = "Elo Advantage/Disadvantage", rank = "SOS Rank") %>% 
  fmt_number(vars(elo, avg_opponent_elo, difference), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(avg_opponent_elo, difference), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = team_strength_of_schedule_tbl, 
       filename = paste0("team_strength_of_schedule_tbl_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

team_difficultly_of_schedule_tbl <- team_strength_of_schedule %>% 
  select(-count) %>% 
  arrange(difference) %>% 
  mutate(rank_difference = row_number()) %>% 
  slice_min(order_by = difference, n = 25) %>% 
  gt() %>% 
  tab_header(title = "2020 Difficulty of Schedule (DOS)",
             subtitle = "Difference between Elo Ratings and Average Opponent Elo Rating") %>% 
  cols_label(team = "Team", elo = "Elo Rating", avg_opponent_elo = "Average Opponent Elo", difference = "Elo Advantage/Disadvantage", rank = "SOS Rank", rank_difference = "DOS Rank") %>% 
  fmt_number(vars(elo, avg_opponent_elo, difference), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(avg_opponent_elo, difference), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = team_difficultly_of_schedule_tbl, 
       filename = paste0("2020_team_difficultly_of_schedule_tbl__", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

team_ease_of_schedule_tbl <- team_strength_of_schedule %>% 
  select(-count) %>% 
  arrange(difference) %>% 
  mutate(rank_difference = row_number()) %>% 
  slice_max(order_by = difference, n = 25) %>% 
  gt() %>% 
  tab_header(title = "2020 Easiest Difficulty of Schedule (DOS)",
             subtitle = "Difference between Elo Ratings and Average Opponent Elo Rating") %>% 
  cols_label(team = "Team", elo = "Elo Rating", avg_opponent_elo = "Average Opponent Elo", difference = "Elo Advantage/Disadvantage", rank = "SOS Rank", rank_difference = "DOS Rank") %>% 
  fmt_number(vars(elo, avg_opponent_elo, difference), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(avg_opponent_elo, difference), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = team_ease_of_schedule_tbl, 
       filename = paste0("2020_team_ease_of_schedule_tbl__", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")


# Strength of Record ------------------------------------------------------

lhs.sor <- games.final %>% 
  filter(season == 2020, !(is.na(home_points) == T & is.na(away_points) == T)) %>% 
  group_by(home_team) %>% 
  rename(opponent = away_team, team = home_team) %>% 
  summarise(avg_opponent_elo = mean(away_elo), elo = mean(home_elo), expected_wins = sum(home_pred_win_prob), actual_wins = sum(game_outcome_home), win_diff = actual_wins - expected_wins, count = n()) %>% 
  arrange(desc(win_diff))

rhs.sor <- games.final %>% 
  filter(season == 2020, !(is.na(home_points) == T & is.na(away_points) == T)) %>% 
  group_by(away_team) %>% 
  rename(opponent = home_team, team = away_team) %>% 
  summarise(avg_opponent_elo = mean(home_elo), elo = mean(away_elo), expected_wins = sum(away_pred_win_prob), actual_wins = sum(1-game_outcome_home), win_diff = actual_wins - expected_wins, count = n()) %>% 
  arrange(desc(win_diff))

strength_of_record <- full_join(lhs.sor, rhs.sor, by = "team") %>% 
  group_by(team) %>% 
  summarise(expected_wins = sum(expected_wins.x,expected_wins.y, na.rm = T), 
            actual_wins = sum(actual_wins.x,actual_wins.y, na.rm = T),
            win_diff = sum(win_diff.x, win_diff.y,na.rm = T),
            count = sum(count.x, count.y, na.rm = T)) %>% 
  mutate(win_perc_above_exp = (actual_wins - expected_wins)/(expected_wins))

team_strength_of_record_tbl <- strength_of_record %>% 
  select(-win_perc_above_exp) %>% 
  arrange(desc(win_diff)) %>% 
  mutate(rank_difference = row_number()) %>% 
  relocate(rank_difference, team, actual_wins, expected_wins) %>% 
  slice_max(order_by = win_diff, n = 25) %>% 
  gt() %>% 
  tab_header(title = "2020 Strength of Record",
             subtitle = "Difference between expected wins and actual wins") %>% 
  cols_label(team = "Team", expected_wins = "Expected Wins", actual_wins = "Actual Wins", win_diff = "Wins Above Expectation", rank_difference = "SOR Rank", count = "Games Played") %>% 
  fmt_number(vars(expected_wins, win_diff), decimals = 1, use_seps = FALSE) %>% 
  data_color(columns = vars(win_diff), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = team_strength_of_record_tbl, 
       filename = paste0("team_strength_of_record_tbl_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")


# Power Rankings (Total Win Elo - Total Loss Elo) -------------------------

lhs.pow <- games.final %>% 
  filter(season == 2020, !(is.na(home_points) == T & is.na(away_points) == T)) %>% 
  group_by(home_team) %>% 
  rename(opponent = away_team, team = home_team) %>% 
  mutate(power_elo = case_when(game_outcome_home == 1 ~ away_elo/1500,
                               TRUE ~ -1500/away_elo)) %>% 
  summarise(power_elo = sum(power_elo, na.rm = T), count = n()) %>% 
  arrange(desc(power_elo))

rhs.pow <- games.final %>% 
  filter(season == 2020, !(is.na(home_points) == T & is.na(away_points) == T)) %>% 
  group_by(away_team) %>% 
  rename(opponent = home_team, team = away_team) %>% 
  mutate(power_elo = case_when(game_outcome_home == 0 ~ home_elo/1500,
                               TRUE ~ -1500/home_elo)) %>% 
  summarise(power_elo = sum(power_elo, na.rm = T), count = n()) %>% 
  arrange(desc(power_elo))

power_elo <- full_join(lhs.pow, rhs.pow, by = "team") %>% 
  group_by(team) %>% 
  summarise(power_elo = sum(power_elo.x, power_elo.y, na.rm = T),
            count = sum(count.x, count.y, na.rm = T),
            ratio = power_elo/count,
            avg_elo = ratio*1500)
