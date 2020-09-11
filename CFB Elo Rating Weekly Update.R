# Libraries and Themes ----------------------------------------------------

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
  for (i in 1:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

## Update this value each week before running script
week_of_games_just_played <- 1
week_of_upcoming_games <- week_of_games_just_played + 1

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

# Predict Upcoming Week Outcomes ------------------------------------------
upcoming.games = cfb_games
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

# Regress ratings if it's a new season
if (today()-max(elo_ratings$date) > 90){
  preseason_elo <- elo_conf %>% group_by(team) %>% 
    slice(which.max(date)) %>% 
    mutate(elo_rating = elo_rating*(regress)+conference_class*(1-regress),
           conference = conference,
           week = 0,
           season=j,
           date=ymd(paste0(j,"-08-15"))) %>% 
    select(-conference_class)
  elo_ratings <- elo_ratings %>% 
    bind_rows(preseason_elo)
  fwrite(preseason_elo, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/elo_ratings_historic.csv", append = TRUE, col.names = FALSE)
}
# Get most updated rating for each team
current_elo_ratings <- elo_ratings %>% group_by(team) %>% slice_max(order_by = date, n = 1)

# Take just team and rating
current_elo_ratings_only <- current_elo_ratings %>% select(team, elo_rating)

# Join upcoming schedule with elo ratings for home and away teams
upcoming.games <- left_join(upcoming.games, current_elo_ratings_only, by = c("home_team" = "team")) %>% 
  rename(home_elo = elo_rating)

upcoming.games <- left_join(upcoming.games, current_elo_ratings_only, by = c("away_team" = "team")) %>% 
  rename(away_elo = elo_rating)

## Add Elo rating for teams with NA elo rating (never been rated before)
# Home
upcoming.games <- upcoming.games %>% mutate(
  elo_final_home = 
    case_when(is.na(home_elo) == T ~ case_when(home_conference %in% power_5 ~ 1500,
                                                              home_conference %in% group_of_5 ~ g5,
                                                              TRUE ~ d3),
                             TRUE ~ home_elo)) %>% 
  select(-home_elo) %>% 
  rename(home_elo = elo_final_home)

# Away
upcoming.games <- upcoming.games %>% mutate(
  elo_final_away = 
    case_when(is.na(away_elo) == T ~ case_when(away_conference %in% power_5 ~ 1500,
                                               away_conference %in% group_of_5 ~ g5,
                                               TRUE ~ d3),
              TRUE ~ away_elo)) %>% 
  select(-away_elo) %>% 
  rename(away_elo = elo_final_away)

# Get win prob
upcoming.games <- upcoming.games %>% 
  mutate(home_pred_win_prob = calc_expected_score(home_elo, away_elo), away_pred_win_prob = 1 - home_pred_win_prob)


# Pull in Last Week Results and Update Elo --------------------------------

# Rename games table
cfb_games <- lastweek.games

### Start calculation for the week
current_week <- cfb_games %>% filter(week == week_of_games_just_played)
current_week <- current_week %>% left_join(current_elo_ratings,
                                           by=c("home_team"="team")) %>% 
  left_join(current_elo_ratings, by=c("away_team"="team")) %>% 
  rename(home_rating = elo_rating.x) %>% 
  rename(away_rating = elo_rating.y) %>% 
  rename(game_date = date.x) %>% 
  rename(home_rating_last_updated = date.y) %>%
  rename(away_rating_last_updated = date)

# only run if it's not preseason, and make sure the results are not calculated twice (game date == last updated date)
if(week_of_games_just_played > 0 & !any(current_week$game_date == current_week$home_rating_last_updated) & !any(current_week$game_date == current_week$away_rating_last_updated)){ #& (length(current_week$home_points) != length(is.na(current_week$home_points)))){

#calculate new ratings after game
current_week <- current_week %>% mutate(new_home_rating = calc_new_elo_rating(home_rating, game_outcome_home, calc_expected_score((home_rating+home_field_advantage), away_rating),k),
                                        new_away_rating = calc_new_elo_rating(away_rating, 1-game_outcome_home, calc_expected_score(away_rating, (home_rating+home_field_advantage)),k))

#keep track of predictions and actual results
k_optimization_temp <- current_week %>% mutate(HomeExpectedWin=calc_expected_score((home_rating+home_field_advantage), away_rating)) %>% 
  select(game_outcome_home, HomeExpectedWin) %>% 
  rename(HomeWin = game_outcome_home) %>% 
  mutate(Year=j,k_val = k,
         regress_val = regress,
         home_field_val = home_field_advantage)

# k_optimization <- k_optimization %>% bind_rows(k_optimization_temp)

#home team elo update
updated_ratings_home <- current_week %>% select(home_team, home_conference, new_home_rating, week.x, season.x, game_date) %>% 
  rename(team=home_team) %>% 
  rename(conference = home_conference) %>% 
  rename(elo_rating = new_home_rating) %>% 
  rename(week = week.x) %>%
  rename(season = season.x) %>% 
  rename(date = game_date)

#away team elo update
updated_ratings_away <- current_week %>% select(away_team, away_conference, new_away_rating, week.x, season.x, game_date) %>% 
  rename(team=away_team) %>% 
  rename(conference = away_conference) %>% 
  rename(elo_rating = new_away_rating) %>% 
  rename(week = week.x) %>%
  rename(season = season.x) %>% 
  rename(date = game_date)

# Save updated home and away elo ratings for the completed week
elo_ratings_updated <- updated_ratings_home %>% 
  bind_rows(updated_ratings_away)

# Update elo ratings internally
elo_ratings <- elo_ratings %>% 
  bind_rows(updated_ratings_home) %>% 
  bind_rows(updated_ratings_away)

# Write new data to github
fwrite(elo_ratings_updated, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/elo_ratings_historic.csv", append = TRUE, col.names = FALSE)
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

### Tables

# Table of preseason Elo Ratings
home_stats <- upcoming.games %>% 
  group_by(home_team) %>% 
  summarise(elo = max(home_elo), expected_wins = sum(home_pred_win_prob), n_games = n())

away_stats <- upcoming.games %>% 
  group_by(away_team) %>% 
  summarise(elo = max(away_elo), expected_wins = sum(away_pred_win_prob), n_games = n())

# Right now, I am removing games where the opponent's elo is NA, so the expected_win value is NA, but this needs to be resolved. Only affects 3 teams.
joined_stats <- left_join(home_stats, away_stats, by = c("home_team" = "away_team")) %>% 
  group_by(home_team) %>% 
  summarise(elo = max(elo.x, elo.y), expected_wins = sum(expected_wins.x, expected_wins.y, na.rm = T), n_games = sum(n_games.x, n_games.y), win_rate = expected_wins / n_games)

# Get just teams and conferences from 2019
conf_most_recent <- elo_conf %>% 
  filter(date == max(date)) %>% 
  select(team, conference) %>% 
  mutate(conference = if_else(is.na(conference) == T, "Non-FBS", conference))

# Add conference to joined_stats
joined_stats <- left_join(joined_stats, conf_most_recent, by = c("home_team" = "team"))

# Preseason rankings - all teams
preseason_2020_rankings <- joined_stats %>% 
  arrange(desc(elo)) %>% 
  mutate(row_num = row_number()) %>% 
  relocate(row_num) %>% 
  select(-n_games) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Preason Elo Ratings and Expected Wins"),
             subtitle = "Expected Wins Based on head-to-head Elo Ratings") %>% 
  cols_label(row_num = "Rank", home_team = "Team", elo = "Elo Rating", expected_wins = "Expected Wins", win_rate = "Win Percentage", conference = "Conference") %>% 
  fmt_number(vars(elo, expected_wins), decimals = 2, use_seps = FALSE) %>% 
  fmt_percent(vars(win_rate), decimals = 2) %>% 
  data_color(columns = vars(elo, expected_wins), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

# Top 25
preseason_2020_top_25 <- joined_stats %>% 
  arrange(desc(elo)) %>% 
  mutate(row_num = row_number()) %>% 
  relocate(row_num) %>% 
  select(-n_games) %>% 
  filter(row_num <= 25) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Preason Elo Ratings and Expected Wins"),
             subtitle = "Expected Wins Based on head-to-head Elo Ratings") %>% 
  cols_label(row_num = "Rank", home_team = "Team", elo = "Elo Rating", expected_wins = "Expected Wins", win_rate = "Win Percentage", conference = "Conference") %>% 
  fmt_number(vars(elo, expected_wins), decimals = 2, use_seps = FALSE) %>% 
  fmt_percent(vars(win_rate), decimals = 2) %>% 
  data_color(columns = vars(elo, expected_wins), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = preseason_2020_top_25, 
       filename = paste0("preseason_2020_top_25_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

# Only conferences that are playing as of right now - top 25
preseason_2020_secaccbig12_top25 <- joined_stats %>% 
  filter(conference %in% c("SEC", "ACC", "Big 12", "American Athletic", "Conference USA", "Sun Belt") | home_team == "Notre Dame") %>% 
  arrange(desc(elo)) %>% 
  mutate(row_num = row_number()) %>% 
  relocate(row_num) %>% 
  select(-n_games) %>% 
  filter(row_num <= 25) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Preason Elo Ratings and Expected Wins"),
             subtitle = "Expected Wins Based on head-to-head Elo Ratings. Only conferences playing in the fall.") %>% 
  cols_label(row_num = "Rank", home_team = "Team", elo = "Elo Rating", expected_wins = "Expected Wins", win_rate = "Win Percentage", conference = "Conference") %>% 
  fmt_number(vars(elo, expected_wins), decimals = 2, use_seps = FALSE) %>% 
  fmt_percent(vars(win_rate), decimals = 2) %>% 
  data_color(columns = vars(elo, expected_wins), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = preseason_2020_secaccbig12_top25, 
       filename = "2020_preseason_elo_rankings_secaccbig12_top25_8.31.20.png",
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

# See what conference has the toughest opponent elo ratings
lhs.tmp <- upcoming.games %>% group_by(home_conference) %>% filter(home_conference != away_conference) %>% summarise(m_away_elo = mean(away_elo), count = n()) %>% arrange(m_away_elo)
rhs.tmp <- upcoming.games %>% group_by(away_conference) %>% filter(home_conference != away_conference) %>% summarise(m_home_elo = mean(home_elo), count = n()) %>% arrange(m_home_elo)
conf_non_conf_SOS <- left_join(lhs.tmp, rhs.tmp, by = c("home_conference" = "away_conference")) %>% mutate(avg.elo = (m_away_elo*count.x + m_home_elo*count.y)/(count.x+count.y)) %>% arrange(avg.elo)

# Table
conf_non_conf_sos_tbl <- conf_non_conf_SOS %>% 
  select(home_conference, avg.elo) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Non-Conference Strength of Schedule"),
             subtitle = "Average Elo of opponents in non-conference games, by conference.") %>% 
  cols_label(home_conference = "Conference", avg.elo = "Average Opponent Elo") %>% 
  fmt_number(vars(avg.elo), decimals = 2, use_seps = FALSE) %>% 
  data_color(columns = vars(avg.elo), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = conf_non_conf_sos_tbl, 
       filename = "2020_preseason_conf_non_conf_sos_tbl_8.31.20.png",
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

# Table of win probabilities for the week
win_probabilities_this_week <- upcoming.games %>% 
  filter(week == week_of_upcoming_games) %>% 
  select(home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference) %>%
  arrange(desc(home_pred_win_prob)) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Win Probabilities"),
             subtitle = "Based on head-to-head Elo Ratings") %>% 
    tab_spanner(label = "Home", # Add a column spanning header
                columns = vars(home_team,home_elo, home_pred_win_prob, home_conference)) %>% 
    tab_spanner(label = "Away", # Add a column spanning header
                columns = vars(away_team, away_elo, away_pred_win_prob, away_conference)) %>% 
  cols_label(home_team = "Team", home_elo = "Elo Rating", home_pred_win_prob = "Win Probability", home_conference = "Conference",
             away_team = "Team", away_elo = "Elo Rating", away_pred_win_prob = "Win Probability", away_conference = "Conference") %>% 
  fmt_percent(columns = vars(home_pred_win_prob, away_pred_win_prob), decimals = 1) %>% 
  fmt_number(vars(home_elo, away_elo), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(home_pred_win_prob, away_pred_win_prob), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
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
        columns = vars(away_team)
      )
    )
  ) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = win_probabilities_this_week, 
       filename = paste0("win_probabilities_this_week_", week_of_upcoming_games, "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

## Calculate biggest upsets week-over-week by win prob and change in Elo

home_wow_elo_change <- elo_ratings %>% 
  filter(season == max(season)) %>% 
  arrange(desc(date)) %>% 
  group_by(team) %>% 
  mutate(wow_change = ((elo_rating) - lag(elo_rating, n = 1, order_by = date))/(lag(elo_rating, n = 1, order_by = date)), previous_elo = lag(elo_rating, n =1, order_by = date)) %>% 
  inner_join(upcoming.games, by = c("team" = "home_team", "week", "season"))

away_wow_elo_change <- elo_ratings %>% 
  filter(season == max(season)) %>% 
  arrange(desc(date)) %>% 
  group_by(team) %>% 
  mutate(wow_change = ((elo_rating) - lag(elo_rating, n = 1, order_by = date))/(lag(elo_rating, n = 1, order_by = date)), previous_elo = lag(elo_rating, n =1, order_by = date)) %>% 
  inner_join(upcoming.games, by = c("team" = "away_team", "week", "season"))

wow_elo_change <- rbind(home_wow_elo_change, away_wow_elo_change) %>% 
  select(1:8, home_team, away_team, home_points, away_points, game_outcome_home, home_pred_win_prob, away_pred_win_prob) %>% 
  mutate(home_surprise = game_outcome_home - home_pred_win_prob, away_surprise = (1-game_outcome_home) - away_pred_win_prob)

# Table of movers

wow_elo_change_tbl <- wow_elo_change %>% 
  filter(season == max(season)) %>% 
  filter(week == max(week)) %>% 
  select(team, conference, elo_rating, wow_change) %>% 
  ungroup() %>% 
  slice_max(order_by = wow_change, n = 10) %>% 
  gt() %>% 
  tab_header(title = paste0(season), " Week ", paste0(week), " Biggest Elo Movers",
             subtitle = "Largest changes in Elo") %>% 
  cols_label(home_conference = "Conference", avg.elo = "Average Opponent Elo") %>% 
  fmt_number(vars(avg.elo), decimals = 2, use_seps = FALSE) %>% 
  data_color(columns = vars(avg.elo), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

## Strength of Schedule

lhs.tmp <- upcoming.games %>% group_by(home_team) %>% 
  rename(opponent = away_team, team = home_team) %>% 
  summarise(avg_opponent_elo = mean(away_elo), elo = mean(home_elo), count = n())

rhs.tmp <- upcoming.games %>% group_by(away_team) %>% 
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
