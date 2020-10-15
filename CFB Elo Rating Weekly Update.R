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
week_of_games_just_played <- games.master %>% filter(is.na(home_points) == F & is.na(away_points) == F) %>% group_by(season) %>% slice_max(order_by = start_date, n = 1) %>% pull(week)
week_of_upcoming_games <- week_of_games_just_played + 1L

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

# Get start of season elo for each team only
preseason_elo_ratings <- elo_ratings %>% group_by(team) %>% slice_max(order_by = season, n = 1) %>% 
  filter(week == 0) %>% 
  select(team, elo_rating)

# Mutate week of elo_ratings for joining purposes
elo_ratings_tmp <- elo_ratings %>% 
  mutate(week = week + 1)

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
  mutate(home_pred_win_prob = calc_expected_score(home_elo+home_field_advantage, away_elo), away_pred_win_prob = 1 - home_pred_win_prob)

rm(list = c("upcoming.tmp", "upcoming.tmp2", "upcoming.tmp3", "upcoming.tmp4"))
# Pull in Last Week Results and Update Elo --------------------------------

# Rename games table
cfb_games <- lastweek.games

### Start calculation for the week
current_week <- upcoming.games %>% filter(week == week_of_games_just_played)

# only run if it's not preseason, and make sure the results are not calculated twice (game date == last updated date)
if(week_of_games_just_played > 0 & !any(current_week %>% filter(!(is.na(home_points) & is.na(away_points))) %>% pull(week) == elo_ratings %>% filter(season == max(season)) %>% filter(week == max(week)) %>% pull(week))){ #& (length(current_week$home_points) != length(is.na(current_week$home_points)))){

#calculate new ratings after game
current_week <- current_week %>% mutate(new_home_rating = calc_new_elo_rating(home_elo, game_outcome_home, calc_expected_score((home_elo+home_field_advantage), away_elo),k),
                                        new_away_rating = calc_new_elo_rating(away_elo, 1-game_outcome_home, calc_expected_score(away_elo, (home_elo+home_field_advantage)),k))

#keep track of predictions and actual results
k_optimization_temp <- current_week %>% mutate(HomeExpectedWin=calc_expected_score((home_elo+home_field_advantage), away_elo)) %>% 
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
  rename(date = game_date)

#away team elo update
updated_ratings_away <- current_week %>% select(away_team, away_conference, new_away_rating, week, season, game_date) %>% 
  rename(team=away_team) %>% 
  rename(conference = away_conference) %>% 
  rename(elo_rating = new_away_rating) %>% 
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


# Rebuild upcoming.games with new Elo Ratings before updating all  --------
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
  mutate(home_pred_win_prob = calc_expected_score(home_elo+home_field_advantage, away_elo), away_pred_win_prob = 1 - home_pred_win_prob)

rm(list = c("upcoming.tmp", "upcoming.tmp2", "upcoming.tmp3", "upcoming.tmp4"))}

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
preseason_2020_rankings <- joined_stats %>% 
  arrange(desc(elo)) %>% 
  mutate(row_num = row_number()) %>% 
  relocate(row_num) %>% 
  select(-n_games) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Elo Ratings and Expected Wins"),
             subtitle = "Expected Wins Based on head-to-head Elo Ratings") %>% 
  cols_label(row_num = "Rank", home_team = "Team", elo = "Elo Rating", expected_wins = "Expected Wins", win_rate = "Win Percentage", conference = "Conference") %>% 
  fmt_number(vars(elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_number(vars(expected_wins), decimals = 1, use_seps = FALSE) %>% 
  fmt_percent(vars(win_rate), decimals = 1) %>% 
  data_color(columns = vars(elo, expected_wins), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = preseason_2020_rankings, 
       filename = paste0("preseason_2020_rankings_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

# Top 25
preseason_2020_top_25 <- joined_stats %>% 
  arrange(desc(elo)) %>% 
  mutate(row_num = row_number()) %>% 
  relocate(row_num) %>% 
  select(-n_games) %>% 
  filter(row_num <= 25) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Elo Ratings and Expected Wins"),
             subtitle = "Expected Wins Based on head-to-head Elo Ratings") %>% 
  cols_label(row_num = "Rank", home_team = "Team", elo = "Elo Rating", expected_wins = "Expected Wins", win_rate = "Win Percentage", conference = "Conference") %>% 
  fmt_number(vars(elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_number(vars(expected_wins), decimals = 1, use_seps = FALSE) %>% 
  fmt_percent(vars(win_rate), decimals = 1) %>% 
  data_color(columns = vars(elo, expected_wins), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = preseason_2020_top_25, 
       filename = paste0("preseason_2020_top_25_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")

# Weekly Win Probabilities and Bets ------------------------------------------------

betting_url <- "https://api.collegefootballdata.com/lines?year=2020&"
full_url_betting <- paste0(betting_url, "week=", as.character(week_of_upcoming_games))

betting.master = data.frame()
full_url_betting_encoded <- URLencode(full_url_betting)
betting <- fromJSON(getURL(full_url_betting_encoded))
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
  select(id, home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference) %>%
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

# Table of win probabilities for the week
win_probabilities_this_week <- win_probs_w_lines %>% 
  mutate(elo_different = if_else(elo_different == T, "Yes", "No")) %>% 
  select(home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference, formattedSpread, elo_different) %>%
  arrange(desc(home_pred_win_prob)) %>% 
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", week_of_upcoming_games, " Win Probabilities"),
             subtitle = "Based on head-to-head Elo Ratings") %>% 
  tab_spanner(label = "Home", # Add a column spanning header
              columns = vars(home_team,home_elo, home_pred_win_prob, home_conference)) %>% 
  tab_spanner(label = "Away", # Add a column spanning header
              columns = vars(away_team, away_elo, away_pred_win_prob, away_conference)) %>% 
  tab_spanner(label = "Betting",
              columns = vars(formattedSpread, elo_different)) %>% 
  cols_label(home_team = "Team", home_elo = "Elo Rating", home_pred_win_prob = "Win Probability", home_conference = "Conference",
             away_team = "Team", away_elo = "Elo Rating", away_pred_win_prob = "Win Probability", away_conference = "Conference",
             formattedSpread = "Spread", elo_different = "Elo Mismatch?") %>% 
  fmt_percent(columns = vars(home_pred_win_prob, away_pred_win_prob), decimals = 1) %>% 
  fmt_number(vars(home_elo, away_elo), decimals = 0, use_seps = FALSE) %>% 
  data_color(columns = vars(home_pred_win_prob, away_pred_win_prob), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  data_color(columns = vars(elo_different),
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
        columns = vars(away_team, formattedSpread)
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

wow_elo_change %>% arrange(desc(wow_change)) %>% filter(week == week_of_games_just_played) %>% select(-game_date.x, -home_surprise, -away_surprise, -conference, -week, -season, -game_outcome_home) %>% View()

wow_elo_change_top <- wow_elo_change %>% 
  arrange(desc(wow_change)) %>% 
  filter(week == week_of_games_just_played) %>% 
  select(-date, -home_surprise, -away_surprise, -conference, -game_outcome_home) %>% 
  ungroup() %>% 
  slice_max(order_by = wow_change, n = 10)

wow_elo_change_bottom <- wow_elo_change %>% 
  arrange((wow_change)) %>% 
  filter(week == week_of_games_just_played) %>% 
  select(-date, -home_surprise, -away_surprise, -conference, -game_outcome_home) %>% 
  ungroup() %>% 
  slice_min(order_by = wow_change, n = 10)

wow_elo_change_combined <- wow_elo_change_top %>% 
  rbind(wow_elo_change_bottom) %>% 
  arrange(desc(wow_change)) %>% 
  mutate(opponent = if_else(is.na(home_team)==T, away_team, home_team)) %>% 
  mutate(win_prob = if_else(is.na(home_team)==T, home_pred_win_prob, 1-home_pred_win_prob)) %>% 
  select(-home_team, -away_team, -home_pred_win_prob)

# Table of movers

wow_elo_change_tbl <- wow_elo_change_combined %>% 
  select(team, opponent, elo_rating, previous_elo, wow_change, win_prob) %>% 
  gt() %>% 
  tab_header(title = paste0(as.character(max(upcoming.games$season)), " Week ", as.character(week_of_games_just_played), " Biggest Elo Movers"),
             subtitle = "Largest changes in Elo") %>% 
  cols_label(team = "Team", elo_rating = "New Elo", previous_elo = "Old Elo", wow_change = "Pct. Change", opponent = "Opponent", win_prob = "Win Probability") %>% 
  fmt_number(vars(elo_rating, previous_elo), decimals = 0, use_seps = FALSE) %>% 
  fmt_percent(vars(wow_change, win_prob), decimals = 1, use_seps = FALSE) %>% 
  data_color(columns = vars(wow_change), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data")

gtsave(data = wow_elo_change_tbl, 
       filename = paste0("wow_elo_change_tbl_", week_of_games_just_played, "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")
