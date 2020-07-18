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

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

# New Season Regression Factor
regress <- (.95)
# k-factor
k <- 85
# home-field advantage (in elo points)
home_field_advantage <- 55

# Expected Score and Updated Elo Rating Functions -------------------------

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

calc_new_elo_rating <- function(team_rating, actual_score, expected_score, k=20){
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

# Predict Upcoming Week Outcomes ------------------------------------------
upcoming.games = cfb_games
# Save a version of this year's games for later
lastweek.games <- upcoming.games
# Read in historic Elo ratings
elo_ratings <- read_csv(file = "/Users/kylebennison/Documents/Documents/Kyle/Staturdays/Data/elo_ratings_historic.csv")

# Regress ratings if it's a new season
if (today()-max(elo_ratings$date) > 90){
  preseason_elo <- elo_ratings %>% group_by(team) %>% 
    slice(which.max(date)) %>% 
    mutate(elo_rating = elo_rating*(regress)+1500*(1-regress),
           week = 0,
           season=j,
           date=ymd(paste0(j,"-08-15")))
  elo_ratings <- elo_ratings %>% 
    bind_rows(preseason_elo)
  write_csv(preseason_elo, path = "/Users/kylebennison/Documents/Documents/Kyle/Staturdays/Github Repo/staturdays/elo_ratings_historic.csv", append = TRUE, col_names = FALSE)
}
# Get most updated rating for each team
current_elo_ratings <- elo_ratings %>% group_by(team) %>% slice_max(order_by = date, n = 1)

# Take just team and rating
current_elo_ratings_only <- current_elo_ratings %>% select(team, elo_rating)

upcoming.games <- left_join(upcoming.games, current_elo_ratings_only, by = c("home_team" = "team")) %>% 
  rename(home_elo = elo_rating)

upcoming.games <- left_join(upcoming.games, current_elo_ratings_only, by = c("away_team" = "team")) %>% 
  rename(away_elo = elo_rating)

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

# Save updated home and away elo ratings for the completed week
elo_ratings_updated <- updated_ratings_home %>% 
  bind_rows(updated_ratings_away)

# Update elo ratings internally
elo_ratings <- elo_ratings %>% 
  bind_rows(updated_ratings_home) %>% 
  bind_rows(updated_ratings_away)

# Write new data to github
write_csv(elo_ratings_updated, path = "/Users/kylebennison/Documents/Documents/Kyle/Staturdays/Github Repo/staturdays/elo_ratings_historic.csv", append = TRUE, col_names = FALSE)

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

# Table of win probabilities for the week
upcoming.games %>% 
  filter(week == week_of_upcoming_games) %>% 
  select(home_team,home_elo, home_pred_win_prob, home_conference, away_team, away_elo, away_pred_win_prob, away_conference) %>%
  gt() %>% 
  tab_header(title = paste0(max(upcoming.games$season), " Week ", max(upcoming.games$week), " Win Probabilities"),
             subtitle = "Based on head-to-head Elo Ratings") %>% 
    tab_spanner(label = "Home", # Add a column spanning header
                columns = vars(home_team,home_elo, home_pred_win_prob, home_conference)) %>% 
    tab_spanner(label = "Away", # Add a column spanning header
                columns = vars(away_team, away_elo, away_pred_win_prob, away_conference)) %>% 
  cols_label(home_team = "Team", home_elo = "Elo Rating", home_pred_win_prob = "Win Probability", home_conference = "Conference",
             away_team = "Team", away_elo = "Elo Rating", away_pred_win_prob = "Win Probability", away_conference = "Conference") %>% 
  fmt_percent(columns = vars(home_pred_win_prob, away_pred_win_prob), decimals = 2) %>% 
  fmt_number(vars(home_elo, away_elo), decimals = 2, use_seps = FALSE) %>% 
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

