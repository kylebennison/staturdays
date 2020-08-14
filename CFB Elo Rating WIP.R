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

staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          axis.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 15)
)

# Pull in Games Data ------------------------------------------------------

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC", "Pac-10")
group_of_5 <- c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt")

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

# Pull in conference data for each year for all teams (FBS only)
conference_url <- "https://api.collegefootballdata.com/teams/fbs?year="
conference.master = data.frame()
for (j in 2000:2019) {
    cat('Loading Conferences ', j, '\n')
    full_url_games <- paste0(conference_url, as.character(j))
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games <- games %>% mutate(year = j)
    conference.master = rbind(conference.master, games)
}


# Get every team's conference in the year 2000 (initialization year)
conferences_2000 <- conference.master %>% filter(year == 2019) %>% select(school, conference)

#keep track of predictions 
k_optimization <- tibble(HomeWin=0, HomeExpectedWin=0, home_spread = 0, elo_diff = 0, Year=0000, kval = k, regress_val = regress, home_field_val = home_field_advantage, g5_val = g5, d3_val = d3)

#for(g5 in seq(1000, 1500, by = 100)){
#  for(d3 in seq(1000, 1500, by = 100)){
    
    for(g5 in c(1200)){
      for(d3 in c(500)){

# Get all unique teams from the games database, join in conference, and then assign an initial Elo Rating
unique_teams <- as_tibble(unique(c(unique(unique(c(unique(games.master$home_team),
                                         unique(games.master$away_team))))))) %>% 
  left_join(conferences_2000, by = c("value" = "school")) %>% 
  mutate(conference_class = case_when(conference %in% power_5 ~ 1500,
                                      conference %in% group_of_5 ~ g5,
                                      conference %in% "FBS Independents" ~ 1500,
                                      TRUE ~ d3))


# Calculate Initial Elo Rating and set up table to store data
teams_elo_initial <- unique_teams %>% select(value, conference, conference_class) %>% 
  rename(elo_rating = conference_class) %>% 
           rename(team = value) %>% 
  mutate(week = 0, season = min(games.master$season), date = as.Date(ymd_hms(min(games.master$start_date))) - 7)


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

# k=100 seems good .18, for regress - .176 for .9 (2010), and .179 (2000), test k again - .176 for 75 and 100, test home_field_adv - .176 for 50 and 65, 55 is min at .1758
# for(regress in c(seq(.9, 1, by = 0.01))){
elo_ratings <- teams_elo_initial
# message(paste0("Testing values: ", "hfa = ", home_field_advantage, " k =", k, " regress = ", regress))

#### updated for loop to speed up process ####
for(yr in c(2000:2019)){
  message(paste0("Calculating elo ratings for: "),yr, "D3: ", d3, "G5: ", g5)
  #regress Elo ratings before the first season of the year
  if(yr != min(cfb_games$season)){
  preseason_elo <- elo_ratings %>% group_by(team) %>% 
    slice(which.max(date)) %>% 
    #briefly bring in the original rankings in order to get the 
    #conference regression values
    left_join(unique_teams, by=c("team" = "value")) %>% 
    #team elo rating week season date
    mutate(elo_rating = elo_rating*(regress)+conference_class*(1-regress),
           week = 0,
           season=yr,
           date=ymd(paste0(yr,"-08-15"))) %>% 
    select(team, conference.x, elo_rating, week, season, date) %>% 
    rename(conference = conference.x)
  
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
      k_optimization_temp <- current_week %>% 
        mutate(HomeExpectedWin=calc_expected_score((home_rating+home_field_advantage), away_rating)) %>% 
        select(game_outcome_home, HomeExpectedWin, home_points, away_points, home_rating, away_rating) %>% 
        rename(HomeWin = game_outcome_home) %>% 
        mutate(home_spread = home_points - away_points,
               elo_diff = home_rating - away_rating,
               Year=yr,
               kval = k,
               regress_val = regress,
               home_field_val = home_field_advantage,
               g5_val = g5,
               d3_val = d3)
  
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
}

# Calc mean predicted vs. mean actual, and Brier
k_optimization %>% 
  filter(Year >= 2010) %>%  
  mutate(error = (HomeWin - HomeExpectedWin)^2) %>% 
  summarise(mean_pred = mean(HomeExpectedWin), mean_actual = mean(HomeWin), Brier = mean(error), sum_win = sum(HomeWin), count = n())

#Calculates the brier score
brier <- k_optimization %>% mutate(error=(HomeWin-HomeExpectedWin)^2) %>% 
  filter(Year>=2010) %>% 
  group_by(kval, home_field_val, regress_val, g5_val, d3_val) %>% 
  summarise(e=mean(error))

# write_csv(brier, path = "C:/Users/Kyle/Documents/Kyle/Staturdays/Data/elo g5 d3 initial 8.13.20.csv")

# See which regress value optimizes Brier the most
k_optimization %>% mutate(error=(HomeWin-HomeExpectedWin)^2) %>% 
  filter(Year>=2010) %>% 
  group_by(regress_val) %>% 
  summarise(e=mean(error)) %>% 
  ggplot(aes(x = regress_val, y = e)) +
  geom_line()

# Get Actual vs. Predicted for Each Win Prob.
actual_vs_predicted_plot <- k_optimization %>% filter(Year >= 2010) %>% mutate(win_prob_bucket = round(HomeExpectedWin, 2), error = (HomeWin - HomeExpectedWin)^2) %>% 
  group_by(win_prob_bucket) %>% 
  summarise(mean_actual_score = mean(HomeWin), mse = mean(error), count= n()) %>% 
  ggplot() +
  geom_point(aes(x = win_prob_bucket, y = mean_actual_score), color = staturdays_colors("medium_blue"), alpha = .5, size = 3) +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    x = "Predicted Win Probability",
    y = "Actual Average Wins",
    title = "Accuracy of Win Probabilities \nUsing Elo Ratings",
    subtitle = "Across 16,000 games from 2000 to 2019",
    caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  staturdays_theme +
  theme(plot.title = element_text(size = 25)) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = percent)

ggsave(filename = "actualvspredict_historic.png", 
       plot = actual_vs_predicted_plot, 
       path = "/Users/kylebennison/Documents/Documents/Kyle/Staturdays/R Plots/",
       width = 200,
       height = 200,
       units = "mm"
       )

# Smooth representation of predictions
k_optimization %>% 
  mutate(win_prob_bucket = round(HomeExpectedWin, 2), error = (HomeWin - HomeExpectedWin)^2) %>% 
  ggplot() + 
  geom_smooth(aes(x = HomeExpectedWin, y = HomeWin)) + 
  geom_abline(slope = 1, intercept = 0)

# Table of the same thing
actual_vs_predict <- k_optimization %>% mutate(win_prob_bucket = round(HomeExpectedWin, 2), error = (HomeWin - HomeExpectedWin)^2) %>% 
  group_by(win_prob_bucket) %>% 
  summarise(mean_actual_score = mean(HomeWin), mse = mean(error), root_mse = sqrt(mse), count= n())

# Linear Regression to get standard error ## NOT SURE IF THIS IS ACCURATE
summary(lm(mean_actual_score ~ win_prob_bucket, {k_optimization %>% mutate(win_prob_bucket = round(HomeExpectedWin, 2), error = (HomeWin - HomeExpectedWin)^2) %>% 
    group_by(win_prob_bucket) %>% 
    summarise(mean_actual_score = mean(HomeWin), mse = mean(error), count= n())}))

summary(lm(HomeWin ~ HomeExpectedWin, {k_optimization %>% filter(Year >= 2010)}))
# For each 10% increase in expected win probability, actual wins increase 9%.

# Convert Elo to an Implied Point Spread WIP ------------------------------

## Thought: maybe I need to do this backwards, and use the historic betting database, put that up against the win probability, and get a 
## implied win probability from actual betting lines. Then use that to find value. Because right now the spreads are all over the place.

# Calculate mean point spread by prediction bucket and elo diff
k_optimization %>% mutate(win_prob_bucket = round(HomeExpectedWin, 2), error = (HomeWin - HomeExpectedWin)^2) %>% 
  group_by(win_prob_bucket) %>% 
  summarise(mean_point_spread = mean(home_spread), mean_elo_diff = mean(elo_diff), mse = mean(error), root_mse = sqrt(mse), count= n())

summary(lm(home_spread ~ elo_diff, {k_optimization %>% filter(Year >= 2010)}))
### So this makes the implied spread formula (elo_diff (home - away) / 20.14) + 4.69. This would imply 4.7 points of home field advantage which seems high.

# Implied point spread and mean actual spread
k_optimization %>% mutate(
implied_spread = 4.6899972 + 0.0496551 * elo_diff
) %>% 
  mutate(implied_spread_bucket = round(implied_spread, 0)) %>% 
  group_by(implied_spread_bucket) %>% 
  summarise(mean_home_spread = mean(home_spread), count = n()) %>% 
  ggplot() +
  geom_point(aes(x = implied_spread_bucket, y = mean_home_spread))

# Difference in Elo and Actual Spread
k_optimization %>% 
  ggplot() +
  geom_point(aes(x = elo_diff, y = home_spread))

# Graphs ------------------------------------------------------------------

# Elo of the top 10 teams in average Elo all-time
elo_ratings %>% 
  filter(team %in% {elo_ratings %>% group_by(team) %>% summarise(avg_elo = mean(elo_rating)) %>% slice_max(order_by = avg_elo, n=10)}$team) %>% 
  ggplot(aes(date, elo_rating, colour = team)) +
  geom_line()

# Elo of Penn State
elo_ratings %>% 
  filter(team %in% "Penn State", season == 2019) %>% 
  ggplot(aes(date, elo_rating, colour = team)) +
  geom_line()

# Function that creates an Elo plot of two teams for a set date range
Elo_head_to_head <- function(team_a, team_b, start_season=min(elo_ratings$season), end_season=max(elo_ratings$season)){
  elo_ratings %>% 
    filter(team %in% c(team_a, team_b), season >= start_season & season <= end_season) %>% 
    ggplot(aes(date, elo_rating, colour = team)) +
    geom_line() +
    labs(
      x = "Date",
      y = "Elo Rating",
      color = "Team"
    )
}

Elo_head_to_head("LSU", "Alabama", 2010, 2020)

elo_h2h_plot <- Elo_head_to_head("LSU", "Clemson", 2019, 2019) + 
  labs(
    title = "LSU's Historic Climb to the \n2019 CFP Championship",
    subtitle = "LSU climbed as they beat highly rated opponents, \nwhile Clemson flatlined",
    caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  staturdays_theme +
  scale_color_manual(values = c("#F66733", "#461D7C")) +
  theme(legend.position = c(.85, .25),
        legend.background = element_blank(),
        legend.text = element_text(color = staturdays_colors("dark_blue")),
        legend.title = element_text(color = staturdays_colors("dark_blue")))
  
ggsave(filename = "lsu_2019_elo.png", 
       plot = elo_h2h_plot, 
       path = "/Users/kylebennison/Documents/Documents/Kyle/Staturdays/R Plots/",
       width = 200,
       height = 200,
       units = "mm"
)

# Predict Upcoming Week Outcomes ------------------------------------------

upcoming.games = data.frame()
for (j in 2020:2020) {
  for (i in 1:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    upcoming.games = rbind(upcoming.games, games)
  }
}

current_elo_ratings_a <- current_elo_ratings %>% select(team, elo_rating)

upcoming.games <- left_join(upcoming.games, current_elo_ratings_a, by = c("home_team" = "team")) %>% 
  rename(home_elo = elo_rating)

upcoming.games <- left_join(upcoming.games, current_elo_ratings_a, by = c("away_team" = "team")) %>% 
  rename(away_elo = elo_rating)

# Get win prob
upcoming.games <- upcoming.games %>% 
  filter(week == 1) %>% 
  mutate(home_pred_win_prob = calc_expected_score(home_elo, away_elo), away_pred_win_prob = 1 - home_pred_win_prob)

# Table of win probabilities for the week
upcoming.games %>% 
  filter(week == 1) %>% 
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
