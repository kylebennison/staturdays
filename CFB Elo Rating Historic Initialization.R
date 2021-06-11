# Libraries and Themes ----------------------------------------------------

library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(jsonlite)
library(stringr)
library(lubridate)
library(gt)
library(data.table)
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/cfbd_api_key_function.R")

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

# Pull in Games Data ------------------------------------------------------

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC", "Pac-10", "FBS Independents")
group_of_5 <- c("American Athletic", "Conference USA", "Mid-American", "Mountain West", "Sun Belt")

base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data

games.master = tibble()
for (j in 2000:2020) {
  for (i in 1:20) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    games <- cfbd_api(full_url_games, my_key)
    games.master = rbind(games.master, games)
  }
}

# Pull in conference data for each year for all teams (FBS only)
conference_url <- "https://api.collegefootballdata.com/teams/fbs?year="
conference.master = data.frame()
for (j in 2000:2020) {
    cat('Loading Conferences ', j, '\n')
    full_url_conf <- paste0(conference_url, as.character(j))
    conf <- cfbd_api(full_url_conf, my_key)
    conf <- conf %>% mutate(year = j)
    conference.master = rbind(conference.master, conf)
}

# Unlist line scores in games dataframe
games.master$home_line_scores <- vapply(games.master$home_line_scores, paste, collapse = ", ", character(1L))
games.master$away_line_scores <- vapply(games.master$away_line_scores, paste, collapse = ", ", character(1L))

# Ensure all games.master start_date values are datetimes
games.master <- games.master %>% 
  mutate(start_date = as_datetime(start_date))

# Get every team's conference in the latest year
conferences_latest <- conference.master %>% filter(year == max(year)) %>% select(school, conference)

#keep track of predictions 
## All factors have been tested and optimized
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
k_optimization <- tibble(HomeWin=0, HomeExpectedWin=0, home_spread = 0, elo_diff = 0, Year=0000, 
                         week=0, home_conf = "", home_team = "", away_team = "", away_conf = "", 
                         kval = k, regress_val = regress, home_field_val = home_field_advantage, 
                         g5_val = g5, d3_val = d3, neutral_adjust)

#for(g5 in seq(1000, 1500, by = 100)){
#  for(d3 in seq(800, 1500, by = 100)){

# Get all unique teams from the games database, join in conference, and then assign an initial Elo Rating
unique_teams <- as_tibble(unique(c(unique(unique(c(unique(games.master$home_team),
                                         unique(games.master$away_team))))))) %>% 
  left_join(conferences_latest, by = c("value" = "school")) %>% 
  mutate(conference_class = case_when(conference %in% power_5 ~ 1500,
                                      conference %in% group_of_5 ~ g5,
                                      conference %in% "FBS Independents" ~ 1500,
                                      TRUE ~ d3))


# Calculate Initial Elo Rating and set up table to store data
teams_elo_initial <- unique_teams %>% select(value, conference, conference_class) %>% 
  rename(elo_rating = conference_class) %>% 
           rename(team = value) %>% 
  mutate(week = 0, season = min(games.master$season), date = ymd_hms(min(games.master$start_date) - 7))


#Select variables we want
cfb_games <- games.master %>% select(id, season, week, season_type, home_team, home_conference, away_team, away_conference, home_points, away_points, start_date, neutral_site, conference_game) %>% 
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

## Postseason week adjustment

cfb_games_final <- tibble()
for(yr in min(cfb_games$season):max(cfb_games$season)) {
  message("working on season ", yr)
  
# Get Week 1 epiweek for referencing later weeks
  cfb_games_temp <- cfb_games %>% filter(season == yr)
  
  week_1_epiweek <- cfb_games_temp %>% filter(week == min(week)) %>% slice_min(date) %>% pull(date) %>% unique() %>% epiweek() %>% as.integer()

# Figure out what week the first week of postseason play should be
  postseason_start_week <- cfb_games_temp %>% filter(season_type == "regular", week == max(week)) %>% 
    pull(week) %>% unique() %>% as.integer() + 1L

  postseason_start_epiweek <- cfb_games_temp %>% filter(season_type == "postseason") %>% slice_min(date) %>% 
    pull(date) %>% unique() %>% epiweek() %>% as.integer()
  
  postseason_start_date <- cfb_games_temp %>% filter(season_type == "postseason") %>% slice_min(date) %>% 
    pull(date) %>% unique()
  
  if(is_empty(postseason_start_epiweek) == F){
    ### Adjust postseason games to correct week
    cfb_games_temp <- cfb_games_temp %>%
      mutate(week = if_else(season_type == "postseason", as.integer(postseason_start_week + difftime(date, postseason_start_date, units = "weeks") %>% floor() %>% as.integer()), as.integer(week))) 
  }
  cfb_games_final <- rbind(cfb_games_final, cfb_games_temp)
  }

cfb_games <- cfb_games_final

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

# Make sure data is in the right order to run calculation by row
cfb_games <- cfb_games %>% 
  arrange(season, week, date)

# Fix NA's for conference_game
cfb_games <- cfb_games %>% mutate(conference_game = 
                                    if_else(is.na(conference_game) == T, 
                                            case_when(home_conference == away_conference ~ TRUE,
                                                      TRUE ~ FALSE),
                                            conference_game))

# k=100 seems good .18, for regress - .176 for .9 (2010), and .179 (2000), test k again - .176 for 75 and 100, test home_field_adv - .176 for 50 and 65, 55 is min at .1758
# for(k in c(seq(60, 90, by = 5))){
# elo_ratings <- teams_elo_initial
# message(paste0("Testing values: \n", "k = ", k))

#### updated for loop to speed up process ####
for(yr in c(2000:2020)){
  message(paste0("Calculating elo ratings for year: "),yr, " D3: ", d3, " G5: ", g5)
  #regress Elo ratings before the first season of the year
  if(yr != min(cfb_games$season)){
  preseason_elo <- elo_ratings %>% group_by(team) %>% 
    slice(which.max(date)) %>% 
    #briefly bring in the original rankings in order to get the 
    #conference regression values
    left_join(unique_teams, by=c("team" = "value")) %>% 
    #team elo rating week season date
    mutate(elo_rating = elo_rating*(regress)+conference_class*(1-regress),
           conference = conference.x,
           week = 0,
           season=yr,
           date=ymd_hms(paste0(yr,"-08-15 00:00:00"))) %>% 
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
      current_week <- current_week %>% mutate(new_home_rating = calc_new_elo_rating(home_rating, game_outcome_home, calc_expected_score((home_rating+if_else(neutral_site == F, home_field_advantage, neutral_adjust)), away_rating),k),
                                              new_away_rating = calc_new_elo_rating(away_rating, 1-game_outcome_home, calc_expected_score(away_rating, (home_rating+if_else(neutral_site == F, home_field_advantage, neutral_adjust))),k))
      
      #keep track of predictions and actual results
      k_optimization_temp <- current_week %>% 
        mutate(HomeExpectedWin=calc_expected_score((home_rating+if_else(neutral_site == F, home_field_advantage, neutral_adjust)), away_rating)) %>% 
        select(game_outcome_home, HomeExpectedWin, home_conference, home_team, away_conference, away_team, home_points, away_points, home_rating, away_rating) %>% 
        rename(HomeWin = game_outcome_home) %>% 
        mutate(home_spread = home_points - away_points,
               elo_diff = home_rating - away_rating,
               Year=yr,
               week = wk,
               home_conf = home_conference,
               home_team = home_team,
               away_team = away_team,
               away_conf = away_conference,
               kval = k,
               regress_val = regress,
               home_field_val = home_field_advantage,
               g5_val = g5,
               d3_val = d3,
               neutral_adjust = neutral_adjust)
  
      k_optimization <- k_optimization %>% bind_rows(k_optimization_temp)
      
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
      
      elo_ratings <- elo_ratings %>% 
        bind_rows(updated_ratings_home) %>% 
        bind_rows(updated_ratings_away)
    }
    
  }
}
# }
#  }
#}
  

# Analyze results of Elo --------------------------------------------------


# Calc mean predicted vs. mean actual, and Brier
k_optimization %>% 
  filter(Year >= 2010) %>%  
  mutate(error = (HomeWin - HomeExpectedWin)^2) %>% 
  summarise(mean_pred = mean(HomeExpectedWin), mean_actual = mean(HomeWin), Brier = mean(error), sum_win = sum(HomeWin), count = n())

#Calculates the brier score
brier <- k_optimization %>% mutate(error=(HomeWin-HomeExpectedWin)^2) %>% 
  filter(Year>=2010) %>% 
  group_by(kval, home_field_val, regress_val, g5_val, d3_val, neutral_adjust) %>% 
  summarise(e=mean(error))

# See which regress value optimizes Brier the most
k_optimization %>% mutate(error=(HomeWin-HomeExpectedWin)^2) %>% 
  filter(Year>=2010) %>% 
  group_by(regress_val) %>% 
  summarise(e=mean(error)) %>% 
  ggplot(aes(x = neutral_adjust, y = e)) +
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

# Evaluate Elo on any given criteria to group by
k_optimization %>% mutate(error=(HomeWin-HomeExpectedWin)^2) %>% 
  filter(Year>=2010) %>% 
  group_by(week) %>% 
  summarise(e=mean(error), count = n())

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


# Write to Github ---------------------------------------------------------

# Write historic calculations to github for the first time
#fwrite(elo_ratings, file = "C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays/elo_ratings_historic.csv", append = FALSE, col.names = TRUE)

# write_csv(brier, path = "C:/Users/Kyle/Documents/Kyle/Staturdays/Data/elo g5 d3 initial 8.13.20.csv")
