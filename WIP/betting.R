# Title: Betting

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

# API wrapper
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
# Staturdays themes + colors
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")


# Data --------------------------------------------------------------------

# Games Historic

games <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/games_historic.csv")

games_record <- games %>% 
  group_by(home_team) %>% 
  mutate(last_8_home_points = frollmean(home_points, n = 8L),
         last_8_home_points = lag(last_8_home_points, n = 1L)) %>% # Use previous week's 8-game moving average since this week's includes the result of today's game
  group_by(away_team) %>% 
  mutate(last_8_away_points = frollmean(away_points, n = 8L),
         last_8_away_points = lag(last_8_away_points, n = 1L))

games_record_home_away <- games_record %>% 
  pivot_longer(cols = c(home_team, away_team), 
               names_to = "home_away", 
               values_to = "team") %>% 
  group_by(team) %>% 
  mutate(last_8_points = frollmean(if_else(home_away == "home_team", 
                                           home_points, 
                                           away_points), 
                                   n = 8L),
         last_8_points = lag(last_8_points, n = 1L)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "home_away", 
              values_from = c("team", "last_8_points")) %>% 
  rename(home_team = team_home_team,
         away_team = team_away_team)

# Betting historic

betting_raw <- tibble()
for(yr in 2000:2020){

betting_url <- paste0("https://api.collegefootballdata.com/lines?year=", as.character(yr), "&seasonType=both")
betting <- cfbd_api(betting_url, key = my_key)
betting <- betting %>% unnest(cols = lines)
betting_raw <- rbind(betting_raw, betting)
message("Done year ", yr)

}

betting_consensus <- betting_raw %>% 
  filter(provider == "consensus")

games_lines_joined <- games_record_home_away %>% 
  inner_join(betting_consensus, by = 
               c("id", 
                 "home_team" = "homeTeam", 
                 "away_team" = "awayTeam",
                 "home_points" = "homeScore",
                 "away_points" = "awayScore")) %>% 
  select(home_team, away_team, home_points, away_points, spread, formattedSpread, overUnder, everything())

joined_2 <- games_lines_joined %>% 
  mutate(favorite_team = str_replace_all(formattedSpread, "[0-9]", ""),
         favorite_team = str_replace_all(favorite_team, "[//.//-]", ""),
         favorite_team = str_trim(favorite_team, side = "both"))

joined_3 <- joined_2 %>% 
  mutate(spread = as.double(spread),
         overUnder = as.double(overUnder),
         actual_spread = away_points - home_points,
         home_team_covered = if_else(actual_spread < spread, T, F),
         favorite_covered = case_when(actual_spread < spread & home_team == favorite_team ~ TRUE,
                                      actual_spread > spread & away_team == favorite_team ~ TRUE,
                                      TRUE ~ FALSE),
         actual_total_points = home_points + away_points,
         over_hit = if_else(actual_total_points > overUnder, TRUE, FALSE)
         )

joined_4 <- joined_3 %>% 
  select(1:7, favorite_team, actual_spread, home_team_covered, favorite_covered,
         actual_total_points, over_hit, everything())


# Exploration Plots -------------------------------------------------------

# overUnder vs. actual total points
joined_4 %>% 
  ggplot(aes(x = overUnder, y = actual_total_points)) +
  geom_point(alpha = 0.3, color = staturdays_colors("lightest_blue")) +
  geom_abline()

# R-squared
summary(lm(actual_total_points ~ overUnder, data = joined_4))

# Spread vs. actual spread
joined_4 %>% 
  ggplot(aes(x = spread, y = actual_spread)) +
  geom_point(alpha = 0.3, color = staturdays_colors("lightest_blue")) +
  geom_abline()

# R-squared
summary(lm(actual_spread ~ spread, data = joined_4))

# Distribution of total points
joined_4 %>% 
  filter(is.na(overUnder) == F) %>% 
  pivot_longer(cols = c(overUnder, actual_total_points), names_to = "stat", values_to = "value") %>% 
  ggplot(aes(x = value, fill = stat)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 2)

# Distribution of spreads
joined_4 %>% 
  filter(is.na(spread) == F) %>% 
  pivot_longer(cols = c(spread, actual_spread), names_to = "stat", values_to = "value") %>% 
  ggplot(aes(x = value, fill = stat)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 2)

# Boxplot of variation of actual point totals by overunder line
joined_4 %>% 
  filter(is.na(overUnder) == F) %>% 
  ggplot(aes(x = cut(overUnder, 10), y = actual_total_points)) +
  geom_boxplot()

# Boxplot of variation of actual spreads by spread
joined_4 %>% 
  filter(is.na(spread) == F) %>% 
  ggplot(aes(x = cut(spread, 10), y = actual_spread)) +
  geom_boxplot()

# Rate of favorites covering
joined_4 %>% 
  filter(is.na(spread) == F) %>% 
  ggplot() +
  geom_bar(aes(x = favorite_covered))

# Rate of overs hitting
joined_4 %>% 
  filter(is.na(overUnder) == F) %>% 
  ggplot() +
  geom_bar(aes(x = over_hit))

# Over Hit Rate by Over Bucket

joined_4 %>% 
  mutate(over_bucket = floor(overUnder/10)) %>% 
  group_by(over_bucket) %>% 
  summarise(over_hit_rate = sum(over_hit) / n(),
            n_games = n()) %>% 
  filter(n_games > 30) %>% 
  ggplot(aes(x = over_bucket, y = over_hit_rate)) +
  geom_col(color = "blue", fill = "transparent") +
  geom_text(aes(label = round(over_hit_rate, digits = 2)), nudge_y = -.2)

# Favorite cover rate by spread bucket
joined_4 %>% 
  mutate(spread_bucket = cut(spread, breaks = 10)) %>% 
  group_by(spread_bucket) %>% 
  summarise(favorite_cover_rate = sum(favorite_covered) / n(),
            n_games = n()) %>% 
  filter(n_games > 30) %>% 
  ggplot(aes(x = spread_bucket, y = favorite_cover_rate)) +
  geom_col(color = "blue", fill = "transparent") +
  geom_text(aes(label = round(favorite_cover_rate, digits = 2)), nudge_y = -.2)
# In 192 games, teams favored by 40-50 points cover only 42% of the time
# Implied odds of -110 are 47.6 so there's some value in taking the underdog
# which covers 58.3% of the time

# Expected value of a bet on a -40 - -50 underdog
100*.583 - (110)*.417 # $12.43

# Bring in pre-game team data ------------------------------------------------------

# Previous season record

# Last X games average score for over-under
summary(lm(actual_total_points ~ last_8_home_points + last_8_away_points, data = joined_4))

# Previous result against this team
# Elo Rating and Elo Win Probability
elo_historic <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv")
elo_join <- elo_historic %>% 
  group_by(team) %>% 
  mutate(last_week_elo = lag(elo_rating, n = 1L, order_by = date))

joined_elo <- joined_4 %>% 
  left_join(elo_join, 
            by = c("season.x" = "season", 
                   "start_date" = "date", 
                   "home_team" = "team")) %>% 
  rename(home_elo_rating = last_week_elo) %>% 
  left_join(elo_join, 
            by = c("season.x" = "season", 
                   "start_date" = "date", 
                   "away_team" = "team")) %>% 
  rename(away_elo_rating = last_week_elo) %>% 
  mutate(favorite_elo = if_else(home_team == favorite_team, home_elo_rating, away_elo_rating),
         underdog_elo = if_else(home_team == favorite_team, away_elo_rating, home_elo_rating))

# Actual cover probability given Elo ratings
summary(glm(favorite_covered ~ favorite_elo + underdog_elo + spread, data = joined_elo))

# Actual over hit probability given Elo ratings
summary(glm(over_hit ~ favorite_elo + underdog_elo + overUnder, data = joined_elo))

# Advanced metrics for previous games including success rate

View(joined_4)
