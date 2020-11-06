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

staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("lightest_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          axis.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.title = element_text(color = staturdays_colors("lightest_blue"), size = 15),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 15))
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
for (j in 2000:2019) {
  for (i in 1:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- tibble(games)
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

# Mutate week of elo_ratings for joining purposes
elo_ratings_tmp <- elo_ratings %>% 
  mutate(week = week + 1)

games.final <- tibble()
for(szn in 2000:2019){
  for(wk in 1:19){
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

# Get the result of the game after the current one for each team. Later we will group_by games that have a win and summarise the avg. next result
# May need to bring in schedule data and use that instead of the games database

# Get schedule for each school
games_home <- games.final %>% 
  group_by(home_team) %>% 
  select(id, season, week, game_date, home_team, away_team, game_outcome_home, home_elo, away_elo) %>% 
  rename(opponent = away_team, team = home_team) %>% 
  mutate(home_away = "home") %>% 
  rename(game_outcome = game_outcome_home)

games_away <- games.final %>% 
  group_by(away_team) %>% 
  select(id, season, week, game_date, home_team, away_team, game_outcome_home, home_elo, away_elo) %>% 
  rename(opponent = home_team, team = away_team) %>% 
  mutate(home_away = "away") %>% 
  mutate(game_outcome = 1-game_outcome_home) %>% 
  select(-game_outcome_home)

schedule_combined <- rbind(games_home, games_away) %>% 
  mutate(elo_diff = case_when(home_away == "home" ~ home_elo - away_elo,
                              home_away == "away" ~ away_elo - home_elo))

# get next game result
schedule_outcomes <- schedule_combined %>% 
  group_by(team, season) %>% 
  mutate(next_game_result = lead(game_outcome, order_by = game_date),
         next_game_elo_diff = lead(elo_diff, order_by = game_date)) %>% 
  mutate(elo_diff_bucket = elo_diff%/%100*100,
         next_game_elo_diff_bucket = next_game_elo_diff %/%100*100)

# Add opponent elo and next opponent elo
schedule_outcomes <- schedule_outcomes %>% 
  group_by(team, season) %>% 
  mutate(opponent_elo = if_else(home_away == "home", away_elo, home_elo)) %>% 
  mutate(next_opponent_elo = if_else(lead(home_away, order_by = game_date) == "home", lead(away_elo, order_by = game_date), lead(home_elo, order_by = game_date))) %>% 
  mutate(opponent_elo_bucket = opponent_elo%/%100*100,
         next_game_opponent_elo_bucket = next_opponent_elo%/%100*100)


# Plots -------------------------------------------------------------------

# Controlled for opponent previous week vs. next week
p <- schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  mutate(outcome_diff = next_game_result - game_outcome) %>% 
  summarise(avg_game_outcome = mean(game_outcome, na.rm = T), 
            avg_next_game_outcome = mean(next_game_result, na.rm = T),
            avg_outcome_diff = avg_next_game_outcome - avg_game_outcome, #mean(outcome_diff, na.rm = T)
            count = n()) %>% 
  filter(elo_diff_bucket == next_game_elo_diff_bucket, count > 50)

p %>% 
  ggplot(aes(x = elo_diff_bucket, y = avg_outcome_diff)) +
  geom_col(aes(fill = if_else(avg_outcome_diff > 0, staturdays_colors("dark_blue"), staturdays_colors("lightest_blue")))) +
  ggrepel::geom_label_repel(aes(label = count), force = 0,
                            fill = staturdays_colors("orange"), color = "white",
                            alpha = 0.8) +
  labs(title = "No evidence of a hangover",
       subtitle = "In back-to-back tough games, underdogs' results \nimprove in the second game\nControlled for by strength of opponent",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = "Elo Advantage/Disadvantage in Both Games",
       y = "Difference in Result vs. Previous Game") +
  staturdays_theme +
  scale_fill_identity() +
  scale_y_continuous(labels = percent)

ggsave(filename = paste0("opponent_controlled_results_", str_replace_all(now(), ":", "."), ".png"), 
       plot = last_plot(),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 200, height = 200, units = "mm")

# Upset win followed by a cupcake game win rate compared to regular win (0 diff) followed by a cupcake
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F, game_outcome == 1) %>% 
  group_by(elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  mutate(outcome_diff = next_game_result - game_outcome) %>% 
  mutate(big_win_normal_win = case_when(elo_diff_bucket < -300  ~ "big_to_cupcake", #& next_game_elo_diff_bucket > 300
                                        elo_diff_bucket == 0  ~ "normal_to_cupcake", #& next_game_elo_diff_bucket > 300
                                        TRUE ~ "other")) %>% 
  filter(big_win_normal_win != "other") %>% 
  group_by(big_win_normal_win) %>% 
  summarise(avg_game_outcome = mean(game_outcome, na.rm = T), 
            avg_next_game_outcome = mean(next_game_result, na.rm = T),
            avg_outcome_diff = avg_next_game_outcome - avg_game_outcome,
            count = n()) %>% 
  ggplot(aes(x = big_win_normal_win, y = avg_outcome_diff)) +
  geom_col(alpha = 0.7, fill = staturdays_colors("orange")) +
  labs(title = "Upset Wins Then Cupcakes",
       subtitle = "Teams actually play slightly better against weak \nopponents after a tough win, compared to a normal win",
       x = "With the 2nd game being a cupcake, the first matchup was a...",
       y = "Avg. 2nd Game Win Rate Difference",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  scale_x_discrete(labels = c("Upset Win", "Normal Win")) +
  staturdays_theme +
  scale_y_continuous(labels = percent)

ggsave(filename = paste0("upset_win_vs_normal_win_", str_replace_all(now(), ":", "."), ".png"), 
       plot = last_plot(),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 200, height = 200, units = "mm")

# This is the same as the next plot below - Avg. Next Game Result in a loss vs. in a win, control for opponent and diff. in elos
schedule_outcomes %>%  
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  filter(elo_diff_bucket == next_game_elo_diff_bucket) %>% 
  group_by(elo_diff_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count > 30, dplyr::between(elo_diff_bucket, -400, 300)) %>% 
  ggplot(aes(x = elo_diff_bucket, y = avg_next_outcome, fill = as.factor(game_outcome))) +
  geom_col(position = "dodge2") +
  ggrepel::geom_text_repel(aes(label = count)) +
  labs(title = "Some letdown effect in back-to-back favored games",
       subtitle = "Underdogs tend to win again off an upset,\nwhile favorites squander the 2nd game")

# Alternate viz - geom_smooth controlled for difference in Elo. Wins vs. losses 2nd game win rate
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  filter(elo_diff_bucket == next_game_elo_diff_bucket) %>% 
  group_by(elo_diff_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(total_count > 50) %>% 
  ggplot(aes(x = elo_diff_bucket, y = avg_next_outcome, color = as.factor(game_outcome))) +
  geom_point() +
  geom_smooth() +
  labs(title = "Result of first game on 2nd game",
       subtitle = "No real evidence of previous result impacting the next \ngame when playing two similar opponents back-to-back\nControlled for opponent",
       color = "First Game Outcome",
       x = "Elo Advantage/Disadvantage vs. Opponent in Both Games",
       y = "Average Next Game Win Rate") +
  staturdays_theme +
  scale_color_manual(values = c(staturdays_colors("orange"), "#394871"),
                     labels = c("Loss", "Win")) +
  scale_y_continuous(labels = percent)

ggsave(filename = paste0("geom_smooth_win_loss_controlled_", str_replace_all(now(), ":", "."), ".png"), 
       plot = last_plot(),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 200, height = 200, units = "mm")

# 
# # Alternate viz - TOUGH THEN EASY - Only games where first opp was a tough win, geom_smooth controlled for difference in Elo. Wins vs. losses 2nd game win rate
# schedule_outcomes %>% 
#   filter(is.na(next_game_result) == F) %>% 
#   filter(elo_diff_bucket < -300 & next_game_elo_diff_bucket > 300) %>% 
#   group_by(game_outcome) %>% 
#   summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
#   ggplot(aes(x = as.factor(game_outcome), y = avg_next_outcome, fill = as.factor(game_outcome))) +
#   geom_col(fill = c(staturdays_colors("orange"), staturdays_colors("lightest_blue"))) +
#   labs(title = "Tough game next result, not controlled for next opponent. No real evidence of prevoius result impact on next game",
#        subtitle = "If anything, teams coming off a win handle their next opponent easier",
#        caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
#        x = "First Game Result",
#        y = "Average 2nd Game Win Rate") +
#   staturdays_theme +
#   theme(legend.position = "none") +
#   scale_x_discrete(labels = c("Underdog Loss", "Underdog Win"))
# 
# # Alternate viz - TWO TOUGH GAMES - Only games where first opp was a tough win, geom_smooth controlled for difference in Elo. Wins vs. losses 2nd game win rate
# schedule_outcomes %>% 
#   filter(is.na(next_game_result) == F) %>% 
#   filter(elo_diff_bucket < -300 & next_game_elo_diff_bucket < -300) %>% 
#   group_by(game_outcome) %>% 
#   summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
#   ggplot(aes(x = as.factor(game_outcome), y = avg_next_outcome, fill = as.factor(game_outcome))) +
#   geom_col(fill = c(staturdays_colors("orange"), staturdays_colors("lightest_blue"))) +
#   labs(title = "Two tough games game next result, not controlled for next opponent. No real evidence of prevoius result impact on next game",
#        subtitle = "If anything, teams coming off a win handle their next opponent easier",
#        caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
#        x = "First Game Result",
#        y = "Average 2nd Game Win Rate") +
#   staturdays_theme +
#   theme(legend.position = "none") +
#   scale_x_discrete(labels = c("Underdog Loss", "Underdog Win"))
# 
# # Alternate viz - TWO EASY GAMES - Only games where first opp was a tough win, geom_smooth controlled for difference in Elo. Wins vs. losses 2nd game win rate
# schedule_outcomes %>% 
#   filter(is.na(next_game_result) == F) %>% 
#   filter(elo_diff_bucket > 300 & next_game_elo_diff_bucket > 300) %>% 
#   group_by(game_outcome) %>% 
#   summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
#   ggplot(aes(x = as.factor(game_outcome), y = avg_next_outcome, fill = as.factor(game_outcome))) +
#   geom_col(fill = c(staturdays_colors("orange"), staturdays_colors("lightest_blue"))) +
#   labs(title = "Two easy games game next result, not controlled for next opponent. No real evidence of prevoius result impact on next game",
#        subtitle = "If anything, teams coming off a win handle their next opponent easier",
#        caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
#        x = "First Game Result",
#        y = "Average 2nd Game Win Rate") +
#   staturdays_theme +
#   theme(legend.position = "none") +
#   scale_x_discrete(labels = c("Favorite Loss", "Favorite Win"))
# 
# # Alternate viz -  EASY THEN TOUGH - Only games where first opp was a tough win, geom_smooth controlled for difference in Elo. Wins vs. losses 2nd game win rate
# schedule_outcomes %>% 
#   filter(is.na(next_game_result) == F) %>% 
#   filter(elo_diff_bucket > 300 & next_game_elo_diff_bucket < -300) %>% 
#   group_by(game_outcome) %>% 
#   summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
#   ggplot(aes(x = as.factor(game_outcome), y = avg_next_outcome, fill = as.factor(game_outcome))) +
#   geom_col(fill = c(staturdays_colors("orange"), staturdays_colors("lightest_blue"))) +
#   labs(title = " Easy then tough games game next result, not controlled for next opponent. No real evidence of prevoius result impact on next game",
#        subtitle = "If anything, teams coming off a win handle their next opponent easier",
#        caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
#        x = "First Game Result",
#        y = "Average 2nd Game Win Rate") +
#   staturdays_theme +
#   theme(legend.position = "none") +
#   scale_x_discrete(labels = c("Favorite Loss", "Favorite Win"))

# Facet toughness
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  mutate(toughness = case_when(elo_diff_bucket > 300 & next_game_elo_diff_bucket < -300 ~ "Easy-Tough",
                               elo_diff_bucket > 300 & next_game_elo_diff_bucket > 300 ~ "2 Cupcakes",
                               elo_diff_bucket < -300 & next_game_elo_diff_bucket < -300 ~ "2 Tough Games",
                               elo_diff_bucket < -300 & next_game_elo_diff_bucket > 300 ~ "Tough-Easy",
                               elo_diff_bucket == 0 & next_game_elo_diff_bucket == 0 ~ "Even Matchups")) %>% 
  group_by(toughness, game_outcome) %>% 
  summarise(avg_outcome = mean(game_outcome), avg_next_outcome = mean(next_game_result), count = n()) %>% 
  filter(is.na(toughness) == F) %>% 
  ggplot(aes(x = as.factor(game_outcome), y = avg_next_outcome, fill = as.factor(game_outcome))) +
  geom_col(fill = rep(c(staturdays_colors("orange"), staturdays_colors("lightest_blue")), 5)) +
  facet_wrap(vars(toughness)) +
  labs(title = "Winning bodes well for your \nnext game",
       subtitle = "Shocker.",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = "First Game Result",
       y = "Average 2nd Game Win Rate") +
  staturdays_theme +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Loss", "Win")) +
  ggrepel::geom_label_repel(aes(label = paste0(round(avg_next_outcome, 2), " \nin ", count, " games")), 
                                fill = staturdays_colors("dark_blue"), 
                                color = "white", force = 1, direction = "y", alpha = 0.8) +
  scale_y_continuous(labels = percent)

ggsave(filename = paste0("toughness_facets_win_vs_loss_", str_replace_all(now(), ":", "."), ".png"), 
       plot = last_plot(),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 200, height = 200, units = "mm")

# Toughness - WINS only
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F, game_outcome == 1) %>% 
  mutate(toughness = case_when(elo_diff_bucket > 300 & next_game_elo_diff_bucket < -300 ~ "Easy-Tough",
                               elo_diff_bucket > 300 & next_game_elo_diff_bucket > 300 ~ "2 Cupcakes",
                               elo_diff_bucket < -300 & next_game_elo_diff_bucket < -300 ~ "2 Tough Games",
                               elo_diff_bucket < -300 & next_game_elo_diff_bucket > 300 ~ "Tough-Easy",
                               elo_diff_bucket == 0 & next_game_elo_diff_bucket == 0 ~ "Even Matchups")) %>% 
  group_by(toughness) %>% 
  summarise(avg_outcome = mean(game_outcome), avg_next_outcome = mean(next_game_result), count = n()) %>% 
  filter(is.na(toughness) == F) %>% 
  arrange(desc(avg_next_outcome)) %>% 
  ggplot(aes(x = reorder(toughness, desc(avg_next_outcome)), y = avg_next_outcome, fill = count)) +
  geom_col() +
  labs(title = "Next result after a win",
       subtitle = "The level of first-win opponent doesn't appear to have \nan effect on your 2nd game outcome",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
       x = "First Game Result",
       y = "Average 2nd Game Win Rate") +
  staturdays_theme +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15)) +
  ggrepel::geom_label_repel(aes(label = paste0(round(avg_next_outcome, 2), " \nin ", count, " games")), 
                            fill = staturdays_colors("orange"), 
                            color = "white", force = 1, direction = "y", alpha = 0.8) +
  scale_fill_continuous(low = staturdays_colors("lightest_blue"), high = staturdays_colors("dark_blue")) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = staturdays_colors("orange")) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = staturdays_colors("orange")) +
  scale_y_continuous(labels = percent)

ggsave(filename = paste0("toughness_wins_only_", str_replace_all(now(), ":", "."), ".png"), 
       plot = last_plot(),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       dpi = 300, width = 200, height = 200, units = "mm")

# Other plots -------------------------------------------------------------

# Avg. Next Game Result in a loss vs. in a win, NOT controlled for opponent and diff. in elos
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, elo_diff_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(elo_diff_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100, dplyr::between(elo_diff_bucket,-500, 400)) %>% 
  ggplot(aes(x = elo_diff_bucket, y = avg_next_outcome, fill = as.factor(game_outcome))) +
  geom_col(position = "dodge2") +
  ggrepel::geom_text_repel(aes(label = paste(count, game_outcome)), nudge_y = -.25, segment.alpha = 0) +
  labs(title = "Teams tend to win their next game after a win",
       subtitle = "No evidence that underdogs squander their next game, or win more")

# Wins against "TOUGH" opponent (high Elo), next week performance, controlled for opp. strength
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, opponent_elo_bucket, next_game_opponent_elo_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  filter(opponent_elo_bucket == next_game_opponent_elo_bucket) %>% 
  group_by(opponent_elo_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(total_count > 50) %>% 
  ggplot(aes(x = opponent_elo_bucket, y = avg_next_outcome, fill = as.factor(game_outcome))) +
  geom_col(position = "dodge2") +
  labs(title = "Teams actually play better coming off a big win",
       subtitle = "Even when they play another tough opponent back to back")

# Wins against "TOUGH" opponent (high Elo), next week performance, don't control for opponent strength
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, opponent_elo_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(opponent_elo_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(total_count > 800) %>% 
  ggplot(aes(x = opponent_elo_bucket, y = avg_next_outcome, fill = as.factor(game_outcome))) +
  geom_col(position = "dodge2") +
  labs(title = "Teams that win tend to win the next game more often than teams that lose... no duh")

# Wins against "TOUGH" opponent (high Elo), next week performance, control on next opponent strength
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F, opponent_elo_bucket >= 1800) %>% 
  group_by(game_outcome, next_game_opponent_elo_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_opponent_elo_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(total_count > 100) %>% 
  ggplot(aes(x = next_game_opponent_elo_bucket, y = avg_next_outcome, fill = if_else(game_outcome == 1, "Win", "Loss"))) +
  geom_col(position = "dodge2") +
  labs(title = "Teams that play a tough opponent, how they do in the next game",
       fill = "Previous Result")

# Scatter with bubbles
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, opponent_elo_bucket, next_game_opponent_elo_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_opponent_elo_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100, opponent_elo_bucket > 700) %>% 
  ggplot(aes(x = opponent_elo_bucket, y = next_game_opponent_elo_bucket, size = avg_next_outcome, color = if_else(game_outcome == 1, "Win", "Loss"))) +
  geom_jitter(alpha = 0.3, height = .25) +
  labs(title = "Teams that play a tough opponent, how they do in the next game",
       color = "Previous Result",
       size = "2nd Game Win %")

# Facet Wrap by First Game Result
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, opponent_elo_bucket, next_game_opponent_elo_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_opponent_elo_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100, opponent_elo_bucket > 700) %>% 
  ggplot(aes(x = opponent_elo_bucket, y = next_game_opponent_elo_bucket, size = avg_next_outcome, color = avg_next_outcome)) +
  facet_wrap(facets = vars(if_else(game_outcome == 1, "Win", "Loss"))) +
  geom_point(aes(alpha = avg_next_outcome)) +
  labs(title = "2nd Game Win Rate by \n1st Game Result",
       color = "2nd Game Win %",
       subtitle = "Bigger, darker bubbles represent higher win rates",
       x = "1st Game Opponent Elo",
       y = "2nd Game Opponent Elo") +
  staturdays_theme +
  scale_color_gradient(low = "grey", high = staturdays_colors("orange")) +
  scale_size(guide = "none") + # Hide size and alpha from legend since they all represent same thing
  scale_alpha(guide = "none")

# Facet Wrap by First Game Result - USING ELO DIFF
facet_elo_diff <- schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_elo_diff_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100) %>% 
  ggplot(aes(x = elo_diff_bucket, y = next_game_elo_diff_bucket, size = avg_next_outcome, color = avg_next_outcome)) +
  facet_wrap(facets = vars(if_else(game_outcome == 1, "Win", "Loss"))) +
  geom_point(aes(alpha = avg_next_outcome)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(title = "2nd Game Win Rate by \n1st Game Result",
       color = "2nd Game Win %",
       subtitle = "Bigger, darker bubbles represent higher win rates\nMin. 100 Games",
       x = "1st Game Elo Difference",
       y = "2nd Game Elo Difference",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  annotate(geom = "label", label = "Underdogs to favorites", x = -350, y = 500, size = 3) +
  annotate(geom = "label", label = "Underdogs twice", x = -350, y = -500, size = 3) +
  annotate(geom = "label", label = "Favorites to underdogs", x = 275, y = -500, size = 3) +
  annotate(geom = "label", label = "Favorites twice", x = 275, y = 500, size = 3) +
  staturdays_theme +
  scale_color_gradient(low = "grey", high = staturdays_colors("orange")) +
  scale_size(guide = "none") + # Hide size and alpha from legend since they all represent same thing
  scale_alpha(guide = "none")

# Table - Elo Rating
top_half <- schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, opponent_elo_bucket, next_game_opponent_elo_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_opponent_elo_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100, opponent_elo_bucket > 700) %>% 
  group_by(opponent_elo_bucket, next_game_opponent_elo_bucket) %>% 
  pivot_wider(names_from = game_outcome, values_from = avg_next_outcome) %>% 
  rename(previous_loss = `0`, previous_win = `1`) %>% 
  filter(is.na(previous_loss) == F) %>% 
  select(-previous_win)

bottom_half <- schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, opponent_elo_bucket, next_game_opponent_elo_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_opponent_elo_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100, opponent_elo_bucket > 700) %>% 
  group_by(opponent_elo_bucket, next_game_opponent_elo_bucket) %>% 
  pivot_wider(names_from = game_outcome, values_from = avg_next_outcome) %>% 
  rename(previous_loss = `0`, previous_win = `1`) %>% 
  filter(is.na(previous_win) == F) %>% 
  select(-previous_loss)

join_table <- left_join(top_half, bottom_half, by = c("opponent_elo_bucket", "next_game_opponent_elo_bucket")) %>% 
  mutate(win_loss_diff = previous_win - previous_loss)

# Table - Elo DIFF
top_half_diff <- schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_elo_diff_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100) %>% 
  group_by(elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  pivot_wider(names_from = game_outcome, values_from = avg_next_outcome) %>% 
  rename(previous_loss = `0`, previous_win = `1`) %>% 
  filter(is.na(previous_loss) == F) %>% 
  select(-previous_win)

bottom_half_diff <- schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_elo_diff_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100) %>% 
  group_by(elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  pivot_wider(names_from = game_outcome, values_from = avg_next_outcome) %>% 
  rename(previous_loss = `0`, previous_win = `1`) %>% 
  filter(is.na(previous_win) == F) %>% 
  select(-previous_loss)

join_table_diff <- left_join(top_half_diff, bottom_half_diff, by = c("elo_diff_bucket", "next_game_elo_diff_bucket")) %>% 
  mutate(win_loss_diff = previous_win - previous_loss)

# Join again to use in graph
pt_1 <- schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(game_outcome, elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  summarise(avg_next_outcome = mean(next_game_result), count = n()) %>% 
  group_by(next_game_elo_diff_bucket) %>% 
  mutate(total_count = sum(count)) %>% 
  filter(count >= 100)

pt_2 <- left_join(pt_1, join_table_diff, by = c("elo_diff_bucket", "next_game_elo_diff_bucket"))

# Viz - NOT SURE THIS ONE is ACCURATE.
pt_2 %>%
  ggplot(aes(x = elo_diff_bucket, y = next_game_elo_diff_bucket, size = avg_next_outcome, color = avg_next_outcome)) +
  facet_wrap(facets = vars(if_else(game_outcome == 1, "Win", "Loss"))) +
  geom_point(aes(alpha = avg_next_outcome), shape = if_else(pt_2$win_loss_diff < 0, 17, 16)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(title = "2nd Game Win Rate by \n1st Game Result",
       color = "2nd Game Win %",
       subtitle = "Bigger, darker bubbles represent higher win rates\nTriangles are games where losers outplay winner\nMin. 100 Games",
       x = "1st Game Elo Difference",
       y = "2nd Game Elo Difference",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  annotate(geom = "label", label = "Underdogs to favorites", x = -350, y = 500, size = 3) +
  annotate(geom = "label", label = "Underdogs twice", x = -350, y = -500, size = 3) +
  annotate(geom = "label", label = "Favorites to underdogs", x = 275, y = -500, size = 3) +
  annotate(geom = "label", label = "Favorites twice", x = 275, y = 500, size = 3) +
  staturdays_theme +
  scale_color_gradient(low = "grey", high = staturdays_colors("orange")) +
  scale_size(guide = "none") + # Hide size and alpha from legend since they all represent same thing
  scale_alpha(guide = "none")

# Underdogs play better in next games when they win their previous matchup across the board, but more so when they're slight underdogs
pt_2 %>% 
  select(-game_outcome) %>% 
  filter(is.na(win_loss_diff) == F) %>% 
  group_by(elo_diff_bucket) %>% 
  ggplot(aes(x = elo_diff_bucket)) +
  geom_col(aes(y = mean(win_loss_diff)), fill = "red", alpha = 0.5, position = "stack")

# Win rate
schedule_outcomes %>% 
  filter(is.na(next_game_result) == F) %>% 
  group_by(elo_diff_bucket, next_game_elo_diff_bucket) %>% 
  summarise(avg_game_outcome = mean(game_outcome, na.rm = T), 
            avg_next_game_outcome = mean(next_game_result, na.rm = T),
            avg_outcome_diff = avg_next_game_outcome - avg_game_outcome,
            count = n()) %>% 
  filter(elo_diff_bucket == next_game_elo_diff_bucket, count > 50) %>% 
  ggplot(aes(x = elo_diff_bucket, y = avg_outcome_diff)) +
  geom_point() +
  labs(title = "No evidence of a letdown effect",
       subtitle = "Underdogs actually play better on average in \ntheir next game after an upset win.\nControlled for by strength of opponent",
       caption = "@staturdays | @kylebeni012 - Data: @cfb_data") +
  staturdays_theme
