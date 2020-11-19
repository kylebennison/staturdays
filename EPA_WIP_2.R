# Get CFB Data API for Play by Play - Use to generate expected points added (EPA)
rm(list=ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(gt)
library(lubridate)
library(ggimage)

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

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

scrimmage_plays_all <- 
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Own)", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown"
  )

scrimmage_plays_non_turnover <-
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Fumble Recovery (Own)"
  )

scrimmage_plays_turnover <-
  c(
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown",
    "Safety"
  )

scrimmage_plays_pass <-
  c(
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Sack",
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown"
  )

scrimmage_plays_rush <-
  c(
    "Rush", 
    "Rushing Touchdown"
  )

no_action_plays <- 
  c(
    "Timeout",
    "End Period",
    "End of Half",
    "End of Game",
    "Kickoff"
  )

scrimmage_plays_kicks <- 
  c(
    "Punt",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Field Goal Good",
    "Blocked Field Goal",
    "Missed Field Goal Return"
  )

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

plays.master = data.frame()
for (j in 2014:2019) {
  for (i in 1:15) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- fromJSON(getURL(full_url_plays_encoded)) # Pull in API using url
    if(is_empty(plays) == F){
      clockcolumns <- plays %>% unnest_legacy(clock) # Takes clock data out as it's own columns
      plays <- plays %>%
        select(-clock) %>%
        as_tibble() %>%
        mutate(minutes = clockcolumns$minutes, seconds = clockcolumns$seconds) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
        mutate_at(c("minutes", "seconds"), ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
      plays <- tibble(plays)
      plays$week = i
      plays$year = j
      plays.master = rbind(plays.master, plays, make.row.names=TRUE)
    }
  }
}
rm(clockcolumns, plays)

games.master = data.frame()
for (j in 2014:2019) {
  for (i in 1:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

drives.master = data.frame()
for (j in 2014:2019) {
  cat('Loading Drives', j, '\n')
  full_url_drives <- paste0(base_url_drives, "seasonType=both&", "year=", as.character(j))
  full_url_drives_encoded <- URLencode(full_url_drives)
  drives <- fromJSON(getURL(full_url_drives_encoded))
  start_time_columns <- as_tibble(drives$start_time) # Takes clock data out as it's own columns
  end_time_columns <- as_tibble(drives$end_time)
  elapsed_time_columns <- as_tibble(drives$elapsed)
  drives <- drives %>% 
    select(-start_time, -end_time, -elapsed) %>% 
    as_tibble() %>% 
    mutate(start_minutes = start_time_columns$minutes, start_seconds = start_time_columns$seconds, 
           end_minutes = end_time_columns$minutes, end_seconds = end_time_columns$seconds,
           elapsed_minutes = elapsed_time_columns$minutes, elapsed_seconds = elapsed_time_columns$seconds,) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
    mutate_at(
      c("start_minutes", "start_seconds", "end_minutes", "end_seconds", "elapsed_minutes", "elapsed_seconds"),
      ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
  drives <- as_tibble(drives)
  drives.master = rbind(drives.master, drives)
}

# # Join Plays with Games to get additional info
# games.temp <- games.master
# games.temp <- mutate(games.temp, id = as.character(id))
# plays.temp <- plays.master
# plays.temp <- mutate(plays.temp, playid = id, id = substr(playid, 1, 9))
# plays_games_joined.master <- left_join(plays.master, games.temp, by = 'id') # Get regular or post season from games data
# games.temp <- data.frame()

# # Join Plays with Drives to Get Start Yardline
# drives.temp <- drives.master
# drives.temp <- mutate(drives.temp, id = as.character(id))
# plays.temp <- plays.master
# plays.temp <- mutate(plays.temp, playid = id, driveid = substr(playid, 1, 10))
# plays_drives.master <- left_join(plays.temp, drives.temp, by = c("drive_id" = "id"))
# drives.temp <- data.frame()
# plays.temp <- data.frame()

# Adjust yards_gained and start_yardline stats
# plays_drives.master <- plays_drives.master %>% 
#   mutate(start_yardline = if_else(offense.x != home, 100 - start_yardline, as.double(start_yardline)), end_yardline = if_else(offense.x != home, 100 - end_yardline, as.double(end_yardline)))

## Mutate Plays with First Downs (Smart), Pass/Rush, Success
plays.master2 <- plays.master %>% 
  mutate(play_specifics = play_type) %>% 
  mutate(first_down = 
           case_when(
             play_type %in% scrimmage_plays_all ~
               ifelse(yards_gained >= distance 
                      & lead(down, n=1, order_by = id) == 1 
                      & lead(offense, n = 1, order_by = id) == offense 
                      & lead(drive_id, n = 1, order_by = id) == drive_id 
                      & (!play_specifics %in% scrimmage_plays_turnover), TRUE, 
                      ifelse(play_specifics %in% c("Passing Touchdown", "Rushing Touchdown") 
                             | ((str_detect(play_text, "TOUCHDOWN") 
                                 | str_detect(play_text, "1ST down")) 
                                & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
             TRUE ~ NA
           )
  )
# Calculate loss of yards on turnovers
plays.master_temp <- plays.master %>% 
  mutate(turnover_yards = case_when(play_type %in% scrimmage_plays_turnover ~ 70L - lead(yards_to_goal, order_by = id), #subtract starting field position of next drive from avg. starting field position to find how many yards that turnover cost you
                                    TRUE ~ 0L))

plays.master_temp <- plays.master_temp %>% mutate(yards_gained = case_when(play_type %in% scrimmage_plays_turnover ~ -turnover_yards,
                                                                           TRUE ~ yards_gained))

plays.master <- plays.master_temp
rm(plays.master_temp)

# Calculate avg. starting field pos on turnovers vs. normal
drives.master %>% summarise(mean(start_yards_to_goal)) # 70 avg.

plays.master %>% mutate(lead_start_yards_to_goal = 
                          case_when(play_type %in% scrimmage_plays_turnover ~ lead(yards_to_goal, order_by = id),
                                    TRUE ~ 0L)) %>% 
  filter(play_type %in% scrimmage_plays_turnover) %>% 
  group_by(play_type) %>% 
  filter(!str_detect(play_type,"Touchdown")) %>% 
  summarise(avg_yds_to_goal = mean(lead_start_yards_to_goal), count = n()) # 54.5 avg, so a turnover is worth -15.5 yds. vs. avg.

# Classify the multiple play types into a simpler just rush or pass
plays.master2$pass_rush[plays.master2$play_type %in% scrimmage_plays_pass] <- "Pass"
plays.master2$pass_rush[plays.master2$play_type %in% scrimmage_plays_rush] <- "Rush"
# Rush Fumble Rows
rush_rows <- plays.master2 %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", 
                               "Fumble Recovery (Opponent)", 
                               "Fumble Return Touchdown", 
                               "Safety"), 
         str_detect(play_text, "run") 
         & !str_detect(play_text, "kick") 
         & !str_detect(play_text, "punt")) %>% 
  mutate(pass_rush = "Rush")
# Pass Fumble Rows
pass_rows <- plays.master2 %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "pass") | str_detect(play_text, "sack")) %>% 
  mutate(pass_rush = "Pass")
# Change fumbles to a pass or rush
plays.master2[which(plays.master2$id %in% rush_rows$id), "pass_rush"] <- "Rush"
plays.master2[which(plays.master2$id %in% pass_rows$id), "pass_rush"] <- "Pass"

#Add Success Column
plays.master2 <- plays.master2 %>% 
  mutate(success = 
           case_when(
             play_type %in% scrimmage_plays_turnover ~ 0,
             down == 1 & yards_gained >= 0.5 * distance ~ 1,
             down == 2 & yards_gained >= 0.7 * distance ~ 1,
             down == 3 & yards_gained >= 1 * distance ~ 1,
             down == 4 & yards_gained >= 1 * distance ~ 1,
             str_detect(play_text, "1ST down") == TRUE ~ 1,
             str_detect(play_text, "TD") == TRUE ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master2
rm(plays.master2)
##

## Add Line Yards Stat

plays.master.temp <- plays.master %>% 
  mutate(lineYards = 
           case_when(
             pass_rush == "Rush" & yards_gained < 0 ~ yards_gained * 1.2,
             pass_rush == "Rush" & yards_gained > 0 & yards_gained <= 4 ~ yards_gained * 1,
             pass_rush == "Rush" & yards_gained > 4 & yards_gained <= 10 ~ 4 + yards_gained * 0.5,
             pass_rush == "Rush" & yards_gained > 10 ~ 7,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

## Garbage Time Indicator WIP

plays.master.temp <- plays.master %>% 
  mutate(garbage_time = 
           case_when(
             period == 2 & abs(offense_score - defense_score) > 38 ~ 1,
             period == 3 & abs(offense_score - defense_score) > 28 ~ 1,
             period == 4 & abs(offense_score - defense_score) > 22 ~ 1,
             period == 4 & minutes < 2 & abs(offense_score - defense_score) > 16 ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

# Plays by percentile to help guide defining explosive
percentile_explosiveness <- plays.master %>% group_by(pass_rush) %>% 
  summarise(avg_yds = mean(yards_gained), 
            stand_dev = sd(yards_gained), 
            percentile = quantile(yards_gained, .90))

explosive_pass <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Pass") %>% pull(percentile))
explosive_rush <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Rush") %>% pull(percentile))

# Passing vs. Standard Downs, Explosive Plays
plays.master.temp <- plays.master %>% 
  mutate(passing_down = case_when(down == 2 & distance >= 7 ~ 1,
                                  down == 3 & distance >= 5 ~ 1,
                                  down == 4 & distance >= 5 ~ 1,
                                  TRUE ~ 0),
         explosive = case_when(play_specifics %in% scrimmage_plays_turnover ~ 0,
                               yards_gained >= explosive_pass 
                               & play_specifics %in% scrimmage_plays_non_turnover 
                               & pass_rush == "Pass" ~ 1,
                               yards_gained >= explosive_rush
                               & play_specifics %in% scrimmage_plays_non_turnover 
                               & pass_rush == "Rush" ~ 1,
                               TRUE ~ 0))

plays.master <- plays.master.temp
rm(plays.master.temp)

# Create a master clock completely in seconds
plays.master.temp <- plays.master %>% 
  mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds)

plays.master <- plays.master.temp
rm(plays.master.temp)


# EPA Model ---------------------------------------------------------------

# Join Drives to Plays
plays_drives.temp <- plays.master %>% left_join(drives.master, by = c("drive_id" = "id"), suffix = c(".plays", ".drives"))

# Add home and away scores
plays_drives.temp2 <- plays_drives.temp %>% mutate(home_score = case_when(home == offense.plays ~ offense_score, # Get home lead/deficit
                                                                        TRUE ~ defense_score),
                                                 away_score = case_when(away == offense.plays ~ offense_score,
                                                                        TRUE ~ defense_score),
                                                 home_score_lead_deficit = home_score - away_score)

# Add possession flag
plays_drives.temp3 <- plays_drives.temp2 %>% 
  mutate(home_poss_flag = if_else(home == offense.plays, 1, 0),
         home_timeouts = if_else(home == offense.plays, offense_timeouts, defense_timeouts),
         away_timeouts = if_else(away == offense.plays, offense_timeouts, defense_timeouts))

# Get change in points for each drive # THIS ISN'T WORKING BECAUSE OFF / DEF SWITCH ON KICKOFFS, "Return Touchdown"
plays_drives.temp4 <- plays_drives.temp3 %>% 
  filter(str_detect(play_type,"Kickoff") == F, play_type != "Penalty", !(play_type %in% no_action_plays)) %>% 
  group_by(drive_id) %>% 
  mutate(offense_score_diff = max(offense_score) - min(offense_score),
         defense_score_diff = max(defense_score) - min(defense_score),
         lead_off_score_diff = lead(offense_score_diff, n = 1L, order_by = drive_id),
         lead_def_score_diff = lead(defense_score_diff, n = 1L, order_by = drive_id),
         drive_points = case_when(offense_score_diff > 0 ~ offense_score_diff, # Offense scores
                                  offense_score_diff == 0 & defense_score_diff > 0 ~ -defense_score_diff, # Defense scores
                                  offense_score_diff == 0 & defense_score_diff == 0 & 
                                    lead_off_score_diff > 0 & 
                                    offense.plays != lead(offense.plays, n = 1L, order_by = drive_id) ~ -lead_off_score_diff, # Offense scores on next drive, and is the other team (no onside kick etc.)
                                  offense_score_diff == 0 & defense_score_diff == 0 & 
                                    lead_off_score_diff > 0 & 
                                    offense.plays == lead(offense.plays, n = 1L, order_by = drive_id) ~ lead_off_score_diff, # Offense scores on next drive and is the same team as previous drive
                                  offense_score_diff == 0 & defense_score_diff == 0 & 
                                    lead_off_score_diff == 0 & lead_def_score_diff > 0 &
                                    offense.plays == lead(defense.plays, n = 1L, order_by = drive_id) ~ lead_def_score_diff, # Defense scores on next drive, is the team on offense from the previous drive
                                  offense_score_diff == 0 & defense_score_diff == 0 & 
                                    lead_off_score_diff == 0 & lead_def_score_diff > 0 &
                                    offense.plays != lead(defense.plays, n = 1L, order_by = drive_id) ~ -lead_def_score_diff, # Defense scores on next drive, is the same defense as the previous drive
                                  TRUE ~ 0L))

# Hist of drive points. This is probably not perfect due to the cases below, but should be fairly good.
plays_drives.temp4 %>% 
  ungroup() %>% 
  ggplot(aes(x = drive_points)) +
  geom_histogram() +
  scale_x_continuous(limits = c(-9, 9), breaks = seq(-9,9, by = 1))

# See where there are issues and inaccuracies
plays_drives.temp4 %>% filter(drive_points > 8 | drive_points < -8) %>% 
  select(id, drive_id, offense.plays, defense.plays, play_type, play_text, drive_points, offense_score, defense_score) %>% View()

rm(list = c("plays_drives.temp", "plays_drives.temp2", "plays_drives.temp3"))
# Build Model for EPA and Test --------------------------------------------

# Split data
ind <- sample(2, nrow(plays_drives.temp4), replace = TRUE, prob = c(0.8, 0.2))
ep_train <- plays_drives.temp4[ind == 1,] %>% ungroup()
ep_test <- plays_drives.temp4[ind == 2,] %>% ungroup()

# Build Model
exp_points <- lm(formula = drive_points ~ down + distance + yards_to_goal + offense_timeouts +
                   defense_timeouts + start_yards_to_goal + clock_in_seconds, 
                 data = ep_train,
                 na.action = na.exclude)

summary(exp_points)

# Evaluate model

ep_test$pred_drive_points <- predict(exp_points, newdata = ep_test, allow.new.levels = TRUE)
ep_test$drive_points_prob <- exp(ep_test$pred_drive_points)/(1+exp(ep_test$pred_drive_points))

# Residuals
ep_test$resid <- ep_test$drive_points - ep_test$pred_drive_points

mean(abs(ep_test$resid), na.rm = T)

ep_test %>% 
  ggplot(aes(x = pred_drive_points, y = drive_points)) +
  geom_point(alpha = 0.1)

ep_test %>% 
  ggplot(aes(x = yards_to_goal)) +
  geom_point(aes(y = pred_drive_points), colour = "blue", alpha = 0.1)

## So may need to do a different type of regression. CFBScrapr uses multinomial logistic regression,
## and predicts the odds of each type of score separately. Then, get EV of each score type using
## probabilities * points values of each. I would assume these should add up to 100% approx.

# In-Game Win Probability Model Based on Game State Factors (no Elo) --------------------------
games.temp <- games.master %>% 
  select(id, home_team, home_points, away_team, away_points)

plays.master.win_prob <- plays.master %>% mutate(home_score = case_when(home == offense ~ offense_score, # Get home lead/deficit
                                               TRUE ~ defense_score),
                        away_score = case_when(away == offense ~ offense_score,
                                               TRUE ~ defense_score),
                        home_score_lead_deficit = home_score - away_score) %>% 
  left_join(games.temp, by = c("home" = "home_team", "away" = "away_team", "game_id" = "id")) # Join games to get final result for each play

# Join Elo Rating as well
elo_ratings <- read_csv(file = "https://raw.githubusercontent.com/kylebennison/staturdays/master/elo_ratings_historic.csv",
                        col_types = list(col_character(), col_character(), col_double(), col_integer(), col_integer(), col_date(format = "%Y-%m-%d"))) %>% 
  select(team, elo_rating, week, season)

elo_ratings_adj <- elo_ratings %>% mutate(week = week + 1)

# Having an issue here where i end up with more rows than before. Join keys may not be unique i.e. multiple matches on rhs for certain plays on lhs
plays.master.win_prob2 <- plays.master.win_prob %>% left_join(elo_ratings, by = c("home" = "team", "week", "year" = "season")) %>% 
  rename(home_elo = elo_rating) %>% 
  left_join(elo_ratings, by = c("away" = "team", "week", "year" = "season")) %>% 
  rename(away_elo = elo_rating) %>% 
  distinct()

# Add win/loss boolean
plays.master.win_prob3 <- plays.master.win_prob2 %>% mutate(home_outcome = case_when(home_points > away_points ~ 1,
                                                           home_points < away_points ~ 0,
                                                           TRUE ~ 0.5))

# Add home_elo_diff
plays.master.win_prob4 <- plays.master.win_prob3 %>% 
  mutate(home_elo_diff = home_elo - away_elo)

# Add home possession flag if they have the ball or not
plays.master.win_prob4 <- plays.master.win_prob4 %>% 
  mutate(home_poss_flag = if_else(home == offense, 1, 0),
         home_timeouts = if_else(home == offense, offense_timeouts, defense_timeouts),
         away_timeouts = if_else(away == offense, offense_timeouts, defense_timeouts))

# Linear Model Test -------------------------------------------------------

# Split data
ind <- sample(2, nrow(plays.master.win_prob4), replace = TRUE, prob = c(0.8, 0.2))
wp_train <- plays.master.win_prob4[ind == 1,]
wp_test <- plays.master.win_prob4[ind == 2,]

# Make prediction model
win_pred <- glm(formula = home_outcome ~ home_score_lead_deficit + clock_in_seconds + down + distance + 
                 yards_to_goal + home_poss_flag + home_timeouts + away_timeouts, 
               data = wp_train,
               family = binomial,
               na.action = na.exclude)

# evaluate model
library(pROC)
win_roc <- roc(wp_test$home_outcome, predict(win_pred, newdata = wp_test))
win_auc <- toString(win_roc$auc)

# plot
ggwin_roc <- ggroc(win_roc)

ggwin_roc +
  geom_text(mapping = aes(x = 0.5, y = 0.5, label = paste0('AUC of ', round(as.double(win_auc), 2))))+
  labs(title = "TD Logistic Model ROC Curve",
       caption = "Data from @cfbscrapR and @CFB_Data | By: Conor McQuiston @ConorMcQ5")+
  theme(plot.title = element_text(hjust = 0.5, size = 15))+
  theme(panel.background = element_rect(color = "gray", size = 0.5, linetype = "solid"))+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())+
  theme(panel.grid.minor=element_blank())

# Predict Wins
wp_test$pred_win <- predict(win_pred, newdata = wp_test, allow.new.levels = TRUE)
wp_test$win_prob <- exp(wp_test$pred_win)/(1+exp(wp_test$pred_win))

# Residuals
wp_test$resid <- wp_test$home_outcome - wp_test$win_prob

mean(abs(wp_test$resid), na.rm = T)

# Test further
test_wins <- wp_test %>% filter(home_outcome == 1)

ggplot(data = test_wins)+
  geom_histogram(mapping = aes(x = win_prob), fill = 'red')+
  xlab('Win')+
  ylab('Count')+
  labs(title = 'Distribution of Win Probability on Wins')+
  theme(panel.background = element_rect(color = "gray", size = 0.5, linetype = "solid"))+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())+
  theme(panel.grid.minor=element_blank())

test_wins <- wp_test %>% filter(home_outcome == 0)

ggplot(data = test_wins)+
  geom_histogram(mapping = aes(x = win_prob), fill = 'red')+
  xlab('Loss')+
  ylab('Count')+
  labs(title = 'Distribution of Win Probability on Losses')+
  theme(panel.background = element_rect(color = "gray", size = 0.5, linetype = "solid"))+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())+
  theme(panel.grid.minor=element_blank())

# Distribution of probs with clock == 0 (game over)
test_wins <- wp_test %>% filter(clock_in_seconds == 0)

ggplot(data = test_wins)+
  geom_histogram(mapping = aes(x = win_prob), fill = 'red')+
  xlab('Game Over')+
  ylab('Count')+
  labs(title = 'Distribution of Win Probability at the end of the game')+
  theme(panel.background = element_rect(color = "gray", size = 0.5, linetype = "solid"))+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank())+
  theme(panel.grid.minor=element_blank())

# Apply to all data
plays.master.win_prob4$pred_win <- predict(win_pred, newdata = plays.master.win_prob4, allow.new.levels = TRUE)
plays.master.win_prob4$win_prob <- exp(plays.master.win_prob4$pred_win)/(1+exp(plays.master.win_prob4$pred_win))

# Check PSU OSU Game
plays.master.win_prob4 %>% filter(year == 2016, home == "Penn State", away == "Ohio State") %>% 
  select(id, home, away, offense, defense, home_score, away_score, minutes, clock_in_seconds, seconds,play_text, pred_win, win_prob) %>% 
  ggplot(aes(x = clock_in_seconds, y = win_prob)) +
  geom_line() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  labs(title = "OSU @ PSU - 2016")

plays.master.win_prob4 %>% filter(year == 2016, home == "Penn State", away == "USC") %>% 
  select(id, home, away, offense, defense, home_score, away_score, minutes, clock_in_seconds, seconds,play_text, pred_win, win_prob) %>% 
  ggplot(aes(x = clock_in_seconds, y = win_prob)) +
  geom_line() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  labs(title = "Rose Bowl: USC-PSU - 2016")

plays.master.win_prob4 %>% filter(year == 2018, home == "Penn State", away == "Appalachian State") %>% 
  select(id, home, away, offense, defense, home_score, away_score, minutes, clock_in_seconds, seconds,play_text, pred_win, win_prob) %>% 
  ggplot(aes(x = clock_in_seconds, y = win_prob)) +
  geom_line() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  labs(title = "App St. PSU OT Thriller")

plays.master.win_prob4 %>% filter(year == 2017, home == "Penn State", away == "Rutgers") %>% 
  select(id, home, away, offense, defense, home_score, away_score, minutes, clock_in_seconds, seconds,play_text, pred_win, win_prob) %>% 
  ggplot(aes(x = clock_in_seconds, y = win_prob)) +
  geom_line() +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  labs(title = "Easy Win over Rutgers")

rm(list = c("plays.master.win_prob", "plays.master.win_prob2", "plays.master.win_prob3"))

# See how various factors in the model correlate to the resulting prediction
plays.master.win_prob4 %>% 
  ggplot(aes(x = home_elo_diff, win_prob)) +
  geom_point(alpha = 0.1)

# Plot predicted vs. actual
plays.master.win_prob4 %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = win_prob), fill = "blue", alpha = 0.5) +
  geom_histogram(mapping = aes(x = home_outcome), fill = "red", alpha = 0.5) +
  labs(title = "Predicted win prob vs. actual result",
       subtitle = "Blue are win probabilities")

# Plot distribution of residuals
plays.master.win_prob4$resid <- plays.master.win_prob4$home_outcome - plays.master.win_prob4$win_prob

plays.master.win_prob4 %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = resid)) +
  labs(title = "Win probability residuals",
       subtitle = "Win prob - actual result")

# Rest of code ------------------------------------------------------------


# Use Cases of Win Prob Model ---------------------------------------------

# Most exciting games - > sum change in WP throughout the game

# Decision making by down/distance/play_type/score (based on WPA pre-play vs. post-play)

# Team offensive tendencies (struggle at making good plays in this situation)

# Team defensive strengths/weaknesses (average negative WPA on 4th down runs, so opponent may try to pass)

# Expected Win Probability ------------------------------------------------

# Summarise by home_deficit, time left in game (rounded to 10 seconds), and home_elo
win_prob_in_game <- plays.master.win_prob3 %>% 
  mutate(rnded_time_remaining = round(clock_in_seconds, -1)) %>% 
  mutate(elo_deficit = ((home_elo - away_elo)%/%50)*50) %>% 
  group_by(home_score_lead_deficit, rnded_time_remaining, elo_deficit) %>% 
  summarise(home_result = mean(home_outcome), count = n())

# Summarise by home_deficit, time left in game (rounded to 30 seconds), down, and yards_to_goal
win_prob_in_game_field_pos <- plays.master.win_prob3 %>% 
  mutate(rnded_time_remaining = (clock_in_seconds%/%45)*45) %>% 
  mutate(elo_deficit = round(home_elo - away_elo, -1)) %>% 
  mutate(home_lead_deficit = case_when(home_score_lead_deficit >= 17 ~ "up_three_score",
                                       home_score_lead_deficit < 17 & home_score_lead_deficit > 8 ~ "up_two_score",
                                       home_score_lead_deficit < 9 & home_score_lead_deficit >= 4 ~ "up_one_score",
                                       home_score_lead_deficit >= 1 & home_score_lead_deficit <= 3 ~ "up_field_goal",
                                       home_score_lead_deficit == 0 ~ "tied",
                                       home_score_lead_deficit < 0 & home_score_lead_deficit >= -3 ~ "down_field_goal",
                                       home_score_lead_deficit < -3 & home_score_lead_deficit >= -8 ~ "down_one_score",
                                       home_score_lead_deficit < -8 & home_score_lead_deficit > -17 ~ "down_two_score",
                                       home_score_lead_deficit <= 17 ~ "down_three_score")) %>% 
  group_by(home_lead_deficit, rnded_time_remaining, down, yards_to_goal) %>% 
  summarise(home_result = mean(home_outcome), count = n())
# Consider adding T/F if home_team has ball or not (is losing/winning team in possession), and 
# bucketing yards_to_goal for "inside_5", "red_zone", "inside_50", "own_half", etc.

## EPA WIP
# The thinking behind this model is to look at each drive per offense, and get the min offense score
# at the start of the drive, and compare it to the max offense score of that drive to find out if they scored.
# We will also look at the defense score difference to see if there was a turnover for a TD.
# 
# We will also look at the following drive in the case that it wasn't a scoring drive to see if the offense scored
# on the following drive.
# 
# From there, we will get the average score for each down, distance, and yards to the goalline.

# Get points scored on each drive
plays.points.temp <- plays.master %>% 
  filter(play_specifics != "Uncategorized") %>% 
  group_by(drive_id, offense) %>% # Looks like some drive ids are for both teams drive.. so grouped by offense and drive, but even then offenses and defenses are both scoring on the same exact drive
  mutate(score_offense = max(offense_score) - min(offense_score), score_defense = -(max(defense_score) - min(defense_score))) #%>% # defense score is made negative
  # ungroup() %>% 
  # mutate(next_drive_offense = lead(max(offense_score), n = 1L, order_by = drive_id) - lead(min(offense_score), n = 1L, order_by = drive_id), next_drive_defense = -(lead(max(defense_score), n = 1L, order_by = drive_id) - lead(min(defense_score), n = 1L, order_by = drive_id)))

plays.master %>% 
  group_by(down, distance, yards_to_goal) %>% 
  mutate(epa_offense = )
  summarise(mean(
    case_when(scoring == T ~ case_when) # If it's scoring drive, get the max offense_score - min offense_score OR max defense_score - min defense_score for that drive
  ))
  
## 2020 Plays
  plays.master.2020 = data.frame()
  for (j in 2020) {
    for (i in 1:15) {
      cat('Loading Plays', j, 'Week', i, '\n')
      full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
      full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
      plays <- fromJSON(getURL(full_url_plays_encoded)) # Pull in API using url
      if(is_empty(plays) == F){
        clockcolumns <- plays %>% unnest_legacy(clock) # Takes clock data out as it's own columns
        plays <- plays %>%
          select(-clock) %>%
          as_tibble() %>%
          mutate(minutes = clockcolumns$minutes, seconds = clockcolumns$seconds) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
          mutate_at(c("minutes", "seconds"), ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
        plays <- tibble(plays)
        plays$week = i
        plays$year = j
        plays.master.2020 = rbind(plays.master.2020, plays, make.row.names=TRUE)
      }
    }
  }
  rm(clockcolumns, pass_rows, plays)

# Bring in team abbreviations
team_url <- "https://api.collegefootballdata.com/teams"
team_info <- fromJSON(getURL(team_url))
  
# Top plays in terms of EPA/PPA for the week (absolute value)
wk <- 4

plays.master.2020 <- left_join(plays.master.2020, team_info, by = c("offense" = "school"), suffix = c("", ".offense"))
plays.master.2020 <- plays.master.2020 %>% select(-last_col(c(0:7))) %>% rename(offense_abb = abbreviation)
plays.master.2020 <- left_join(plays.master.2020, team_info, by = c("defense" = "school"), suffix = c("", ".defense"))
plays.master.2020 <- plays.master.2020 %>% select(-last_col(c(0:7))) %>% rename(defense_abb = abbreviation)

top_plays_ppa <- plays.master.2020 %>% 
  filter(week == wk) %>% 
  mutate(ppa = as.double(ppa)) %>% 
  mutate(ppa_abs = abs(ppa)) %>% 
  arrange(desc(ppa_abs)) %>% 
  select(ppa, offense_score, defense_score, ppa_abs, play_text, play_type, home, away, offense, defense, offense_abb, defense_abb, down, distance, yards_to_goal, minutes, seconds, period) %>% 
  mutate(matchup = paste0(away, " @ ", home), 
         situation = as.character(paste0(as.character(down), case_when(down == 1 ~ "st",
                                                                       down == 2 ~ "nd",
                                                                       down == 3 ~ "rd",
                                                                       down == 4 ~ "th",
                                                                       TRUE ~ ""), " and ", as.character(distance), " from the ", as.character(case_when(yards_to_goal > 50 ~ offense,
                                                                             yards_to_goal < 50 ~ defense,
                                                                             yards_to_goal == 50 ~ "",
                                                                             TRUE ~ "")), " ", as.character(case_when(yards_to_goal > 50 ~ 100L - yards_to_goal,
                                                                                                        yards_to_goal < 50 ~ yards_to_goal,
                                                                                                        yards_to_goal == 50 ~ yards_to_goal,
                                                                                                        TRUE ~ yards_to_goal), " Yard Line"))),
         gametime = paste0(minutes, ":", seconds, " ", period, "Q"),
         score = paste0(offense_abb, " ", offense_score, " - ", defense_score, " ", defense_abb))

top_plays_ppa_tbl <- top_plays_ppa %>% 
  select(ppa_abs, ppa, play_text, matchup, situation, gametime, score) %>% 
  slice_max(n = 10, order_by = ppa_abs) %>% 
  select(-ppa_abs) %>% 
  gt() %>% 
  tab_header(title = paste0("2020 Week ", wk, " Top Plays by EPA"),
             subtitle = "These were the biggest plays of the week on offense and defense") %>% 
  cols_label(ppa = "EPA", play_text = "Description", matchup = "Matchup", situation = "Situation", gametime = "Time", score = "Score (Off - Def)") %>% 
  fmt_number(vars(ppa), decimals = 2, use_seps = FALSE) %>% 
  data_color(columns = vars(ppa), # Use a color scale on win prob
             colors = scales::col_numeric(
               palette = staturdays_palette,
               domain = NULL),
             alpha = 0.7) %>% 
  tab_source_note("@kylebeni012 | @staturdays — Data: @cfb_data PPA")

gtsave(data = top_plays_ppa_tbl, 
       filename = paste0("top_plays_ppa_tbl_", wk, "_", str_replace_all(now(), ":", "."), ".png"),
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots")
