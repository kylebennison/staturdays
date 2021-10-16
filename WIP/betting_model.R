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
# Get everything important
source("Production/source_everything.r")

# Data --------------------------------------------------------------------

games <- get_games(2014, 2019)
records <- tibble()
for(yr in 2014:2019){
  
  record_url <- paste0("https://api.collegefootballdata.com/records?year=", as.character(yr), "&seasonType=both")
  r1 <- cfbd_api(record_url, key = my_key)
  records <- rbind(records, r1)
  message("Done year ", yr)
  
}
plays <- fread("Data/plays_2014_2020.csv")
lines <- tibble()
for(yr in 2014:2019){
  
  betting_url <- paste0("https://api.collegefootballdata.com/lines?year=", as.character(yr), "&seasonType=both")
  b1 <- cfbd_api(betting_url, key = my_key)
  b1 <- b1 %>% unnest(cols = lines)
  lines <- rbind(lines, b1)
  message("Done year ", yr)
  
}
talent <- tibble()
for(yr in 2014:2019){
  
  talent_url <- paste0("https://api.collegefootballdata.com/talent?year=", as.character(yr))
  t1 <- cfbd_api(talent_url, key = my_key)
  talent <- rbind(talent, t1)
  message("Done year ", yr)
  
}
rankings <- tibble()
for(yr in 2014:2019){
  
  rankings_url <- paste0("https://api.collegefootballdata.com/rankings?year=", as.character(yr))
  r1 <- cfbd_api(rankings_url, key = my_key)
  rankings <- rbind(rankings, r1)
  message("Done year ", yr)
  
}
returning <- tibble()
for(yr in 2014:2019){
  
  returning_url <- paste0("https://api.collegefootballdata.com/player/returning?year=", as.character(yr))
  r1 <- cfbd_api(returning_url, key = my_key)
  r1 <- r1
  returning <- rbind(returning, r1)
  message("Done year ", yr)
  
}
recruiting <- tibble()
for(yr in 2014:2019){
  
  recruiting_url <- paste0("https://api.collegefootballdata.com/recruiting/teams?year=", as.character(yr))
  r1 <- cfbd_api(recruiting_url, key = my_key)
  r1 <- r1
  recruiting <- rbind(recruiting, r1)
  message("Done year ", yr)
  
}
# ppa <- tibble()
# for(yr in 2014:2019){
#   
#   ppa_url <- paste0("https://api.collegefootballdata.com/ppa/players/season?year=", as.character(yr))
#   p1 <- cfbd_api(ppa_url, key = my_key)
#   p1 <- p1
#   ppa <- rbind(ppa, p1)
#   message("Done year ", yr)
#   
# }
stats_advanced <- tibble()
for(yr in 2014:2019){
  
  stats_url <- paste0("https://api.collegefootballdata.com/stats/season/advanced?year=", as.character(yr))
  s1 <- cfbd_api(stats_url, key = my_key)
  s1 <- s1
  stats_advanced <- rbind(stats_advanced, s1)
  message("Done year ", yr)
  
}

rm(list = c("r1", "s1", "t1", "b1"))


# Clean any data ----------------------------------------------------------

# Unnest columns
# rankings <- rankings %>% unnest(cols = polls) %>% unnest(cols = ranks)

# Summarise a single line per game
lines_tmp <- lines %>% 
  mutate(spread = as.double(spread),
         overUnder = as.integer(overUnder)) %>% 
  group_by(id, season, seasonType, week, homeTeam, awayTeam,
           homeScore, awayScore) %>% 
  summarise(avg_spread = mean(spread),
            avg_over_under = mean(overUnder, na.rm = T))

lines <- lines_tmp

rm(lines_tmp)


# Prep for joining ---------

# Join last years stats to this year's games for predictions
# ppa_prep <- ppa %>% 
#   mutate(join_year = season + 1L)

records_prep <- records %>% 
  mutate(join_year = year + 1L)

stats_prep <- stats_advanced %>% 
  mutate(join_year = season + 1L)

# Need to confirm that the rankings come out after that week is played.
# i.e. Week 1 rankings should be used to predict Week 2 games.
# If they come out before the week, they should be used to predict the same week
# upon review this data doesn't look reliable so omitting
# rankings_prep <- rankings %>% 
#   filter(poll %in% c("AP Top 25", 
#                      "Playoff Committee Rankings")) %>% 
#   mutate(consensus_rank == mean(rank, na.rm = TRUE),
#          join_week = week + 1L)

# Build a giant table -----------------------------------------------------

big_table1 <- games %>% 
  inner_join(lines, by = 
               c("id", 
                 "home_team" = "homeTeam", 
                 "away_team" = "awayTeam",
                 "home_points" = "homeScore",
                 "away_points" = "awayScore"))

big_table2 <- big_table1 %>% 
  left_join(stats_prep, by = c("home_team" = "team",
                                   "season.x" = "join_year")) %>% 
  left_join(stats_prep, by = c("away_team" = "team",
                               "season.x" = "join_year"),
            suffix = c("_home", "_away"))

#### PPA is by player, not team. Omit.
big_table3 <- big_table2 %>% 
  left_join(records_prep, by = c("home_team" = "team",
                                 "season.x" = "join_year")) %>% 
  left_join(records_prep, by = c("away_team" = "team",
                                 "season.x" = "join_year"),
            suffix = c("_home", "_away"))

# These three can join with the same season as games
big_table4 <- big_table3 %>% 
  left_join(recruiting, by = c("home_team" = "team",
                               "season.x" = "year")) %>% 
  left_join(recruiting, by = c("away_team" = "team",
                               "season.x" = "year"),
            suffix = c("_home", "_away")) %>% 
  left_join(returning, by = c("home_team" = "team",
                               "season.x" = "season")) %>% 
  left_join(returning, by = c("away_team" = "team",
                               "season.x" = "season"),
            suffix = c("_home", "_away")) %>% 
  left_join(talent, by = c("home_team" = "school",
                           "season.x" = "year")) %>% 
  left_join(talent, by = c("away_team" = "school",
                           "season.x" = "year"),
            suffix = c("_home", "_away"))

# Summarise plays into success rate, ppa/game over rolling past 4 games and join
# Note, currently grouping by offense
p2 <- plays %>% 
  filter(year != 2020) %>% 
  add_success()

# Add elo so we can get moving average elo of opponent
elo <- get_elo(2014, 2019)

# Mutate week for joining so it's pre-game elo
elo <- elo %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date)) # get next week's game date.

games2 <- games %>% 
  mutate(id = as.character(id), start = 4L)

p2_5 <- p2 %>%
  mutate(game_id = str_sub(game_id, start = 4L)) %>% 
  left_join(games2, by = c("game_id" = "id")) %>%  # Get game start_time info for joining to elo
  mutate(start_date = lubridate::as_datetime(start_date)) %>%
  left_join(elo,
            by = c(
              "home_team" = "team",
              "start_date" = "join_date",
              "season" = "season"
            )) %>%
  left_join(
    elo,
    by = c(
      "away_team" = "team",
      "start_date" = "join_date",
      "season" = "season"
    ),
    suffix = c("_home", "_away")
  ) %>%
  mutate(
    offense_elo = if_else(offense == home_team, elo_rating_home, elo_rating_away),
    defense_elo = if_else(defense == home_team, elo_rating_home, elo_rating_away)
  )

### across(everything(), .fns = sum)

p3 <- p2_5 %>%
  group_by(game_id, offense) %>%
  summarise(
    completion_rate = sum(pass_completion, na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
    pass_touchdown_rate = sum(pass_touchdown, na.rm = TRUE) /
      sum(pass_attempt, na.rm = TRUE),
    interception_rate = sum(pass_intercepted, na.rm = TRUE) /
      sum(pass_attempt, na.rm = TRUE),
    sack_rate = sum(passer_sacked, na.rm = TRUE) / n(),
    success_rate = sum(success, na.rm = TRUE) / n(),
    pass_rate = sum(pass_attempt, na.rm = TRUE) / sum(
      sum(pass_attempt, na.rm = TRUE),
      sum(rush_attempt, na.rm = TRUE),
      na.rm = TRUE
    ),
    rush_touchdown_rate = sum(rush_touchdown, na.rm = TRUE) /
      sum(rush_attempt, na.rm = TRUE),
    rush_rate = sum(rush_attempt, na.rm = TRUE) / sum(
      sum(pass_attempt, na.rm = TRUE),
      sum(rush_attempt, na.rm = TRUE),
      na.rm = TRUE
    ),
    pass_fumble_rate = sum(pass_fumbled, na.rm = TRUE) /
      sum(
        sum(pass_attempt, na.rm = TRUE),
        sum(passer_sacked, na.rm = TRUE),
        na.rm = TRUE
      ),
    rush_fumble_rate = sum(rush_fumbled, na.rm = TRUE) /
      sum(rush_attempt, na.rm = TRUE),
    total_touchdown_rate = sum(
      sum(pass_touchdown, na.rm = TRUE),
      sum(rush_touchdown, na.rm = TRUE)
    ) / sum(pass_attempt, rush_attempt, na.rm = TRUE),
    avg_ppa = mean(ppa, na.rm = TRUE),
    n = n(),
    points_for = max(offense_score),
    points_against = max(defense_score),
    home_spread = points_for - points_against,
    won_game = if_else(max(offense_score) > max(defense_score), 1, 0),
    opponent_elo = max(defense_elo)
  )

rm("p2", "p2_5")

# Get moving average of 4 previous games (note, this currently takes averages of averages on completion rate and other stats)
p4 <- p3 %>% 
  arrange(game_id) %>% 
  group_by(offense) %>% 
  mutate(across(.cols = -game_id, 
                .fns = ~ zoo::rollapply(.x,
                                        width = list(c(-4, -3, -2, -1)),
                                        FUN = mean,
                                        na.rm = TRUE,
                                        fill = NA),
                .names = "{.col}_ma_4")) %>% 
  select(offense, game_id, contains("_ma_4")) %>% # Filter out stats from the current game since that should be hidden from the model
  mutate(game_id = as.integer(str_sub(game_id, start = 4L))) # Remove "id_" for joining
  
rm(list = c("p2", "p3"))

big_table5 <- big_table4 %>% 
  left_join(p4, by = c("home_team" = "offense",
                       "id" = "game_id")) %>% 
  left_join(p4, by = c("away_team" = "offense",
                       "id" = "game_id"),
            suffix = c("_home", "_away"))

# May want to add elo to give some idea of the quality of their opponents in wins/losses
elo <- get_elo(2014,2019)

# Mutate week for joining so it's pre-game elo
elo <- elo %>% 
  group_by(team) %>% 
  mutate(join_date = lead(date, n = 1L, order_by = date)) # get next week's game date.

big_table6 <- big_table5 %>% 
  mutate(start_date = lubridate::as_datetime(start_date)) %>% 
  left_join(elo, by = c("home_team" = "team",
                        "start_date" = "join_date",
                        "season.x" = "season")) %>% 
  left_join(elo, by = c("away_team" = "team",
                        "start_date" = "join_date",
                        "season.x" = "season"),
            suffix = c("_home", "_away"))

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

big_table7 <- big_table6 %>% 
  mutate(home_elo_adv = elo_rating_home + if_else(neutral_site == TRUE, 0, 55) - elo_rating_away,
         home_elo_wp = calc_expected_score(elo_rating_home + if_else(neutral_site == TRUE, 0, 55),
                                           elo_rating_away))

# Get avg elo of last 4 opponents, and average win rate in last 4 games
big_table8 <- big_table7 %>% 
  

rm(list = c(paste0("big_table", 1:6)))
# Cross-validate xgboost model --------------------------------------------
# Prep data
prepped_table <- big_table7 %>% 
  filter(season.x != 2014) %>% # remove 2014 since it doesn't have 2013 data
  ungroup() %>% 
  mutate(response_home_win = if_else(home_points > away_points, 1, 0),
         response_total_points = home_points + away_points,
         response_home_spread = away_points - home_points)

# NOTE: right now including pre-game line and spread as predictors, but in the
# future may want to omit
# Predict Winner

# pass 2 ------------------------------------------------------------------
prepped_table %>% 
  filter(season.x != 2019) %>% 
  ### Do one-hot encoding here
  mutate(value = 1) %>% 
  spread(c(neutral_site, conference_game), value, fill = 0) %>% 
  ### finish one-hot encoding
  select(avg_ppa_ma_4_home, avg_ppa_ma_4_away,
         home_elo_adv, home_elo_wp,
         if_else(neutral_site == TRUE, 1, 0),
         if_else(conference_game == TRUE, 1, 0))

# pass 1 ------------------------------------------------------------------



x.train <- prepped_table %>% 
  filter(season.x != 2019) %>% 
  select(-c(id, home_points, away_points, home_line_scores, away_line_scores,
            attendance, home_post_win_prob, away_post_win_prob,
            excitement_index,
            avg_spread, avg_over_under,
            response_home_win,
            response_total_points,
            response_home_spread)) %>% 
  select(where(is.numeric)) %>% 
  as.matrix()

x.train.response <- prepped_table %>% 
  filter(season.x != 2019) %>% 
  select(response_home_win) %>% 
  as.matrix()

x.train.leftover <- prepped_table %>% 
  filter(season.x != 2019) %>% 
  select(c(id, home_points, away_points, home_line_scores, away_line_scores,
            attendance, home_post_win_prob, away_post_win_prob,
            excitement_index,
            avg_spread, avg_over_under,
           response_home_win,
           response_total_points,
           response_home_spread),
         !where(is.numeric))

x.test <- prepped_table %>% 
  filter(season.x == 2019) %>% 
  select(-c(id, home_points, away_points, home_line_scores, away_line_scores,
            attendance, home_post_win_prob, away_post_win_prob,
            excitement_index,
            avg_spread, avg_over_under,
            response_home_win,
            response_total_points,
            response_home_spread)) %>% 
  select(where(is.numeric)) %>% 
  as.matrix()

x.test.leftover <- prepped_table %>% 
  filter(season.x == 2019) %>% 
  select(c(id, home_points, away_points, home_line_scores, away_line_scores,
           attendance, home_post_win_prob, away_post_win_prob,
           excitement_index,
           avg_spread, avg_over_under,
           response_home_win,
           response_total_points,
           response_home_spread),
         !where(is.numeric))

library(xgboost)

dtrain <- xgb.DMatrix(x.train,label=x.train.response,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)

# Use cross validation 
param <- list(  objective           = "binary:logistic",
                gamma               = 0.04, #.02
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.06,
                max_depth           = 15,
                min_child_weight    = 2,
                subsample           = 1,
                colsample_bytree    = 1,
                tree_method = 'hist'
)

#run this for training, otherwise skip
XGBm <- xgb.cv(params=param,nfold=5,nrounds=100,missing=NA,data=dtrain,print_every_n=10, early_stopping_rounds = 25)

# Tune paramaters

results <- tibble()

best_param = list()
best_rmse = Inf
best_rmse_index = 0
for(i in 1:25){
  message("Starting round ", i)
  param <- list(  objective           = "binary:logistic",
                  gamma               = runif(1, 0, .2), #.02
                  booster             = "gbtree",
                  eval_metric         = "auc",
                  eta                 = runif(1, 0.01, .3),
                  max_depth           = sample(5:25, 1),
                  min_child_weight    = 2,
                  subsample           = runif(1, .6, 1),
                  colsample_bytree    = runif(1, .5, 1),
                  tree_method = 'hist'
  )
  
  mdcv <- xgb.cv(params=param,nfold=5,nrounds=100,missing=NA,data=dtrain,print_every_n=10, early_stopping_rounds = 25)
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_auc_mean
  
  if (min_rmse < best_rmse) {
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_param <- param
  }
}

# The best index (min_rmse_index) is the best "nround" in the model
nround = best_rmse_index

#train the full model
watchlist <- list( train = dtrain)
XGBm <- xgb.train( params=best_param,nrounds=nround,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=100)

# Predict winners
res <- x.test %>% as_tibble() %>% cbind(x.test.leftover)

res$pred_home_wp <- predict(XGBm, newdata = dtest)

res %>% 
  mutate(bucket_pred = round(pred_home_wp, digits = 2)) %>% 
  group_by(bucket_pred) %>% 
  summarise(actual_home_res = mean(response_home_win, na.rm = TRUE),
            n = n()) %>% 
  ggplot(aes(x = bucket_pred, y = actual_home_res)) +
  geom_point(alpha = .1) +
  geom_abline(linetype = 2, color = "red")

library(pROC)

roc <- pROC::roc(res$response_home_win, res$pred_home_wp)
auc <- auc(roc)

ggroc(roc) +
  geom_text(label = paste0("AUC of ", round(auc, 2)),
            aes(x = .5, y = .5))

brier <- mean((res$pred_home_wp - res$response_home_win)^2)
mean(brier)

xgb.importance(model = XGBm)

### So first pass is bad. Brier of .175. Small sample size but still not great.
# Need to look at what's useful and what's not, and reeval how much moving avg.
# data to include. Right now it's 4. maybe try 8.


# Predict total points ----------------------------------------------------

x.train <- prepped_table %>% 
  filter(season.x != 2019) %>% 
  select(-c(id, home_points, away_points, home_line_scores, away_line_scores,
            attendance, home_post_win_prob, away_post_win_prob,
            excitement_index,
            avg_spread, avg_over_under,
            response_home_win,
            response_total_points,
            response_home_spread)) %>% 
  select(where(is.numeric)) %>% 
  as.matrix()

x.train.response <- prepped_table %>% 
  filter(season.x != 2019) %>% 
  select(response_total_points) %>% 
  as.matrix()

x.train.leftover <- prepped_table %>% 
  filter(season.x != 2019) %>% 
  select(c(id, home_points, away_points, home_line_scores, away_line_scores,
           attendance, home_post_win_prob, away_post_win_prob,
           excitement_index,
           avg_spread, avg_over_under,
           response_home_win,
           response_total_points,
           response_home_spread),
         !where(is.numeric))

x.test <- prepped_table %>% 
  filter(season.x == 2019) %>% 
  select(-c(id, home_points, away_points, home_line_scores, away_line_scores,
            attendance, home_post_win_prob, away_post_win_prob,
            excitement_index,
            avg_spread, avg_over_under,
            response_home_win,
            response_total_points,
            response_home_spread)) %>% 
  select(where(is.numeric)) %>% 
  as.matrix()

x.test.leftover <- prepped_table %>% 
  filter(season.x == 2019) %>% 
  select(c(id, home_points, away_points, home_line_scores, away_line_scores,
           attendance, home_post_win_prob, away_post_win_prob,
           excitement_index,
           avg_spread, avg_over_under,
           response_home_win,
           response_total_points,
           response_home_spread),
         !where(is.numeric))

dtrain <- xgb.DMatrix(x.train,label=x.train.response,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)

# Use cross validation 
param <- list(  objective           = "reg:squarederror",
                gamma               = 0.04, #.02
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.06,
                max_depth           = 15,
                min_child_weight    = 2,
                subsample           = 1,
                colsample_bytree    = 1,
                tree_method = 'hist'
)

#run this for training, otherwise skip
XGBm <- xgb.cv(params=param,nfold=5,nrounds=100,missing=NA,data=dtrain,print_every_n=10, early_stopping_rounds = 25)

# Tune paramaters

results <- tibble()

best_param = list()
best_rmse = Inf
best_rmse_index = 0
for(i in 1:25){
  message("Starting round ", i)
  param <- list(  objective           = "binary:logistic",
                  gamma               = runif(1, 0, .2), #.02
                  booster             = "gbtree",
                  eval_metric         = "auc",
                  eta                 = runif(1, 0.01, .3),
                  max_depth           = sample(5:25, 1),
                  min_child_weight    = 2,
                  subsample           = runif(1, .6, 1),
                  colsample_bytree    = runif(1, .5, 1),
                  tree_method = 'hist'
  )
  
  mdcv <- xgb.cv(params=param,nfold=5,nrounds=100,missing=NA,data=dtrain,print_every_n=10, early_stopping_rounds = 25)
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_auc_mean
  
  if (min_rmse < best_rmse) {
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_param <- param
  }
}

# The best index (min_rmse_index) is the best "nround" in the model
nround = best_rmse_index

#train the full model
watchlist <- list( train = dtrain)
XGBm <- xgb.train( params=best_param,nrounds=nround,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=100)

# Predict winners
res <- x.test %>% as_tibble() %>% cbind(x.test.leftover)

res$pred_total_points <- predict(XGBm, newdata = dtest)

res %>% 
  ggplot(aes(x = response_total_points, y = pred_total_points)) +
  geom_point(alpha = .1) +
  geom_abline(linetype = 2, color = "red") +
  xlim(0,100) +
  ylim(0, 100)

rmse <- sqrt(mean((res$pred_total_points - res$response_total_points)^2))

xgb.importance(model = XGBm)

#' Off by an average of 14 points (i think), or 17.5. not sure.
#' mean(abs(residual diff)) == 14 while the rmse above is 17.5

# Predict spread

# Apply model to get predicted WP vs. implied odds ------------------------


# Set betting rules on when to bet and how much ---------------------------


# Calculate season returns ------------------------------------------------


# --------------------------- Old Code -----------------
# Games Historic

games <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/games_historic.csv")

# Get points scored over last 8 home games and away games
games_record <- games %>% 
  group_by(home_team) %>% 
  mutate(last_8_home_points = frollmean(home_points, n = 8L),
         last_8_home_points = lag(last_8_home_points, n = 1L)) %>% # Use previous week's 8-game moving average since this week's includes the result of today's game
  group_by(away_team) %>% 
  mutate(last_8_away_points = frollmean(away_points, n = 8L),
         last_8_away_points = lag(last_8_away_points, n = 1L))

# Get points scored over last 8 games, home and away
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

# Get only the consensus lines
betting_consensus <- betting_raw %>% 
  filter(provider == "consensus")

# Add betting data to games data
games_lines_joined <- games_record_home_away %>% 
  inner_join(betting_consensus, by = 
               c("id", 
                 "home_team" = "homeTeam", 
                 "away_team" = "awayTeam",
                 "home_points" = "homeScore",
                 "away_points" = "awayScore")) %>% 
  select(home_team, away_team, home_points, away_points, spread, formattedSpread, overUnder, everything())

# Identify the favorite
joined_2 <- games_lines_joined %>% 
  mutate(favorite_team = str_replace_all(formattedSpread, "[0-9]", ""), # Get rid of the numbers in the spread
         favorite_team = str_replace_all(favorite_team, "[//.//-]", ""), # Get rid of decimals and minuses
         favorite_team = str_trim(favorite_team, side = "both")) # Get rid of whitespace

# Modify columns
joined_3 <- joined_2 %>% 
  mutate(spread = as.double(spread),
         overUnder = as.double(overUnder),
         actual_spread = away_points - home_points, # Positive means home team lost
         home_team_covered = if_else(actual_spread < spread, T, F),
         favorite_covered = case_when(actual_spread < spread & home_team == favorite_team ~ TRUE,
                                      actual_spread > spread & away_team == favorite_team ~ TRUE,
                                      TRUE ~ FALSE),
         actual_total_points = home_points + away_points,
         over_hit = if_else(actual_total_points > overUnder, TRUE, FALSE)
  )

# Rearrange columns
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
summary(lm(actual_total_points ~ overUnder, data = joined_4)) # Significant relationship and good predictor of actual score

# Spread vs. actual spread
joined_4 %>% 
  ggplot(aes(x = spread, y = actual_spread)) +
  geom_point(alpha = 0.3, color = staturdays_colors("lightest_blue")) +
  geom_abline()

# R-squared
summary(lm(actual_spread ~ spread, data = joined_4)) # Significant

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
  mutate(over_bucket = cut(overUnder, breaks = 10)) %>% 
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

### Using Previous season record

### Using Last X games average score for over-under
summary(lm(actual_total_points ~ last_8_home_points + last_8_away_points, data = joined_4))

### Using Previous result against this team
### Using Elo Rating and Elo Win Probability
elo_historic <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv",
                      encoding = "UTF-8")
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

source("Production/elo_calc_functions.R")

# Calc win prob and elo_diff
joined_elo <- joined_elo %>% 
  mutate(elo_diff = favorite_elo - underdog_elo,
         elo_pred_win_prob = calc_expected_score(favorite_elo, underdog_elo),
         favorite_won = case_when(home_team == favorite_team & home_points > away_points ~ TRUE,
                                  home_team == favorite_team & home_points < away_points ~ FALSE,
                                  away_team == favorite_team & home_points < away_points ~ TRUE,
                                  TRUE ~ FALSE))

# Make sure all datatypes I want to use are ok for logistic regression
str(joined_elo)

## Model 1
# Select initial variables
model_1_data <- joined_elo %>% 
  select(home_team, away_team, favorite_team, home_points, away_points,
         overUnder, over_hit, spread,
         last_8_points_home_team, last_8_points_away_team,
         elo_diff, elo_pred_win_prob, favorite_won, favorite_covered)

# Build a model
model_1 <- glm(favorite_covered ~ ., data = model_1_data,
               family = "binomial")

## Model 2
# Select initial variables
model_2_data <- joined_elo %>% 
  select(home_points, away_points,
         overUnder, over_hit, spread,
         last_8_points_home_team, last_8_points_away_team,
         elo_diff, elo_pred_win_prob, favorite_won, favorite_covered)

# Build a model
model_2 <- glm(favorite_covered ~ ., data = model_2_data,
               family = "binomial")

## Model 3 (built with only things we know before the game)
# Select initial variables
model_3_data <- joined_elo %>% 
  mutate(last_8_points_favorite = if_else(home_team == favorite_team, last_8_points_home_team,
                                          last_8_points_away_team),
         last_8_points_underdog = if_else(home_team == favorite_team, last_8_points_away_team,
                                          last_8_points_home_team)) %>% 
  select(overUnder, spread,
         last_8_points_favorite, last_8_points_underdog,
         elo_diff, elo_pred_win_prob, favorite_covered)

# Build a model
model_3 <- glm(favorite_covered ~ ., data = model_3_data,
               family = "binomial") # Not a good model

## Model 4 (predict Over_hit)
# Select initial variables
model_4_data <- joined_elo %>% 
  mutate(last_8_points_favorite = if_else(home_team == favorite_team, last_8_points_home_team,
                                          last_8_points_away_team),
         last_8_points_underdog = if_else(home_team == favorite_team, last_8_points_away_team,
                                          last_8_points_home_team)) %>% 
  select(overUnder, spread, over_hit,
         last_8_points_favorite, last_8_points_underdog,
         elo_diff, elo_pred_win_prob)

# Build a model
model_4 <- glm(over_hit ~ ., data = model_4_data,
               family = "binomial") # Elo significant here

# Plot residuals
model_4$residuals %>% 
  tibble() %>% 
  rename(resid = ".") %>% 
  mutate(index = row_number()) %>% 
  ggplot(aes(x = index, y = resid)) + 
  geom_point(position = "identity")

### Advanced metrics for previous games including success rate

