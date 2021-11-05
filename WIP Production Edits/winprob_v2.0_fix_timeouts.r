# Get CFB Data API for Play by Play - Use to generate expected points added (EPA)
rm(list=ls())
options(scipen=999)
library(data.table)
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(gt)
library(lubridate)
library(caret)
library(xgboost)

source("Production/source_everything.R")

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

#get plays data and gmes data
#get plays data and gmes data
#plays.master <- get_plays(start_week = 1, end_week = 15, start_year = 2014, end_year = 2020)
games.master <- get_games(start_week = 1, end_week = 16, start_year = 2014, end_year = 2020)

#save plays data so as not to call the API each time
#fwrite(plays.master, "C:/Users/drewb/Desktop/cfb_plays_2014_2020.csv")

plays.master <- fread("C:/Users/drewb/Desktop/cfb_plays_2014_2020.csv")
# plays.master <- fread("Data/plays_2014_2020.csv", encoding = "UTF-8") %>% # Read kyle's data
#   mutate(game_id = str_remove(game_id, "id_")) %>% 
#   mutate(game_id = as.integer(game_id))

#bring in new games
plays.master2 <- get_plays(start_week = 1, end_week = 16, start_year = 2021, end_year = 2021)
games.master2 <- get_games(start_week = 1, end_week = 16, start_year = 2021, end_year = 2021)
games.master2 <- games.master2[,c(1:25)]
games.master2 <- games.master2 %>% filter(!is.na(home_points))

games.master_less <- games.master[,c(1:25)]

#combine data sets 
plays.master <- rbind(plays.master, plays.master2)
games.master <- rbind(games.master_less, games.master2)

rm(games.master2)
rm(plays.master2)
rm(games.master_less)
# Remove overtime games
#overtime_games <- plays.master %>% group_by(game_id) %>%
#  summarise(overtime = ifelse(max(period)>4,1,0)) 

#plays.master <- plays.master %>% 
#  left_join(overtime_games, by=c("game_id")) %>% 
#  filter(overtime==0)
# In-Game Win Probability Model Based on Game State Factors (no Elo) --------------------------
games.temp <- games.master %>% 
  select(id, home_team, home_points, away_team, away_points,
         neutral_site) %>% 
  mutate(id = as.integer(id))

plays.master.win_prob <- plays.master %>% mutate(home_score = case_when(home == offense ~ offense_score, # Get home lead/deficit
                                                                        TRUE ~ defense_score),
                                                 away_score = case_when(away == offense ~ offense_score,
                                                                        TRUE ~ defense_score),
                                                 home_score_lead_deficit = home_score - away_score) %>% 
  left_join(games.temp, by = c("home" = "home_team", "away" = "away_team", "game_id" = "id")) # Join games to get final result for each play

# Add win/loss boolean
plays.master.win_prob2 <- plays.master.win_prob %>% mutate(home_outcome = case_when(home_points > away_points ~ 1,
                                                                                     home_points < away_points ~ 0,
                                                                                     TRUE ~ 0.5))

# Add home possession flag if they have the ball or not
plays.master.win_prob2 <- plays.master.win_prob2 %>% 
  mutate(home_poss_flag = if_else(home == offense, 1, 0),
         home_timeouts = if_else(home == offense, offense_timeouts, defense_timeouts),
         away_timeouts = if_else(away == offense, offense_timeouts, defense_timeouts))

### NEW
# Adjust timeouts based on the half
plays.master.win_prob2 <- plays.master.win_prob2 %>% 
  mutate(home_timeouts_new = if_else(period %in% c(1,2), 
                                     home_timeouts + 3L,
                                     home_timeouts),
         away_timeouts_new = if_else(period %in% c(1,2), 
                                     away_timeouts + 3L,
                                     away_timeouts)
         )
### END NEW

plays.master.win_prob2

rm(plays.master)
rm(plays.master.win_prob)

#elo ratings
elo_ratings <- read_csv(file = "https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/elo_ratings_historic.csv") %>% 
  select(team, elo_rating, week, season)

elo_ratings_adj <- elo_ratings %>% mutate(week = week + 1)

# Having an issue here where I end up with more rows than before. Join keys may not be unique i.e. multiple matches on rhs for certain plays on lhs
plays.master.win_prob3 <- plays.master.win_prob2 %>% left_join(elo_ratings_adj, by = c("home" = "team", "week", "year" = "season")) %>% 
  rename(home_elo = elo_rating) %>% 
  left_join(elo_ratings_adj, by = c("away" = "team", "week", "year" = "season")) %>% 
  rename(away_elo = elo_rating) %>% 
  distinct() %>% 
  mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds) %>% 
  tidyr::replace_na(list(home_timeouts = 0, away_timeouts = 0, home_elo = 1300, away_elo=1300,
                  offense_timeouts = 0, defense_timeouts=0,
                  home_timeouts_new = 0,
                  away_timeouts_new = 0))


# Add home_elo_diff
plays.master.win_prob3 <- plays.master.win_prob3 %>% 
  mutate(home_elo_diff = home_elo - away_elo)

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

### NEW
# Add elo home wp

plays.master.win_prob3 <- plays.master.win_prob3 %>% 
  mutate(home_elo_wp = calc_expected_score(home_elo + 
                                             if_else(neutral_site == FALSE |
                                                       is.na(neutral_site) == TRUE, 
                                                     55L, 
                                                     0L),
                                           away_elo))
### END NEW


### keep only the first play when there are duplicate times ####
#filter out timeout rows?
plays.master.win_prob3 <- plays.master.win_prob3 %>% group_by(game_id, clock_in_seconds) %>% 
  filter(row_number()==1) %>%  #n()) %>% 
  ungroup()


#MAKE END ROW FOR EACH GAME THAT SHOWS WHO WON - only for games that are finished
plays.make.end.rows <- plays.master.win_prob3 %>% 
  group_by(game_id) %>% 
  filter(row_number()==n()) %>% 
  ungroup()


x<-plays.make.end.rows %>% 
  mutate(period=-10,
         home_timeouts=-10,
         away_timeouts=-10,
         home_timeouts_new=-10,
         away_timeouts_new=-10,
         clock_in_seconds=-.5,
         down=-10,
         distance=-10,
         home_score_lead_deficit=home_score_lead_deficit,
         yards_to_goal=-10,
         home_poss_flag=-10,
         )

#add on user created row
plays.master.win_prob4 <- rbind(plays.master.win_prob3, x)


plays.master.win_prob4 <- plays.master.win_prob4 %>% 
  mutate(game_over = ifelse(period==-10,1,0))
  

# Prep Data for Modeling 
y.train <- plays.master.win_prob4$home_outcome
x.train <- plays.master.win_prob4 %>% 
  select(home_score_lead_deficit, clock_in_seconds, down, distance,
         yards_to_goal, home_poss_flag, home_timeouts_new, away_timeouts_new, 
         home_elo_wp, game_over) %>% 
  as.matrix()

#extra columns that aren't used for modeling but might be good for prediction
x.train.leftover <- plays.master.win_prob4 %>% 
  select(-(colnames(x.train))
  )

#pick one game if you want to test
x.test <- plays.master.win_prob4 %>% filter(year == 2021, game_id == "401309885") %>% 
  select(home_score_lead_deficit, clock_in_seconds, down, distance,
         yards_to_goal, home_poss_flag, home_timeouts_new, away_timeouts_new, 
         home_elo_wp, game_over) %>% 
  as.matrix()


# Prep for XGboost
dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


# Use cross validation 
param <- list(  objective           = "binary:logistic",
                gamma               = 0.04, #.02
                booster             = "gbtree",
                eval_metric         = "logloss",
                eta                 = 0.06,
                max_depth           = 20,
                min_child_weight    = 2,
                subsample           = 1,
                colsample_bytree    = 1,
                tree_method = 'hist'
)
#run this for training, otherwise skip
XGBm <- xgb.cv(params=param,nfold=5,nrounds=5000,missing=NA,data=dtrain,print_every_n=10, early_stopping_rounds = 25)

#train the full model
watchlist <- list( train = dtrain)
XGBm <- xgb.train(params=param,
                  nrounds=700,
                  missing=NA,
                  data=dtrain,
                  watchlist=watchlist,
                  print_every_n=100,
                  early_stopping_rounds = 50)

### Try it nflfastR way

# More conservative model = less overfitting
nrounds <- 65
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = 0.2, # higher = less conservative
    gamma = 0,
    subsample = 0.8, # most conservative is 0.5
    colsample_bytree = 0.8, # most conservative is 0.5
    max_depth = 5, # higher = less conservative
    min_child_weight = 2 # higher = more conservative
  )

seasons <- unique(plays.master.win_prob4$year)

model_data <- plays.master.win_prob4 %>% 
  select(year, home_score_lead_deficit, clock_in_seconds, down, distance,
         yards_to_goal, home_poss_flag, home_timeouts_new, away_timeouts_new, 
         home_elo_wp, game_over, home_outcome)

cv_results <- map_dfr(seasons, function(x) {
  test_data <- model_data %>%
    filter(year == x) %>%
    select(-year)
  train_data <- model_data %>%
    filter(year != x) %>%
    select(-year)
  
  full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% select(-home_outcome)),
                                     label = train_data$home_outcome
  )
  wp_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)
  
  preds <- as.data.frame(
    matrix(predict(wp_model, as.matrix(test_data %>% select(-home_outcome))))
  ) %>%
    dplyr::rename(wp = V1)
  
  cv_data <- bind_cols(test_data, preds) %>% mutate(season = x)
  return(cv_data)
})

# TIME FOR BINNING
wp_cv_loso_calibration_results <- cv_results %>%
  # Create BINS for wp:
  mutate(bin_pred_prob = round(wp / 0.01) * .01) %>%
  # Group by both the qtr and bin_pred_prob:
  group_by(bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(
    n_plays = n(),
    n_wins = length(which(home_outcome == 1)),
    bin_actual_prob = n_wins / n_plays
  )

ann_text <- data.frame(
  x = c(.25, 0.75), y = c(0.75, 0.25),
  lab = c("More times\nthan expected", "Fewer times\nthan expected")
)

wp_cv_loso_calibration_results %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    size = "Number of plays",
    x = "Estimated win probability",
    y = "Observed win probability"
  ) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab), size = 3) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 90),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Calculate the calibration error values:
wp_cv_cal_error <- wp_cv_loso_calibration_results %>%
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  summarize(
    weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
    n_wins = sum(n_wins, na.rm = TRUE)
  )

wp_cv_cal_error

# This method appears better calibrated, 
# or at least the calibration plot isn't overfitted thanks to
# leave-one-season-out method

# nflfastR logloss wp_cv_cal_error = 
# A tibble: 1 x 2
# weight_cal_error n_wins
# <dbl>  <int>
#   1          0.00770 337707
# v1 logloss
# A tibble: 1 x 2
#weight_cal_error n_wins
#<dbl>  <int>
#  1           0.0694 337707
# nflfastR logloss w/ max_depth 5 instead of 4
# A tibble: 1 x 2
# weight_cal_error n_wins
# <dbl>  <int>
#   1           0.0127 337707
# nflfastR logloss w/ min_child_weight = 2 instead of 1
# A tibble: 1 x 2
# weight_cal_error n_wins
# <dbl>  <int>
#   1           0.0118 337707

full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = model_data %>% filter(year != 2021) %>% select(-home_outcome)),
                                   label = model_data %>% filter(year != 2021) %>% pull(home_outcome))

wp_model <- xgboost::xgboost(params = params, 
                             data = full_train, 
                             nrounds = nrounds, 
                             verbose = 2)

preds <- as.data.frame(
  matrix(predict(wp_model, as.matrix(model_data %>% filter(year == 2021) %>% select(-home_outcome))))
) %>%
  dplyr::rename(wp = V1)

full_preds <- cbind(plays.master.win_prob4 %>% filter(year == 2021), preds)

# Look at single games from 2021
full_preds %>% 
  filter(game_id == 401309885) %>% 
  ggplot(aes(x = -clock_in_seconds, y = wp)) +
  geom_line() +
  ylim(0,1) +
  geom_vline(xintercept=-900, colour="grey") +
  geom_text(aes(x=-900, label="\nEnd Q3", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
  geom_vline(xintercept=-1800, colour="grey") +
  geom_text(aes(x=-1800, label="\nEnd Q2", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
  geom_vline(xintercept=-2700, colour="grey") +
  geom_text(aes(x=-2700, label="\nEnd Q1", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
  geom_text(aes(x = -3300, y = .9, label = home)) +
  geom_text(aes(x = -3300, y = .1, label = away))

# Look at end-game wps. Not all 0s and 1s so not ideal
full_preds %>%
  filter(game_over == 1) %>%
  select(wp) %>% 
  ggplot(aes(x = wp)) + 
  geom_histogram()

full_preds %>% 
  filter(game_over == 1) %>% 
  select(wp) %>% 
  round(1) %>% 
  group_by(wp) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n))

## Next step is compare the above method's calibration error to the original 
## in-game WP 1.0 in production now.

# NflfastR - 84% of games at .9-1 or .1-0 at game_over
# v1.0 model - 93.6% of all games at .9-1 or .1-0 at game over
# NflfastsR w/ max_depth = 5 -> 91.3%
# w/ min_child_weight 2 -> 91.7%

### End NFLfastR method

library(zoo)

res <- x.test %>% as_tibble()
res$winprob <-predict(XGBm, newdata = dtest)

res %>% ggplot(aes(x=-clock_in_seconds, y=winprob)) +geom_line() + #rollmean(winprob, 5, na.pad = TRUE))
  ylim(0,1) +
  theme_bw() +
  labs(x="",
       y="Clemson win probability",
       title = "Georgia vs. Clemson Win Probability Chart") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_vline(xintercept=-900, colour="grey") +
  geom_text(aes(x=-900, label="\nEnd Q3", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
  geom_vline(xintercept=-1800, colour="grey") +
  geom_text(aes(x=-1800, label="\nEnd Q2", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
  geom_vline(xintercept=-2700, colour="grey") +
  geom_text(aes(x=-2700, label="\nEnd Q1", y=0.8), colour="blue", angle=90, text=element_text(size=9))

ggsave("C:/Users/drewb/Desktop/winprobchart2.png", height = 5, width = 7)

#test on full data, and add back in features so we can look at specific plays
x.train.copy <- x.train
x.train.copy <- x.train.copy %>% as_tibble()
x.train.copy$winprob <- predict(XGBm, newdata = dtrain)
x.train.copy$actualhomeresults <- y.train
#add in original extra data
res <- cbind(x.train.copy, x.train.leftover)

#plot any game
x <- res %>% filter(year==2021,home=="Clemson", away=="Georgia") %>% 
  mutate(diff = winprob - lag(winprob, default = 0)) %>% 
  ggplot(aes(x=-clock_in_seconds, y=winprob)) +geom_line() + #rollmean(winprob, 5, na.pad = TRUE))
  ylim(0,1) +
  theme_bw() +
  labs(x="",
       y="Clemson win probability",
       title = "Georgia vs. Clemson Win Probability Chart") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_vline(xintercept=-900, colour="grey") +
  geom_text(aes(x=-900, label="\nEnd Q3", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
  geom_vline(xintercept=-1800, colour="grey") +
  geom_text(aes(x=-1800, label="\nEnd Q2", y=0.8), colour="blue", angle=90, text=element_text(size=9)) +
  geom_vline(xintercept=-2700, colour="grey") +
  geom_text(aes(x=-2700, label="\nEnd Q1", y=0.8), colour="blue", angle=90, text=element_text(size=9))


# Plot predicted vs. actual
res %>% 
  mutate(win_prob_bucket = round(winprob, digits = 2)) %>% 
  group_by(win_prob_bucket) %>% 
  summarise(mean_actual = mean(home_outcome)) %>% 
  ggplot(aes(x = win_prob_bucket, y = mean_actual)) +
  geom_point() +
  geom_abline()

# Save model
# saveRDS(XGBm, file = "Production Models/in_game_wp.rds")
