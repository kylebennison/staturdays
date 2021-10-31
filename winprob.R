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
  select(id, home_team, home_points, away_team, away_points) %>% 
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
                  offense_timeouts = 0, defense_timeouts=0))


# Add home_elo_diff
plays.master.win_prob3 <- plays.master.win_prob3 %>% 
  mutate(home_elo_diff = home_elo - away_elo)


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
         clock_in_seconds=-.5,
         down=-10,
         distance=-10,
         home_score_lead_deficit=home_points-away_points,
         yards_to_goal=-10,
         home_poss_flag=-10,
         )

#add on user created row
plays.master.win_prob4 <- rbind(plays.master.win_prob3, x)


plays.master.win_prob4 <- plays.master.win_prob4 %>% 
  mutate(game_over = ifelse(period==-10,1,0))
  

# Prep Data for Modeling 
y.train <- plays.master.win_prob4$home_outcome
x.train <- plays.master.win_prob4 %>% select(home_score_lead_deficit, clock_in_seconds, down, distance,
                                               yards_to_goal, home_poss_flag, home_timeouts,away_timeouts, home_elo_diff, game_over) %>% 
  as.matrix()

#extra columns that aren't used for modeling but might be good for prediction
x.train.leftover <- plays.master.win_prob4 %>% select(-home_score_lead_deficit, -clock_in_seconds, -down, -distance,
                                                      -yards_to_goal, -home_poss_flag, -home_timeouts,-away_timeouts, -home_elo_diff, -game_over)

#pick one game if you want to test
x.test <- plays.master.win_prob4 %>% filter(year == 2021, home == "Clemson", away == "Georgia") %>% 
  select(home_score_lead_deficit, clock_in_seconds, down, distance,
         yards_to_goal, home_poss_flag, home_timeouts,away_timeouts, home_elo_diff, game_over) %>% 
  as.matrix()


# Prep for XGboost
dtrain <- xgb.DMatrix(x.train,label=y.train,missing=NA)
dtest <- xgb.DMatrix(x.test,missing=NA)


# Use cross validation 
param <- list(  objective           = "binary:logistic",
                gamma               = 0.04, #.02
                booster             = "gbtree",
                eval_metric         = "auc",
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
XGBm <- xgb.train( params=param,nrounds=50,missing=NA,data=dtrain,watchlist=watchlist,print_every_n=100)

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
