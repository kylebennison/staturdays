# Predict postseason fantasy points using regular season fantasy points

library(nflfastR)
library(nflreadr)
library(dplyr)
library(stringr)
library(xgboost)
library(mltools)
library(data.table)

seasons <- c(seq(2015, 2021))
POSITIONS <- c("QB", "WR", "TE", "RB", "FB", "K", "HB")

# Test season

nfl_stats_season <- nflreadr::load_player_stats(seasons = seasons) %>% 
  filter(position %in% POSITIONS)

# Get each player's total epa for the season
y <- nfl_stats_season %>% 
  filter(season_type == "POST") %>% 
  select(player_id, player_display_name, position, fantasy_points_ppr, season) %>% 
  group_by(player_id, player_display_name, position, season) %>% 
  summarise(fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE)) %>% 
  select(player_id, player_display_name, season, position, fantasy_points_ppr) %>% 
  mutate(first_name = word(player_display_name, 1),
         last_name = word(player_display_name, -1)) %>% 
  rename(postseason_ppr = fantasy_points_ppr)

# Train seasons

x <- nfl_stats_season %>% 
  filter(season_type == "REG") %>% 
  group_by(player_id, player_display_name, position, season) %>% 
  summarise(total_ppr = sum(fantasy_points_ppr, na.rm = TRUE),
            avg_weekly_ppr = mean(fantasy_points_ppr, na.rm = TRUE),
            sd_weekly_ppr = sd(fantasy_points_ppr, na.rm = TRUE),
            games_played = n(),
            worst_week = min(fantasy_points_ppr, na.rm = TRUE),
            best_week = max(fantasy_points_ppr, na.rm = TRUE),
            ptile_25 = quantile(fantasy_points_ppr, probs = .25, na.rm = TRUE),
            ptile_50 = quantile(fantasy_points_ppr, probs = .50, na.rm = TRUE),
            ptile_75 = quantile(fantasy_points_ppr, probs = .75, na.rm = TRUE),
  ) %>% 
  rename_with(.fn = ~paste0(.x, "_reg"), .cols = where(is.numeric))

df <- x %>% 
  left_join(y, by = c("player_id", "season_reg" = "season", "player_display_name", "position"))

# Split train and validation

train_test_valid <- function(df, split = c(.7, .2, .1)){
  
  df$ttv <- sample(c("train", "test", "validation"), 
                       size = nrow(df), 
                       replace = TRUE, 
                       prob = c(.7, .2, .1))
  
  return(df)
}

set.seed(0)

df <- df %>% train_test_valid()

df$position <- as.factor(df$position)

df <- mltools::one_hot(as.data.table(df))

# Modeling ----------------------------------------------------------------
X <- as.data.table(df %>% 
                     ungroup() %>% 
                     select(!c("player_id", 
                               "player_display_name", 
                               "first_name", 
                               "last_name",
                               "postseason_ppr",
                               "season_reg"
                     )))

extra_x <- df %>% select(player_id, player_display_name, season_reg, ttv)

y <- df %>% select(postseason_ppr, ttv)
y <- replace(y, is.na(y), 0)

X_train <- X %>% filter(ttv == "train") %>% select(!ttv)
y_train <- y %>% filter(ttv == "train") %>% ungroup() %>% pull("postseason_ppr")

X_gb <- xgboost::xgb.DMatrix(as.matrix(X_train), label = y_train)

nrounds <- 100

params <- list(
  "objective" = "reg:squarederror", # default
  "booster" = "gbtree",
  "eta" = .1,
  "gamma" = 0,
  "max_depth" = 1,
  "min_child_weight" = 0,
  "subsample" = 0.5,
  "colsample_bytree" = 0.8,
  "lambda" = 1,
  "alpha" = 0
)

cv <- xgb.cv(
  params = params,
  data = X_gb,
  nrounds = nrounds,
  nfold = 10,
  showsd = TRUE,
  verbose = TRUE,
  early_stopping_rounds = 25,
)

cv$evaluation_log

xgbmodel <- xgb.train(
  params = params,
  data = X_gb,
  nrounds = cv$best_iteration,
  early_stopping_rounds = 25
)