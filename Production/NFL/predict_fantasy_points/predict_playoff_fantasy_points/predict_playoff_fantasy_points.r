# Predict postseason fantasy points using reguLA season fantasy points

library(nflfastR)
library(nflreadr)
library(dplyr)
library(stringr)
library(xgboost)
library(mltools)
library(data.table)
library(ggplot2)

seasons <- c(seq(2010, 2021))
POSITIONS <- c("QB", "WR", "TE", "RB", "FB", "K", "HB")

# Playoff teams
teams_2010 <- c("NE", "PIT", "IND", "BAL", "NYJ", "KC", "ATL", "CHI", "GB", "NO", "SEA", "PHI")
teams_2011 <- c("NE", "BAL", "HOU", "DEN", "PIT", "CIN", "NYG", "SF", "NO", "GB", "DET", "ATL")
teams_2012 <- c("NE", "BAL", "HOU", "DEN", "IND", "CIN", "ATL", "SF", "GB", "WAS", "MIN", "SEA")
teams_2013 <- c("NE", "DEN", "IND", "CIN", "KC", "LAC", "SEA", "CAR", "SF", "NO", "PHI", "GB")
teams_2014 <- c("NE", "DEN", "IND", "CIN", "PIT", "BAL", "SEA", "GB", "DAL", "DET", "CAR", "ARI")
teams_2015 <- c("NE", "DEN", "CIN", "HOU", "KC", "PIT", "CAR", "ARI", "MIN", "WAS", "GB", "SEA")
teams_2016 <- c("NE", "LV", "PIT", "HOU", "KC", "MIA", "DAL", "DET", "SEA", "ATL", "NYG", "GB")
teams_2017 <- c("NE", "PIT", "JAX", "KC", "TEN", "BUF", "PHI", "MIN", "NO", "LA", "CAR", "ATL")
teams_2018 <- c("KC", "NE", "HOU", "BAL", "LAC", "IND", "NO", "LA", "CHI", "DAL", "SEA", "PHI")
teams_2019 <- c("BAL", "KC", "NE", "HOU", "BUF", "TEN", "SF", "GB", "NO", "SEA", "MIN", "PHI")
teams_2020 <- c("KC", "BUF", "PIT", "TEN", "IND", "CLE", "GB", "NO", "SEA", "WAS", "TB", "LA", "BAL", "CHI") # missing BAL, CHI, 
teams_2021 <- c("BUF", "NE", "CIN", "PIT", "TEN", "KC", "LV", "DAL", "PHI", "GB", "TB", "LA", "ARI", "SF")

# Test season
# TODO: since we will know which players/teams make the playoffs, let's train only
# on players who make the playoffs. Need to get playoff T/F for each player's team
# and filter for T

nfl_stats_season <- nflreadr::load_player_stats(seasons = seasons) %>% 
  filter(position %in% POSITIONS) %>% 
  mutate(
    made_playoffs = case_when(
      season == 2010 ~ if_else(recent_team %in% teams_2010, TRUE, FALSE),
      season == 2011 ~ if_else(recent_team %in% teams_2011, TRUE, FALSE),
      season == 2012 ~ if_else(recent_team %in% teams_2012, TRUE, FALSE),
      season == 2013 ~ if_else(recent_team %in% teams_2013, TRUE, FALSE),
      season == 2014 ~ if_else(recent_team %in% teams_2014, TRUE, FALSE),
      season == 2015 ~ if_else(recent_team %in% teams_2015, TRUE, FALSE),
      season == 2016 ~ if_else(recent_team %in% teams_2016, TRUE, FALSE),
      season == 2017 ~ if_else(recent_team %in% teams_2017, TRUE, FALSE),
      season == 2018 ~ if_else(recent_team %in% teams_2018, TRUE, FALSE),
      season == 2019 ~ if_else(recent_team %in% teams_2019, TRUE, FALSE),
      season == 2020 ~ if_else(recent_team %in% teams_2020, TRUE, FALSE),
      season == 2021 ~ if_else(recent_team %in% teams_2021, TRUE, FALSE),
      TRUE ~ FALSE
  )) %>% 
  filter(made_playoffs == TRUE)

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

nrounds <- 200

params <- list(
  "objective" = "reg:squarederror", # default
  "booster" = "gblinear", # performing better than a tree
  "eta" = .5, # best
  "gamma" = 0,
  "max_depth" = 3, # best
  "min_child_weight" = 0, # best
  "subsample" = .75, # best
  "colsample_bytree" = .5, # best
  "lambda" = 1, # best
  "alpha" = 0 # best
)

set.seed(0)
cv <- xgb.cv(
  params = params,
  data = X_gb,
  nrounds = nrounds,
  nfold = 10,
  showsd = TRUE,
  verbose = TRUE,
  early_stopping_rounds = 25,
)

# cv$evaluation_log
cv$best_iteration
cat(paste0("Best mean test rmse: ", cv$evaluation_log$test_rmse_mean %>% min(),
       "\nVs. Previous: ", cv$evaluation_log$test_rmse_mean %>% min() - prev_best))
cat(paste0("Best sd test rmse: ", cv$evaluation_log$test_rmse_std[cv$best_iteration],
       "\nVs. Previous ", cv$evaluation_log$test_rmse_std[cv$best_iteration] - prev_best_sd))

prev_best <- cv$evaluation_log$test_rmse_mean %>% min()
prev_best_sd <- cv$evaluation_log$test_rmse_std[cv$best_iteration]
best_params <- params

X_test <- X %>% filter(ttv == "test") %>% select(!ttv)
y_test <- y %>% filter(ttv == "test") %>% ungroup() %>% pull("postseason_ppr")

dtest <- xgboost::xgb.DMatrix(as.matrix(X_test), label = y_test)

watchlist <- list(train=X_gb, test=dtest)

xgbmodel <- xgb.train(
  params = params,
  data = X_gb,
  nrounds = cv$best_iteration,
  early_stopping_rounds = 25,
  watchlist = watchlist
)

# Predict on test
y_preds <- predict(xgbmodel, newdata = dtest)

X_test$pred <- y_preds

X_test$act <- y_test

X_test <- cbind(X_test, extra_x %>% filter(ttv == "test"))

mean(abs(X_test$act - X_test$pred))

X_test %>% 
  ggplot(aes(x = act, y = pred)) +
  geom_point() +
  geom_abline()

# Predict on validation

# Select team based on predicted points (high to low, one team each and position limits)

# Compare predicted best team to actual best team

# Predict and pick team for 2022