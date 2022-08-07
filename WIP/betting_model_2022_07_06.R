library(dplyr)

bet_df <- readRDS("Data/betting_prepped.rds")


# drop any forward-looking or non-useful columns

bet_df <- bet_df %>%
  select(
    !c(
      "id",
      "start_date",
      "start_time_tbd",
      "venue",
      "home_team",
      "home_line_scores",
      "home_post_win_prob",
      "away_team",
      "away_line_scores",
      "away_post_win_prob",
      "excitement_index",
      "highlights",
      "notes",
      "seasonType",
      "conference_home",
      "conference_away",
      "conference_home_home",
      "division_home",
      "conference_away_away",
      "division_away",
      "conference_home_home_home",
      "conference_away_away_away",
      "conference_home_home_home_home",
      "date_home",
      "conference_away_away_away_away",
      "date_away",
      "first_name_home",
      "last_name_home",
      "hire_date_home",
      "coach_home",
      "first_name_away",
      "last_name_away",
      "hire_date_away",
      "coach_away"
    )
  )

# One-hot encode
library(mltools)
library(data.table)
bet_df <- as.data.table(bet_df %>% 
                          mutate(across(.cols = c("season_type",
                                                  "neutral_site",
                                                  "conference_game",
                                                  "home_conference",
                                                  "home_division",
                                                  "away_conference",
                                                  "away_division"),
                                        .fns = as.factor)))

bet_df <- one_hot(bet_df, cols = c(
    "season_type",
    "neutral_site",
    "conference_game",
    "home_conference",
    "home_division",
    "away_conference",
    "away_division"
  ),
  sparsifyNAs = TRUE,
  dropCols = TRUE) %>% 
  select(!contains("FALSE"))


# Convert to numeric
bet_df <- bet_df %>%
  mutate(across(
    .cols = c(
      "points_home",
      "points_away",
      "talent_home",
      "talent_away",
      "srs_home",
      "sp_overall_home",
      "sp_offense_home",
      "sp_defense_home",
      "srs_away",
      "sp_overall_away",
      "sp_offense_away",
      "sp_defense_away"
    ),
    .fns = as.numeric
  ))

# Clean column names
names(bet_df) <- janitor::make_clean_names(names(bet_df))

# Remove more forward-looking columns
bet_df <- bet_df %>% 
  select(!c(
    season,
    attendance,
    venue_id,
    home_id,
    home_postgame_elo,
    home_points,
    away_points,
    away_postgame_elo,
    away_id,
    season_y,
    week_x,
    week_y,
    season_y,
    season_away,
    year_home,
    year_away,
    year_home_home,
    year_away_away
  ))

# Pull out responses
response_home_win <- bet_df %>% 
  pull(response_home_win)
response_home_spread <- bet_df %>% 
  pull(response_home_spread)
response_total_points <- bet_df %>% 
  pull(response_total_points)

bet_df <- bet_df %>% 
  select(!contains("response"))

# Impute medians
impute_median <- function(column){
  
  column[is.na(column)] <- median(column, na.rm = TRUE)
  
  return(column)
}

bet_df <- bet_df %>% 
  mutate(across(.cols = everything(),
                .fns = ~ impute_median(.x)))

# Normalize all columns
normalize_columns <- function(column){
  
  return((column - min(column)) / (max(column) - min(column)))
  
}

bet_df <- bet_df %>% 
  mutate(across(.cols = everything(),
                .fns = normalize_columns))

bet_df <- bet_df %>% 
  select(!where(~all(is.nan(.x))))


# Predict Home Win --------------------------------------------------------

# Boosted Tree
library(xgboost)

set.seed(1)
train_ind <- sample(nrow(bet_df), nrow(bet_df) * .8)

x_train <- bet_df[train_ind,]

y_train <- response_home_win[train_ind]

x_test <- bet_df[-train_ind,]

y_test <- response_home_win[-train_ind]

x_train_xgb <- xgb.DMatrix(as.matrix(x_train), label=y_train, missing = NA)



# Start CV Hyperparam Tuning ----------------------------------------------


    # Define model params
results <- data.frame()
param_grid <- expand.grid(eta = c(.1), 
                          nrounds = c(100),
                          max_depth = c(3),
                          subsample = c(1),
                          colsample_bytree = c(1),
                          min_child_weight = c(8,9,10),
                          eval_metric = c("logloss"),
                          gamma = c(0,.1,.2),
                          alpha = c(.01,.1,.2),
                          lambda = c(1.1,1.2,1.5)) %>% 
  unique()
pb = txtProgressBar(min = 0, max = nrow(param_grid), initial = 0)

set.seed(1)
for(i in 1:nrow(param_grid)){
  if(i == 1){
    time_start <- lubridate::now()
    cat("\nStarting Tuning at ", format.Date(time_start), "\n")
  }
  nrounds <- param_grid$nrounds[i]
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = param_grid$eval_metric[i],
      eta = param_grid$eta[i], # higher = less conservative
      gamma = param_grid$gamma[i], # higher = more conservative (amount of loss required to make a split)
      subsample = param_grid$subsample[i], # most conservative is 0.5
      colsample_bytree = param_grid$colsample_bytree[i], # most conservative is 0.5
      max_depth = param_grid$max_depth[i], # higher = less conservative
      min_child_weight = param_grid$min_child_weight[i], # higher = more conservative,
      alpha = param_grid$alpha[i], # Higher is more conservative
      lambda = param_grid$lambda[i] # Higher is more conservative
    )
  
  cv_results <- xgb.cv(
    params = params,
    data = x_train_xgb,
    nrounds = nrounds,
    verbose = 0,
    nfold = 5
  )
  metric_name = as.name(paste0("test_", param_grid$eval_metric[i], "_mean"))
  best_test_loss <- cv_results$evaluation_log %>% pull(metric_name) %>% min()
  
  best_test_ind <- which(cv_results$evaluation_log %>% pull(metric_name) == best_test_loss)
  
  results <- rbind(results, data.frame(round = i, 
                                       best_test_loss = best_test_loss,
                                       best_test_n_trees = best_test_ind,
                                       params = params))
  
  setTxtProgressBar(pb,i)
  close(pb)
  if(i == nrow(param_grid)){
    time_end <- lubridate::now()
    cat("\nFinished Tuning at ", format.Date(time_end), "\n")
    cat("CV Tuning Took ", round(abs(as.numeric(time_end - time_start, units = "secs")), 1),
        "seconds\n")
  }
}

# Save best param results so far
if(exists("best_results")) {
  cat(
    "Best test loss is :",
    min(results$best_test_loss),
    "\n",
    "Previous best is :",
    best_results$best_test_loss,
    "\n"
  )
  
  if (min(results$best_test_loss) < best_results$best_test_loss) {
    cat("Improved so using new parameters", results)
  } else {
    cat("No improvement so keeping existing parameters")
    no_improvement_rounds <- no_improvement_rounds + 1
    cat("\nNo improvement in ", no_improvement_rounds, " rounds\n")
  }
  
  # Only save new results if they are better than the previous best results
  if (min(best_results$best_test_loss) > min(results$best_test_loss)) {
    best_results <-
      results[which(results$best_test_loss == min(results$best_test_loss)), ]
  }
} else {
  cat("First run complete. Saving best params to best_results object")
  best_results <-
    results[which(results$best_test_loss == min(results$best_test_loss)), ]
  
}


# Best params
#' colsample = 1
#' subsample = 1
#' n_rounds = 87
#' eta = .1
#' gamma = 0 (TBD)
#' alpha = 0 (TBD)
#' lambda = 1 (TBD)
#' max_depth = 3
#' min_child_weight = 6

# Random Forest

# Bagging

# Lasso/Ridge Regression
library(glmnet)

x <- as.matrix(bet_df)
y <- response_home_spread

model_glm <- glmnet(x = x,
                    y = y,
                    family = "gaussian",
                    alpha = 1)

summary(model_glm)

model_glm

plot(model_glm)

coefs <- coef(model_glm, s = 0, exact = FALSE)

max(coefs)
