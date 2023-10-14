# Make predictions for latest season
setwd("Production/NFL/predict_fantasy_points/predict_playoff_fantasy_points/")

# Run preprocess and get filename of preprocessed data
source("preprocess.r")
PREDICT_YEAR <- 2022

res <- preprocess(RUN_TYPE = "PREDICT", predict_year = PREDICT_YEAR)

df_list <- readRDS(res)

X_test <- df_list$X_test
y_test <- df_list$y_test
X_extra <- df_list$X_extra

# Load in trained model
model <- readRDS("model.RDS")

# Check if any feature columns from the model are missing in the test data
missing_feats <- model$feature_names[!model$feature_names %in% names(X_test)]

# TODO: Find a way to not manually do this in the future
X_test$position_K <- 0

X_test <- X_test %>% relocate(position_K, .before = position_QB)

# Predict on test
X_test$pred <- predict(model, newdata = dtest)

X_test <- cbind(X_test, X_extra %>% filter(ttv == "test"))

# Save preds to csv
X_test %>% 
  select(player_display_name, position, recent_team, pred) %>% 
  arrange(desc(pred)) %>% 
  fwrite(glue::glue("preds_{PREDICT_YEAR}.csv"))

if(exists("y_test")){
  
  # If results are available, compare preds to actual for the season
  X_test %>% cbind(y_test) %>% 
    select(player_display_name, position, recent_team, pred, y_test) %>% 
    arrange(desc(pred)) %>% 
    fwrite(glue::glue("preds_actual_{PREDICT_YEAR}.csv"))
  
  library(statRdaysCFB)
  X_test %>% cbind(y_test) %>% 
    ggplot(aes(x = y_test, y = pred)) +
    geom_point(alpha = .4, color = staturdays_colors("dark_blue")) +
    geom_abline() +
    staturdays_theme +
    labs(x = "Actual Fantasy Points", y = "Predicted") +
    ggrepel::geom_text_repel(aes(label = if_else(y_test > 40 | pred > 40, player_display_name, ""))) +
    coord_cartesian(clip = "off")
  
  ggsave("actual_vs_pred_plot.png",
         width = 160, height = 130, units = "mm")
  
  
  pred_actual <- X_test %>% 
    cbind(y_test)
  
  # RMSE
  sqrt(mean((pred_actual$pred - pred_actual$y_test)^2))
  
  # MAE
  mean(abs(pred_actual$pred - pred_actual$y_test))
  # Predict on validation
  
  # Select team based on predicted points (high to low, one team each and position limits)
  
  # Select top players
  # Confirm one per team
  # Confirm right positions
  # If not, find player with the min dropoff to the replacement player and swap in
  # Repeat until n_teams = 14 and n_qb's = 2, rbs = 3, wr's = 3, te = 2, K = 1
  
  team <- X_test %>% 
    cbind(y_test) %>% 
    slice_max(order_by = pred, n = 100) %>% 
    select(player_display_name, position, recent_team, pred, y_test) %>% 
    group_by(position) %>% 
    mutate(value_over_next_best = pred - lag(pred, n = 1L, order_by = pred))
  
  team %>% 
    fwrite(glue::glue("vorp_{PREDICT_YEAR}.csv"))
}

# TODO: Choose best combo of players based on roster restrictions