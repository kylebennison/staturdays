# Train model to predict postseason fantasy points using regular season fantasy points
setwd("Production/NFL/predict_fantasy_points/predict_playoff_fantasy_points/")
getwd()

# Pull in preprocessed data
source("preprocess.r")

res <- preprocess(RUN_TYPE = "TRAIN", predict_year = 2022)

df_list <- readRDS(res)

X_train <- df_list$X_train
y_train <- df_list$y_train
X_test <- df_list$X_test
y_test <- df_list$y_test
X_extra <- df_list$X_extra

dtrain <- xgboost::xgb.DMatrix(as.matrix(X_train), label = y_train)

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
  data = dtrain,
  nrounds = nrounds,
  nfold = 10,
  showsd = TRUE,
  verbose = TRUE,
  early_stopping_rounds = 25,
)

# For interactive hyperparam tuning only
# cv$evaluation_log
# cv$best_iteration
# cat(paste0("Best mean test rmse: ", cv$evaluation_log$test_rmse_mean %>% min(),
#        "\nVs. Previous: ", cv$evaluation_log$test_rmse_mean %>% min() - prev_best))
# cat(paste0("Best sd test rmse: ", cv$evaluation_log$test_rmse_std[cv$best_iteration],
#        "\nVs. Previous ", cv$evaluation_log$test_rmse_std[cv$best_iteration] - prev_best_sd))
# 
# prev_best <- cv$evaluation_log$test_rmse_mean %>% min()
# prev_best_sd <- cv$evaluation_log$test_rmse_std[cv$best_iteration]
# best_params <- params

# 
dtest <- xgboost::xgb.DMatrix(as.matrix(X_test), label = y_test)

watchlist <- list(train=dtrain, test=dtest)

# Train and save final model
xgbmodel <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = cv$best_iteration,
  early_stopping_rounds = 25,
  watchlist = watchlist
)

saveRDS(xgbmodel, "model.RDS")

# Predict on test
X_test$pred <- predict(xgbmodel, newdata = dtest)

X_test$actual <- y_test

X_test <- cbind(X_test, X_extra %>% filter(ttv == "test"))

# RMSE
rmse(X_test$pred, X_test$actual)

# MAE
mean(abs(X_test$actual - X_test$pred))

X_test %>% 
  ggplot(aes(x = actual, y = pred)) +
  geom_point() +
  geom_abline()