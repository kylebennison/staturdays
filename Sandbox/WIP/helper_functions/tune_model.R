#' Tune xgb parameters with a grid search
#' 
#' @description Warning: this will take a very long time to run and use a lot of memory.
tune_params <- function(
    start_eta = .1,
    start_rounds = 100,
    start_depth = 2,
    start_subsample = .6,
    start_colsample = .6,
    start_child_weight = 5,
    tuning_rounds = 10
){
  no_improvement_rounds <- 0
  last_result <- 0
  
  while(no_improvement_rounds < tuning_rounds){
    
    # Define model params
    results <- data.frame()
    param_grid <- expand.grid(eta = c(start_eta*.1, start_eta, start_eta * 2), 
                              nrounds = c(max(start_rounds * .5, 1), start_rounds, start_rounds * 2),
                              max_depth = c(max(start_depth - 1, 1), start_depth, start_depth + 1),
                              subsample = c(max(start_subsample*.9, .5), start_subsample, min(start_subsample*1.1, 1)),
                              colsample_bytree = c(max(start_colsample*.9, .5), start_colsample, min(start_colsample*1.1, 1)),
                              min_child_weight = c(max(start_child_weight - 1, 0), start_child_weight, start_child_weight + 1),
                              eval_metric = c("logloss"),
                              gamma = c(0),
                              alpha = c(0),
                              lambda = c(1)) %>% 
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
        cat("CV Tuning Took ", round(abs(time_end - time_start), 1),
            "seconds\n")
      }
    }
    
    # Save best param results so far
    cat(
      "Best test loss is :", min(results$best_test_loss), "\n",
      "Previous best is :", best_results$best_test_loss, "\n"
    )
    
    if (min(results$best_test_loss) < best_results$best_test_loss) {
      cat("Improved so using new parameters", results)
    } else {
      cat("No improvement so keeping existing parameters")
      no_improvement_rounds <- no_improvement_rounds + 1
      cat("\nNo improvement in ", no_improvement_rounds, " rounds\n")
    }
    
    # Only save new results if they are better than the previous best results
    if(min(best_results$best_test_loss) > min(results$best_test_loss)){
      best_results <- results[which(results$best_test_loss == min(results$best_test_loss)),]
    }
    
    # Grab the best params to update the param grid
    best_ind_this_time <- which(results$best_test_loss == min(results$best_test_loss))
    start_eta = results$params.eta[best_ind_this_time]
    start_rounds = results$round[best_ind_this_time]
    start_depth = results$params.max_depth[best_ind_this_time]
    start_subsample = results$params.subsample[best_ind_this_time]
    start_colsample = results$params.colsample_bytree[best_ind_this_time]
    start_child_weight = results$params.min_child_weight[best_ind_this_time]
    
    if(no_improvement_rounds == tuning_rounds){
      cat("\nNo improvement in ", no_improvement_rounds,
          " consecutive rounds so stopping tuning")
    }
    
    if(min(results$best_test_loss) == last_result){
      warning("Test Loss Reached a Minimum")
      break
    }
    
    last_result <- min(results$best_test_loss)
    
  }
}