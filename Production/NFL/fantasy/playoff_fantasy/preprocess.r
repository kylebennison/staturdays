# Preprocess data

# Import libraries
{
  library(nflfastR)
  library(nflreadr)
  library(dplyr)
  library(stringr)
  library(xgboost)
  library(mltools)
  library(data.table)
  library(ggplot2)
}

# Define eligible positions
POSITIONS <- c("QB", "WR", "TE", "RB", "FB", "K", "HB")

# Identify playoff teams each season
{
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
  teams_2022 <- c("BUF", "MIA", "CIN", "BAL", "JAX", "KC", "LAC", "PHI", "DAL", "NYG", "MIN", "TB", "SF", "SEA")
}

# Start Preprocessing Function
preprocess <- function(
    RUN_TYPE = "TRAIN",
    predict_year = 2022
    ){

  if (RUN_TYPE == "TRAIN"){
    
    SEASONS <- c(seq(2010, predict_year - 1))

  } else{
    
    SEASONS <- predict_year
    
  }
  
  nfl_stats_season <- nflreadr::load_player_stats(seasons = SEASONS) %>% 
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
        season == 2022 ~ if_else(recent_team %in% teams_2022, TRUE, FALSE),
        TRUE ~ FALSE
      )) %>% 
    filter(made_playoffs == TRUE)
  
  # Add NFL team records
  team_schedules <- nflreadr::load_schedules(seasons = SEASONS)
  
  team_results <- team_schedules %>% 
    filter(game_type == "REG") %>% 
    clean_homeaway() %>% 
    group_by(team, season) %>% 
    mutate(
      win = case_when(
        result > 0 & location == "home" ~ 1,
        result < 0 & location == "away" ~ 1,
        result == 0 ~ 0.5,
        TRUE ~ 0
      )
    ) %>% 
    summarise(
      wins = sum(win),
      games = n_distinct(week),
      avg_spread = mean(result),
      sd_spread = sd(result),
      point_diff = sum(result),
      avg_points_for = mean(team_score),
      avg_points_against = mean(opponent_score)
    ) %>% 
    mutate(
      win_rate = wins/games
    )
  
  # Get postseason stats
  y <- nfl_stats_season %>% 
    filter(season_type == "POST") %>% 
    select(player_id, player_display_name, position, fantasy_points_ppr, season, recent_team) %>% 
    group_by(player_id, player_display_name, position, season, recent_team) %>% 
    summarise(fantasy_points_ppr = sum(fantasy_points_ppr, na.rm = TRUE)) %>% 
    select(player_id, player_display_name, season, position, fantasy_points_ppr, recent_team) %>% 
    mutate(first_name = word(player_display_name, 1),
           last_name = word(player_display_name, -1)) %>% 
    rename(postseason_ppr = fantasy_points_ppr)
  
  # Train seasons
  
  x <- nfl_stats_season %>% 
    filter(season_type == "REG") %>% 
    group_by(player_id, player_display_name, position, season, recent_team) %>% 
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
  
  x <- x %>% 
    left_join(team_results, by = c("season_reg" = "season", "recent_team" = "team"))
  
  df <- x %>% 
    left_join(y, by = c("player_id",
                        "season_reg" = "season",
                        "player_display_name",
                        "position",
                        "recent_team"))
  
  # Split train and validation
  if (RUN_TYPE == "TRAIN"){
    
    train_test_valid <- function(df, split = c(.7, .2, .1)){
      
      df$ttv <- sample(c("train", "test", "validation"), 
                       size = nrow(df), 
                       replace = TRUE, 
                       prob = c(.7, .2, .1))
      
      return(df)
    }
    
  } else {
    
    
    train_test_valid <- function(df){
      
      df <- df %>% 
        mutate(ttv = "test")
      
      return(df)
    }
    
  }
  
  
  set.seed(0)
  
  df <- df %>% train_test_valid()
  
  
  extra_x <- df %>% select(player_id, player_display_name, season_reg, ttv, recent_team, position)
  
  
  df$position <- as.factor(df$position)
  
  df <- mltools::one_hot(as.data.table(df))
  
  # Split train and validation
  
  
  X <- as.data.table(df %>% 
                       ungroup() %>% 
                       select(!c("player_id", 
                                 "player_display_name", 
                                 "first_name", 
                                 "last_name",
                                 "postseason_ppr",
                                 "season_reg",
                                 "recent_team"
                       )))
  
  y <- df %>% select(postseason_ppr, ttv)
  y <- replace(y, is.na(y), 0)
  
  
  X_test <- X %>% filter(ttv == "test") %>% select(!ttv)
  y_test <- y %>% filter(ttv == "test") %>% ungroup() %>% pull("postseason_ppr")
  
  
  if(RUN_TYPE == "TRAIN"){
    
    X_train <- X %>% filter(ttv == "train") %>% select(!ttv)
    y_train <- y %>% filter(ttv == "train") %>% ungroup() %>% pull("postseason_ppr")
    
    df_list <- list("X_train" = X_train,
                    "y_train" = y_train,
                    "X_test" = X_test,
                    "y_test" = y_test,
                    "X_extra" = extra_x
                    )
    
  } else {
    df_list <- list("X_test" = X_test,
                    "y_test" = y_test,
                    "X_extra" = extra_x
    )
  }
  
  
  save_name <- if (RUN_TYPE == "TRAIN") {
    "preprocess_train.RDS"
  } else {
    "preprocess_predict.RDS"
  }
  
  saveRDS(df_list, save_name)
  
  return(save_name)
}
