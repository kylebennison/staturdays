# Script to add wpa to plays data
add_wpa <- function(plays_df){
  
  library(xgboost)
  
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")
  source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/source_everything.R")
  
  plays.master <- plays_df
  
  original_columns <- colnames(plays.master)
  
  if(any(class(plays.master) %in% c("data.frame", "tbl_df", "tbl"))){
    
    # Start data manipulation
    cat("Starting success rate calculations\n")
    
    
    rm(plays_df)
    
    start_year <- plays.master$year %>% min()
    end_year <- plays.master$year %>% max()
    start_week <- plays.master$week %>% min()
    end_week <- plays.master$week %>% max()
    
    if(start_week == 1 & end_week >= 15){
      
      games <- get_games(start_year = start_year, end_year = end_year)
      
    } else {
      
      games <- get_games(start_year = start_year, end_year = end_year,
                         start_week = start_week, end_week = end_week)
      
    }
    
    # Get betting data
    if(start_week == 1 & end_week >= 15){
      
      betting <- get_betting(start_year = start_year, end_year = end_year)
      
    } else {
      
      betting <- get_betting(start_year = start_year, end_year = end_year,
                         start_week = start_week, end_week = end_week)
      
    }
    
    # Select columns we need
    games.master <- games[,c(1:25)]
    # Filter unplayed games out
    games.master <- games.master %>% 
      filter(!is.na(home_points))
    
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
    
    ### Get number of plays and % of plays completed
    plays.master.win_prob3 <- plays.master.win_prob3 %>% 
      group_by(game_id) %>% 
      mutate(play_num = row_number(),
             n_plays = n(),
             pct_done = play_num / n_plays)
    
    
    ### keep only the first play when there are duplicate times ####
    #MAKE END ROW FOR EACH GAME THAT SHOWS WHO WON - only for games that are finished
    plays.make.end.rows <- plays.master.win_prob3 %>% 
      group_by(game_id) %>% 
      filter(row_number()==n()) %>% 
      ungroup()
    
    # #filter out timeout rows?
    # plays.master.win_prob3 <- plays.master.win_prob3 %>% group_by(game_id, clock_in_seconds) %>% 
    #   filter(row_number()==1) %>%  #n()) %>% 
    #   ungroup()
    
    x<-plays.make.end.rows %>% 
      mutate(period=20,
             home_timeouts=0,
             away_timeouts=0,
             home_timeouts_new=0,
             away_timeouts_new=0,
             clock_in_seconds=-10000,
             down=5,
             distance=100,
             home_score_lead_deficit=home_score_lead_deficit,
             yards_to_goal=100,
             home_poss_flag=if_else(home_score_lead_deficit > 0, 1, 0),
      )
    
    #add on user created row
    plays.master.win_prob4 <- rbind(plays.master.win_prob3, x)
    
    
    plays.master.win_prob4 <- plays.master.win_prob4 %>% 
      mutate(game_over = ifelse(period==20,1,0))
    
    # Join in betting data
    plays.master.win_prob4 <- plays.master.win_prob4 %>% 
      left_join(betting %>% select(id, spread), by = c("game_id" = "id")) %>% 
      mutate(spread = if_else(is.na(spread) == TRUE, 0, spread)) %>% 
      mutate(spread = spread * (clock_in_seconds/3600)^3) # Decrease spread as game goes on to reduce it's effect
    
    ### NEW
    # Add kickoff indicator
    plays.master.win_prob4 <- plays.master.win_prob4 %>% 
      mutate(is_kickoff = if_else(play_type %in% c("Kickoff", "Kickoff Return (Offense)"), 1, 0)) %>% 
      ungroup()
    
    # Read in Model
    XGBm <- readRDS("Production Models/in_game_wp_v3.1.13.rds")

    # Predict In-Game WP
    
    plays_wp <- plays.master.win_prob4
    
    x.test <- plays_wp %>% 
      select(home_score_lead_deficit, down, distance,
             yards_to_goal, home_poss_flag, home_timeouts_new, away_timeouts_new, 
             home_elo_wp, game_over, spread,
             pct_done, is_kickoff) %>% 
      as.matrix()
    
    dtest <- xgb.DMatrix(x.test,missing=NA)
    
    plays_wp$home_wp <- predict(XGBm, newdata = dtest)
    
    plays_wp <- plays_wp %>% 
      group_by(game_id) %>% 
      mutate(home_wpa = lead(home_wp, n = 1L, order_by = play_num) -
               home_wp,
             away_wpa = -home_wpa,
             offense_wpa = if_else(offense == home, home_wpa, away_wpa),
             defense_wpa = if_else(defense == home, home_wpa, away_wpa))
    
    plays_wp <- plays_wp %>% 
      select(all_of(original_columns), home_wp, contains("wpa"), play_num,
             pct_done, home_elo_wp)
    
    message("Done")
    return(plays_wp)
    
  } else {
    
    message("Data supplied was not of type data.frame or tibble.\n",
            "Please use the plays data provided from the get_plays() function.")
    
  }
  
}
