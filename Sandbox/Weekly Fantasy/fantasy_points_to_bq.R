library(nflreadr)
library(bigrquery)

# TODO: Use service account
source("config.r")

# Confirm it's time to update the table
current_week <- get_current_week()

query <- glue::glue(
  "SELECT MAX(week) AS max_week FROM `{PROJECT}.{DATASET}.player_points`"
)

result <- bq_project_query(PROJECT, query)
max_week <- bq_table_download(result)
max_week <- max_week$max_week

if(current_week > max_week){
  
  # Load in player database to get IDs and full names
  players <- load_players()
  
  players <- players %>% 
    select(display_name, gsis_id)

  # Get fantasy data
  df <- nflreadr::load_player_stats()
  
  df <- df %>% 
    select(player_id, player_display_name, position, recent_team, season, week, fantasy_points_ppr) %>% 
    filter(position %in% POSITIONS,
           week == max(week)) %>% 
    rename(player_name = player_display_name,
           team = recent_team,
           fantasy_points = fantasy_points_ppr)
  
  kicking <- load_player_stats(stat_type = "kicking")
  
  # Get kicker full names
  kicking <- kicking %>% 
    left_join(players, by = c("player_id" = "gsis_id"))
  
  kicking <- kicking %>% 
    select(player_id, display_name, team, season, week, fg_made, pat_made) %>% 
    filter(week == max(week)) %>% 
    mutate(position = "K",
           fantasy_points = fg_made * 3 + pat_made) %>% 
    select(!c(fg_made, pat_made)) %>% 
    rename(player_name = display_name)
  
  df <- df %>% rbind(kicking)
  
  # Write to BQ
  table <- bq_table(project = PROJECT, DATASET = DATASET, table = "player_points")
  
  
  bq_table_upload(
    table, df
  )

}

