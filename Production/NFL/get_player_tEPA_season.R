library(nflfastR)
library(nflreadr)
library(dplyr)
library(stringr)

season <- 2021

nfl_stats_season <- nflreadr::load_player_stats(seasons = season)

# Get each player's total epa for the season
nfl_df <- nfl_stats_season %>% 
  select(player_id, player_display_name, position, contains("epa")) %>% 
  group_by(player_id, player_display_name, position) %>% 
  summarise(total_epa = sum(passing_epa, rushing_epa, receiving_epa, na.rm = TRUE)) %>% 
  select(player_id, player_display_name, position, total_epa) %>% 
  mutate(first_name = word(player_display_name, 1)) %>% 
  mutate(last_name = word(player_display_name, -1))
