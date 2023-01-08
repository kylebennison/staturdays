library(nflfastR)
library(nflreadr)
library(dplyr)
library(stringr)
# TODO: Get 5 year tEPA and make that the response variable
# So use 5 years of college data to predict 5 years of NFL tEPA
# note I think NA NFL data (didn't get drafted) needs to be imputed as
# 0 because otherwise I'm only training on guys that made it which limits the
# model usefulness to known/presumed draftees and not the entire CFB population.
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
