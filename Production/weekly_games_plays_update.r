
# Weekly Update to Plays and Games RDS ------------------------------------

source("Production/source_everything.r")

games_path <- "Data/games_2021.rds"

plays_path <- "Data/plays_2021.rds"

# Can probably just make one api call a week and then completely overwrite the 
# entire file to ensure it has the most updated date

games <- get_games(2021, 2021)

plays <- get_plays(1, 16, 2021, 2021)

# Write to file

saveRDS(games, file = games_path)

saveRDS(plays, file = plays_path)