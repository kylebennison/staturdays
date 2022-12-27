library(statRdaysCFB)
library(dplyr)

start_year <- 2015
end_year <- 2020

# Get CFB player season advanced stats

# Season PPA

tryCatch(
  {
    
    ppa <- readRDS(glue::glue("Data/player_ppa_raw_{start_year}_{end_year}"))  
    
  },
  error = function(cond){
    message(cond)
    ppa <- get_anything("https://api.collegefootballdata.com/ppa/players/season",
                 start_year = start_year,
                 end_year = end_year)
    saveRDS(ppa, glue::glue("Data/player_ppa_raw_{start_year}_{end_year}"))
  }
)

# Season usage

tryCatch(
  {
    usage <- readRDS(glue::glue("Data/player_usage_raw_{start_year}_{end_year}"))
  },
  error = function(cond){
    message(cond)
    usage <- get_anything("https://api.collegefootballdata.com/player/usage",
                          start_year = start_year,
                          end_year = end_year)
    saveRDS(usage, glue::glue("Data/player_usage_raw_{start_year}_{end_year}"))
  }
)


# Summarise by player ID for (presumably) their entire college career

ppa_total <- ppa %>% 
  group_by(id, name, position, conference) %>% 
  summarise(tepa = sum(totalPPA.all),
            aepa = mean(averagePPA.all), # This is an average of an average
            n_plays = sum(countablePlays))

usage_total <- usage %>% 
  group_by(id, name, position, conference) %>% 
  summarise(avg_usage = mean(usage.overall)) # This is an average of an average


# Join together

college_df <- left_join(ppa_total, usage_total, 
                        by = c("id", "name", "position", "conference"), 
                        keep = FALSE)
