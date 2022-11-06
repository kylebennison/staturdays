
# DraftKings Live Odds API ----------------------------------------------------

library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)

url <- "https://sportsbook-us-pa.draftkings.com//sites/US-PA-SB/api/v2/eventgroup/88670775/full?includePromotions=true&format=json"

response <- GET(url = url,
                content_type_json())

data <- content(response, as = "text", enconding = "UTF-8") %>% 
  jsonlite::fromJSON(flatten = TRUE) %>% 
  tibble() %>% 
  readr::type_convert()

d2 <- data$.$eventGroup$offerCategories %>%
  filter(name == "Game Lines")

d3 <- d2$offerSubcategoryDescriptors

d4 <- d3[[1]]

d5 <- d4$offerSubcategory.offers

d6 <- d5[[1]]

# Stuck here, need the "outcomes" list inside of each list which represents one matchup

d_final <- tibble()
for (i in 1:length(d6)){
  message("starting ", i)
d7 <- d6[[i]]$outcomes[[3]] # The 3rd list here is the moneylines. Spreads are [[1]]
if(ncol(d7) == 9){ # Append only if columns match
d_final <- rbind(d_final, d7)
}
}

# d_final contains moneyline odds for each team, for multiple matchups throughout the season
# Pivoting wider to get matchups
# Issue: When both teams have "-" odds, both are labelled as the favorite and can't pivot_wider

moneylines <- d_final %>% 
  select(providerOfferId, oddsAmerican, label) %>% 
  mutate(favorite = case_when(str_detect(oddsAmerican, "-") == TRUE ~ "favorite",
                              str_detect(oddsAmerican, "\\+") == TRUE ~ "underdog",
                              TRUE ~ "unknown")) %>% 
  group_by(providerOfferId) %>% 
  mutate(odds_sum = sum(abs(as.integer(oddsAmerican)))) %>% 
  mutate(cnt = favorite == "favorite", # Check how many teams per game are marked as the favorite
         cnt = sum(cnt)) %>% 
  ungroup() %>% 
  mutate(implied_odds = case_when(str_detect(oddsAmerican, "-") == TRUE ~ abs(as.integer(oddsAmerican))/(abs(as.integer(oddsAmerican)) + 100),
                                  str_detect(oddsAmerican, "\\+") == TRUE ~ 100/(abs(as.integer(oddsAmerican))+100),
                                  TRUE ~ 0)) %>% 
  group_by(providerOfferId) %>% 
  mutate(cnt_odds = max(implied_odds) == implied_odds, # Check if the teams odds are the highest odds
         cnt_odds = sum(cnt_odds),
         team_num = rank(label)) %>% # Check if both teams have the same odds
  mutate(favorite = case_when(cnt != 1 & cnt_odds == 1 & implied_odds == max(implied_odds) ~ "favorite", # If both teams have negative odds or both have positive odds, then pick a favorite based on implied odds, and if both are have the same exact implied odds, label them as a toss up
                              cnt != 1 & cnt_odds == 1 & implied_odds < max(implied_odds) ~ "underdog",
                              cnt != 1 & cnt_odds == 2 ~ paste0("toss_up", "_", team_num),
                              TRUE ~ favorite)) %>% 
  pivot_wider(names_from = c(favorite), values_from = c(label, oddsAmerican, implied_odds))

# Now you can join this with games data to get moneyline by game.
# Just need to figure out join keys. Team names should be unique but we don't know home and away based on this data, so date would be better but we don't have it.

ml_df <- moneylines %>% 
  mutate(join_key = paste0(label_favorite, label_underdog))