
# DraftKings Live Odds API ----------------------------------------------------

library(httr)
library(tidyverse)
library(jsonlite)
library(data.table)

url <- "https://sportsbook-us-pa.draftkings.com//sites/US-PA-SB/api/v2/eventgroup/88670775/full?includePromotions=true&format=json"

response <- GET(url = url,
                content_type_json())

data <- content(response, as = "text", enconding = "UTF-8") %>% 
  fromJSON(flatten = TRUE) %>% 
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

moneylines <- d_final %>% 
  select(providerOfferId, oddsAmerican, label) %>% 
  mutate(favorite = case_when(str_detect(oddsAmerican, "-") == TRUE ~ "favorite",
                              str_detect(oddsAmerican, "\\+") == TRUE ~ "underdog",
                              TRUE ~ "unknown")) %>% 
  pivot_wider(names_from = c(favorite), values_from = c(label, oddsAmerican))

# Now you can join this with games data to get moneyline by game.
# Just need to figure out join keys. Team names should be unique but we don't know home and away based on this data, so date would be better but we don't have it.