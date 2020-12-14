# Clear environment and read in libraries
rm(list=ls())
library(scales)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(gt)
library(lubridate)
library(ggimage)
library(grid)
library(png)
library(bit64)
library(data.table)

passing_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=regular&category=passing&year="
rushing_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=regular&category=rushing&year="
receiving_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=regular&category=receiving&year="

passing.master <- tibble()
rushing.master <- tibble()
receiving.master <- tibble()
for (yr in 2000:2020){
  message("Running Year ", yr)
  full_pass_url <- paste0(passing_url, as.character(yr))
  full_rush_url <- paste0(rushing_url, as.character(yr))
  full_receiving_url <- paste0(receiving_url, as.character(yr))
  passing <- fromJSON(full_pass_url) %>% tibble()
  passing <- passing %>% mutate(year = yr)
  rushing <- fromJSON(full_rush_url) %>% tibble()
  rushing <- rushing %>% mutate(year = yr)
  receiving <- fromJSON(full_receiving_url) %>% tibble()
  receiving <- receiving %>% mutate(year = yr)
  passing.master <- rbind(passing.master, passing)
  rushing.master <- rbind(rushing.master, rushing)
  receiving.master <- rbind(receiving.master, receiving)
}

teams_url <- "https://api.collegefootballdata.com/records?year="
teams.master <- tibble()
for (yr in 2000:2020){
  message("Running Year ", yr)
  full_team_url <- paste0(teams_url, as.character(yr))
  teams <- fromJSON(full_team_url) %>% tibble()
  teams <- data.table(teams)
  teams <- teams %>% mutate(year = yr) %>% 
    select(year, team, total.games, total.wins) %>% 
    mutate(winPerc = total.wins/total.games)
  teams.master <- rbind(teams.master, teams, row.names=NULL)
}

passing_spread <- passing.master %>% pivot_wider(names_from = c("statType"), values_from = c("stat"))
rushing_spread <- rushing.master %>% pivot_wider(names_from = c("statType"), values_from = c("stat"))
receiving_spread <- receiving.master %>% pivot_wider(names_from = c("statType"), values_from = c("stat"))

joined_stats <- passing_spread %>% full_join(rushing_spread, by = c("playerId", "year"), suffix = c("_pass", "_rush"))

joined_stats <- joined_stats %>% full_join(receiving_spread, by = c("playerId", "year"), suffix = c("", "_receiving"))


# Unify some columns
joined_stats <- joined_stats %>% 
  mutate(player = case_when(is.na(player_pass) == F ~ player_pass,
                            is.na(player_rush) == F ~ player_rush,
                            TRUE ~ player),
         team = case_when(is.na(team_pass) == F ~ team_pass,
                          is.na(team_rush) == F ~ team_rush,
                          TRUE ~ team),
         conference = case_when(is.na(conference_pass) == F ~ conference_pass,
                                is.na(conference_rush) == F ~ conference_rush,
                                TRUE ~ conference)) %>% 
  select(-dplyr::contains("category"), -dplyr::contains(c("player_", "team_", "conference_")))

joined_stats <- joined_stats %>% left_join(teams.master, by=c("year", "team"))

# Select fields and convert columns to numerics
joined_stats <- joined_stats %>% select(playerId, player, team, conference, year, dplyr::everything()) %>%
  mutate(across(-(1:5), as.numeric)) %>% 
  replace_na(list(INT = 0, TD_pass = 0, YDS_pass = 0, COMPLETIONS = 0, YPA = 0, PCT = 0, ATT = 0,
                  LONG = 0, CAR = 0, TD_rush = 0, YPC = 0, YDS_rush = 0, 
                  LONG_receiving = 0, TD = 0, YDS = 0, YPR = 0, REC = 0)) %>% 
  mutate(total_TDs = TD_pass+TD_rush+TD,
         total_YDs = YDS_rush + YDS_pass + YDS)

heisman_stats <- joined_stats %>% 
  mutate(heisman_winner = case_when(year == 2004 & playerId == "120511" ~ 1,
                                    year == 2005 & playerId == "145158" ~ 1,
                                    year == 2006 & playerId == "133648" ~ 1,
                                    year == 2007 & playerId == "183484" ~ 1,
                                    year == 2008 & playerId == "188934" ~ 1,
                                    year == 2009 & playerId == "379061" ~ 1,
                                    year == 2010 & playerId == "232016" ~ 1,
                                    year == 2011 & playerId == "378497" ~ 1,
                                    year == 2012 & playerId == "517475" ~ 1,
                                    year == 2013 & playerId == "530308" ~ 1,
                                    year == 2014 & playerId == "511459" ~ 1,
                                    year == 2015 & playerId == "546368" ~ 1,
                                    year == 2016 & playerId == "3916387" ~ 1,
                                    year == 2017 & playerId == "550373" ~ 1,
                                    year == 2018 & playerId == "3917315" ~ 1,
                                    year == 2019 & playerId == "3915511" ~ 1,
                                    TRUE ~ 0
  ))


# Add other variables and game averages
heisman_stats <- heisman_stats %>%
  mutate(PowerFive = ifelse(conference %in% c("ACC", "Big 12", "Big Ten", "ACC", "Pac-10", "Pac-12","SEC"),1,0),
         QBFlag = ifelse(ATT > 100,1,0),
         RBFlag = ifelse(CAR > 100,1,0),
         WRFlag = ifelse(REC > 100,1,0),
         INTperGame = INT/total.games,
         YDS_rushperGame = YDS_rush/total.games,
         total_TDsperGame = total_TDs/total.games,
         YDSperGame = YDS/total.games,
         total_YDsperGame = total_YDs/total.games)

model <- glm(heisman_winner ~ INTperGame + YDS_rushperGame + total_TDsperGame + YDSperGame + total_YDsperGame + PowerFive + QBFlag + RBFlag + WRFlag + winPerc, data = heisman_stats, family = "binomial", na.action = na.pass)
# Other model options
# total_TDs + INT + YDS_rush
summary <- summary(model)
summary

# Split data
ind <- sample(2, nrow(heisman_stats), replace = TRUE, prob = c(0.6, 0.4))
#ep_train <- heisman_stats[ind == 1,] %>% ungroup()
#ep_test <- heisman_stats[ind == 2,] %>% ungroup()

ep_train <- heisman_stats %>% filter(year!=2020) %>% ungroup()
ep_test <- heisman_stats %>% filter(year==2020)%>% ungroup()


# Build Model
heisman_model <- glm(formula = heisman_winner ~ INTperGame + YDS_rushperGame + total_TDsperGame + YDSperGame + total_YDsperGame + PowerFive + QBFlag + RBFlag + WRFlag + winPerc, data = ep_train, family = "binomial", na.action = na.pass)

summary(heisman_model)

# Evaluate model

ep_test$heisman <- predict(heisman_model, newdata = ep_test, allow.new.levels = TRUE)
ep_test$heisman_prob <- exp(ep_test$heisman)/(1+exp(ep_test$heisman))

# Residuals
ep_test$resid <- ep_test$heisman_winner - ep_test$heisman_prob

mean(abs(ep_test$resid), na.rm = T)

ep_test %>% 
  ggplot(aes(x = as.factor(heisman_winner), y = heisman_prob)) +
  geom_point(alpha = 0.1)

ep_test %>% 
  ggplot(aes(x = YDS_pass)) +
  geom_point(aes(y = heisman_prob, size = as.factor(heisman_winner)), colour = "blue", alpha = 0.1)

# Apply to full data
heisman_final <- heisman_stats

heisman_final$heisman <- predict(heisman_model, newdata = heisman_final, allow.new.levels = TRUE)
heisman_final$heisman_prob <- exp(heisman_final$heisman)/(1+exp(heisman_final$heisman))

heisman_cumulative <- heisman_final %>% select(year, heisman_prob) %>% 
  group_by(year) %>% 
  summarise(totalprob = sum(heisman_prob))

heisman_final <- heisman_final %>% left_join(heisman_cumulative, by="year") %>% 
  mutate(heisman_prob = heisman_prob/totalprob)


heisman_final %>% 
  ggplot(aes(x = as.factor(heisman_winner), y = heisman_prob)) +
  geom_point(alpha = 0.1)

heisman_final %>% 
  ggplot(aes(x = total_TDs)) +
  geom_point(aes(y = heisman_prob, colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, y = heisman_prob, label = player), data = {heisman_final %>% filter(heisman_prob >.5)}) +
  scale_color_manual(values = c("red", "blue"))

# See top 5 in terms of heisman prob each year
heisman_final %>% group_by(year) %>% slice_max(heisman_prob, n = 5)
heisman_final %>% filter(year == 2020) %>% slice_max(heisman_prob, n = 5) %>% select(player, heisman_prob)

ep_test_cumulative <- sum(ep_test$heisman_prob)

ep_test %>% slice_max(heisman_prob, n = 5) %>% select(player, heisman_prob) %>% 
  mutate(heisman_prob = heisman_prob/ep_test_cumulative)

