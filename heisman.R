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

# Required Themes and Data ------------------------------------------------

source("Production/source_everything.r")

# Read in all data --------------------------------------------------------

passing_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=regular&category=passing&year="
rushing_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=regular&category=rushing&year="
receiving_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=regular&category=receiving&year="

passing.master <- tibble()
rushing.master <- tibble()
receiving.master <- tibble()
for (yr in 2004:2021){
  message("Running Year ", yr)
  full_pass_url <- paste0(passing_url, as.character(yr))
  full_rush_url <- paste0(rushing_url, as.character(yr))
  full_receiving_url <- paste0(receiving_url, as.character(yr))
  passing <- cfbd_api(full_pass_url, my_key) %>% tibble()
  passing <- passing %>% mutate(year = yr)
  rushing <- cfbd_api(full_rush_url, my_key) %>% tibble()
  rushing <- rushing %>% mutate(year = yr)
  receiving <- cfbd_api(full_receiving_url, my_key) %>% tibble()
  receiving <- receiving %>% mutate(year = yr)
  passing.master <- rbind(passing.master, passing)
  rushing.master <- rbind(rushing.master, rushing)
  receiving.master <- rbind(receiving.master, receiving)
}

teams_url <- "https://api.collegefootballdata.com/records?year="
teams.master <- tibble()
for (yr in 2004:2021){
  message("Running Year ", yr)
  full_team_url <- paste0(teams_url, as.character(yr))
  teams <- cfbd_api(full_team_url, my_key) %>% tibble()
  # teams <- data.table::data.table(teams)
  teams <- teams %>% mutate(year = yr) %>% 
    select(year, team, total.games, total.wins) %>% 
    mutate(winPerc = total.wins/total.games)
  teams.master <- rbind(teams.master, teams, row.names=NULL)
}
# Not run
# ppa_url <- "https://api.collegefootballdata.com/ppa/players/season?threshold=100&year="
# usage_url <- "https://api.collegefootballdata.com/player/usage?year="
# 
# ppa.master <- tibble()
# usage.master <- tibble()
# for (yr in 2000:2020){
#   message("Running Year ", yr)
#   full_team_url <- paste0(ppa_url, as.character(yr))
#   teams <- fromJSON(full_team_url) %>% tibble()
#   teams <- data.table(teams)
#   if(nrow(teams)>=1){
#   teams <- teams %>% mutate(year = yr)
#   ppa.master <- rbind(ppa.master, teams, row.names=NULL, fill = TRUE)
#   }
# }
# 
# for (yr in 2000:2020){
#   message("Running Year ", yr)
#   full_team_url <- paste0(usage_url, as.character(yr))
#   teams <- fromJSON(full_team_url) %>% tibble()
#   teams <- data.table(teams)
#   if(nrow(teams)>=1){
#     teams <- teams %>% mutate(year = yr)
#     usage.master <- rbind(usage.master, teams, row.names=NULL, fill = TRUE)
#   }
# }

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

# ppa.select <- ppa.master %>% select(id, year, countablePlays, averagePPA.all, averagePPA.pass, averagePPA.rush, totalPPA.all, totalPPA.pass, totalPPA.rush)
# joined_stats <- joined_stats %>% left_join(ppa.select, by = c("playerId" = "id", "year"))
# 
# usage.select <- usage.master %>% select(id, year, usage.overall, usage.pass, usage.rush)
# joined_stats <- joined_stats %>% left_join(usage.select, by = c("playerId" = "id", "year"))

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
                                    year == 2020 & playerId == "4241478" ~ 1,
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
         total_YDsperGame = total_YDs/total.games) %>% 
  group_by(playerId, year) %>% 
  mutate(position = case_when(max(ATT, CAR, REC) == ATT ~ "QB",
                              max(ATT, CAR, REC) == CAR ~ "RB",
                              max(ATT, CAR, REC) == REC ~ "WR",
                              TRUE ~ "DUAL")) %>% 
  ungroup()

# Add z-score for each player above/below average for their position and year
heisman_stats <- heisman_stats %>% 
  group_by(year, position) %>% 
  mutate(total_TDs_z = (total_TDs-mean(total_TDs))/sd(total_TDs),
         rush_yds_z = (YDS_rush-mean(YDS_rush))/sd(YDS_rush),
         INT_z = (INT-mean(INT))/sd(INT)) %>% 
  filter(position != "DUAL")

model <- glm(heisman_winner ~ INTperGame + YDS_rushperGame + total_TDsperGame + YDSperGame + total_YDsperGame + PowerFive + QBFlag + RBFlag + WRFlag + winPerc, data = heisman_stats, family = "binomial", na.action = na.pass)
# Other model options
model_sig_only <- glm(heisman_winner ~ rush_yds_z + total_TDsperGame + winPerc, data = heisman_stats, family = "binomial", na.action = na.pass)
summary(model_sig_only)
# Extract p-values
pvals <- tibble(coef(summary(model_sig_only))[,4])
# total_TDs + INT + YDS_rush
summary <- summary(model)
summary

# Split data
ind <- sample(2, nrow(heisman_stats), replace = TRUE, prob = c(0.6, 0.4))
#ep_train <- heisman_stats[ind == 1,] %>% ungroup()
#ep_test <- heisman_stats[ind == 2,] %>% ungroup()

ep_train <- heisman_stats %>% filter(year!=2021) %>% ungroup()
ep_test <- heisman_stats %>% filter(year==2021)%>% ungroup()


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

# Plot this year's projected winners
ep_test %>%
  ggplot(aes(x = total_TDsperGame, y = heisman_prob)) +
  geom_point(colour = staturdays_colors("orange"), alpha = 0.1,
             size = 2) +
  ggrepel::geom_text_repel(aes(label = if_else(
    heisman_prob > .25, paste0(player, " - ", position), ""
  ), size = heisman_prob),
  color = staturdays_colors("dark_blue")) +
  staturdays_theme +
  labs(x = "TDs per Game",
       y = "Heisman Odds",
       title = "2021 Heisman Watch",
       subtitle = "Independent Heisman probabilities through five weeks") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none")

ggsave(filename = paste0("heisman_", lubridate::today(), ".jpg"),
       plot = last_plot(),
       path = "R Plots/",
       width = 400,
       height = 200,
       dpi = 300,
       units = "mm")

library(pROC)
heis_roc <- roc(ep_test$heisman_winner, predict(heisman_model, newdata = ep_test))
heis_auc <- toString(heis_roc$auc)

# plot of AUC
ggwin_roc <- ggroc(heis_roc)

ggwin_roc +
  geom_text(mapping = aes(x = 0.5, y = 0.5, label = paste0('AUC of ', round(as.double(heis_auc), 5))))

# Apply to full data
heisman_final <- heisman_stats

heisman_final$heisman <- predict(heisman_model, newdata = heisman_final, allow.new.levels = TRUE)
heisman_final$heisman_prob <- exp(heisman_final$heisman)/(1+exp(heisman_final$heisman))

heisman_cumulative <- heisman_final %>% select(year, heisman_prob) %>% 
  group_by(year) %>% 
  summarise(totalprob = sum(heisman_prob))

heisman_final <- heisman_final %>% left_join(heisman_cumulative, by="year") %>% 
  mutate(heisman_prob = heisman_prob/totalprob)

# Average Prob for Heisman Winners vs. Non-Winners
heisman_final %>% 
  group_by(heisman_winner) %>% 
  summarise(mean_prob = mean(heisman_prob)) %>% 
  ggplot(aes(x = as.factor(heisman_winner), y = mean_prob)) +
  geom_col(alpha = 0.5, fill = staturdays_colors("orange")) +
  staturdays_theme

# Only for top 5 per year
heisman_final %>% 
  group_by(year) %>% 
  slice_max(heisman_prob, n = 5) %>% 
  group_by(heisman_winner) %>% 
  summarise(mean_prob = mean(heisman_prob)) %>% 
  ggplot(aes(x = as.factor(heisman_winner), y = mean_prob)) +
  geom_col(alpha = 0.5, fill = staturdays_colors("orange")) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_discrete("Heisman Winner", labels = c("0" = "No","1" = "Yes")) +
  staturdays_theme +
  labs(title = "Heisman Win Probabilities",
       subtitle = "Average Probability of Top 5 Contenders Each Season",
       y = "Heisman Probability")

ggsave(filename = "heisman_top_5_winner_vs_non_winner.png", 
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = last_plot(),
       width = 200,
       height = 200,
       units = "mm",
       dpi = 300)

heisman_final %>% 
  ggplot(aes(x = as.factor(heisman_winner), y = heisman_prob)) +
  geom_point(alpha = 0.5, color = staturdays_colors("orange")) +
  staturdays_theme

# Total TDs -> Heisman Prob
heisman_final %>% 
  ggplot(aes(x = total_TDs)) +
  geom_point(aes(y = heisman_prob, colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, y = heisman_prob, label = player), data = {heisman_final %>% filter(heisman_winner == 1)}) +
  scale_color_manual(values = c("red", "blue")) +
  staturdays_theme +
  labs(title = "Total TDs (Regular Season)",
       subtitle = "Total TDs are the most predictive stat",
       x = "Total TDs (Regular Season)",
       y = "Heisman Probability") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  theme(legend.position = "none")


ggsave(filename = "total_tds_heisman_prob_v2.png", 
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = last_plot(),
       width = 200,
       height = 200,
       units = "mm",
       dpi = 300)

# Only this year's frontrunners
ep_test %>% 
  ggplot(aes(x = total_TDs)) +
  geom_point(aes(y = heisman_prob, colour = if_else(total_TDs >= 40, "blue", "red")), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, 
                               y = heisman_prob, 
                               label = player), 
                           data = {ep_test %>% 
                               filter(total_TDs >= 40 & year == 2021)},
                           force = 1,
                           min.segment.length = .25) +
  staturdays_theme +
  scale_color_identity() +
  labs(title = "Total TDs (Regular Season)",
       subtitle = "Total TDs are the most predictive stat",
       x = "Total TDs (Regular Season)",
       y = "Heisman Probability") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  theme(legend.position = "none")

ggsave(filename = paste0(today(), "_total_tds_heisman_prob_v2.png"), 
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = last_plot(),
       width = 400,
       height = 200,
       units = "mm",
       dpi = 300)

# Coulda/shoulda won
heisman_final %>% 
  ggplot(aes(x = total_TDs)) +
  geom_point(aes(y = heisman_prob, colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, y = heisman_prob, label = player), data = {heisman_final %>% filter(heisman_prob > .3 & heisman_winner == 0)}) +
  scale_color_manual(values = c("red", "blue")) +
  staturdays_theme +
  labs(title = "Close, but no cigar",
       subtitle = "Heisman coulda/shoulda wons",
       x = "Total TDs (Regular Season)",
       y = "Heisman Probability") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  theme(legend.position = "none")

# Highest prob by year
heisman_final %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = heisman_prob, colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = year, y = heisman_prob, label = player), data = {heisman_final %>% group_by(year) %>% slice_max(heisman_prob, n = 1)}) +
  scale_color_manual(values = c("red", "blue")) +
  staturdays_theme +
  labs(title = "Top Heisman Probability by Year",
       subtitle = "Top heisman probability ",
       x = "Total TDs (Regular Season)",
       y = "Heisman Probability") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  theme(legend.position = "none")

ggsave(filename = "heisman_top_prob_by_yr.png", 
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = last_plot(),
       width = 200,
       height = 200,
       units = "mm",
       dpi = 300)

# Just 2020 Top 10 Odds - Total TDs/Game -> Heisman Prob
heisman_final %>% filter(year == 2021) %>% ungroup() %>% slice_max(heisman_prob, n = 10) %>% 
  ggplot(aes(x = total_TDsperGame)) +
  geom_point(aes(y = heisman_prob, size = rush_yds_z, fill = winPerc), shape = 21, alpha = 0.9) +
  ggrepel::geom_text_repel(aes(y = heisman_prob, label = player), data = {heisman_final %>% ungroup() %>% filter(year == 2021) %>% slice_max(heisman_prob, n = 10)}) +
  staturdays_theme +
  labs(title = "2021 Heisman Frontrunners",
       subtitle = "Looking at the most predictive stats",
       x = "Total TDs Per Game",
       y = "Heisman Probability",
       size = "Rush Yard Z-Score",
       fill = "Win %") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_gradient(label = percent_format(accuracy = 1), high = staturdays_colors("orange"), low = staturdays_colors("lightest_blue"))

ggsave(filename = paste0("2021_heisman_frontrunners", str_replace_all(now(), ":", "."), ".png"), 
       path = "C:/Users/Kyle/Documents/Kyle/Staturdays/R Plots",
       plot = last_plot(),
       width = 200,
       height = 200,
       units = "mm",
       dpi = 300)

# Total TDs/Game -> Heisman Prob
heisman_final %>% 
  ggplot(aes(x = total_TDsperGame)) +
  geom_point(aes(y = heisman_prob, colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDsperGame, y = heisman_prob, label = player), data = {heisman_final %>% filter(heisman_prob >.5)}) +
  scale_color_manual(values = c("red", "blue")) + 
  labs(title = "Total TDs Per Game")

# Rush Yards Z-Score -> Heisman Prob
heisman_final %>% 
  filter(position == "RB") %>% 
  ggplot(aes(x = rush_yds_z)) +
  geom_point(aes(y = heisman_prob, colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = rush_yds_z, y = heisman_prob, label = player), data = {heisman_final %>% filter(heisman_prob >.15, position == "RB")}) +
  scale_color_manual(values = c("red", "blue")) + 
  labs(title = "Rush Yards Z-Score",
       subtitle = "For Season at Position\nRBs Only")

# 
heisman_final %>% 
  ggplot(aes(x = total_TDs, y = YDS_rush)) +
  geom_point(aes(colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, label = player), data = {heisman_final %>% filter(heisman_prob >.5)}) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Total TDs vs. Rush Yards")

heisman_final %>% 
  filter(position != "WR") %>% 
  ggplot(aes(x = total_TDs, y = rush_yds_z)) +
  geom_point(aes(colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, label = player), data = {heisman_final %>% filter(heisman_prob >.5)}) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Total TDs vs. Rush Yds. Z-Score")

heisman_final %>% 
  ggplot(aes(x = total_TDs, y = winPerc)) +
  geom_point(aes(colour = as.factor(heisman_winner)), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, label = player), data = {heisman_final %>% filter(heisman_winner == 1)}) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "Total TDs vs. Win Perc")

heisman_final %>% 
  ggplot(aes(x = total_TDs, y = INT)) +
  geom_point(aes(colour = if_else(year == 2021 & heisman_prob > .1 & position == "QB", "blue", "red")), alpha = 0.5) +
  ggrepel::geom_text_repel(aes(x = total_TDs, label = player), data = {heisman_final %>% filter(year == 2021 & heisman_prob > .1 & position == "QB")}) +
  scale_color_identity() +
  labs(title = "Total TDs vs. INTs")

# See top 5 in terms of heisman prob each year
heisman_final %>% group_by(year) %>% slice_max(heisman_prob, n = 5)
heisman_final %>% filter(year == 2021) %>% ungroup() %>%  slice_max(heisman_prob, n = 5) %>% select(player, heisman_prob, total_TDs, total_TDsperGame, YDS_rushperGame, rush_yds_z, winPerc)

ep_test_cumulative <- sum(ep_test$heisman_prob)

ep_test %>% slice_max(heisman_prob, n = 5) %>% select(player, heisman_prob) %>% 
  mutate(heisman_prob = heisman_prob/ep_test_cumulative)

# Percent of favorites from model that actually won
heisman_final %>% group_by(year) %>% slice_max(heisman_prob, n = 1) %>% ungroup() %>% summarise(sum_win = sum(heisman_winner), count = n())
heisman_final %>% group_by(year) %>% slice_max(heisman_prob, n = 5) %>% ungroup() %>% summarise(sum_win = sum(heisman_winner), count = n())
