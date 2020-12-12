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

passing_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=both&category=passing&year="
rushing_url <- "https://api.collegefootballdata.com/stats/player/season?seasonType=both&category=rushing&year="

passing.master <- tibble()
rushing.master <- tibble()
for (yr in 2000:2020){
  message("Running Year ", yr)
  full_pass_url <- paste0(passing_url, as.character(yr))
  full_rush_url <- paste0(rushing_url, as.character(yr))
  passing <- fromJSON(full_pass_url) %>% tibble()
  passing <- passing %>% mutate(year = yr)
  rushing <- fromJSON(full_rush_url) %>% tibble()
  rushing <- rushing %>% mutate(year = yr)
  passing.master <- rbind(passing.master, passing)
  rushing.master <- rbind(rushing.master, rushing)
}

passing_spread <- passing.master %>% pivot_wider(names_from = c("statType"), values_from = c("stat"))
rushing_spread <- rushing.master %>% pivot_wider(names_from = c("statType"), values_from = c("stat"))

joined_stats <- passing_spread %>% left_join(rushing_spread, by = c("playerId", "year"), suffix = c("_pass", "_rush"))
joined_stats <- joined_stats %>% select(playerId, year, dplyr::ends_with("pass"), INT, COMPLETIONS, YPA, PCT, ATT, LONG, CAR, TD_rush, YPC, YDS_rush) %>%
  mutate(across(c("INT", "TD_pass", "COMPLETIONS", "YPA", "YDS_pass", "PCT", "ATT", # Change data types to numeric where applicable using across()
                  "LONG", "CAR", "TD_rush", "YPC", "YDS_rush"), as.numeric)) %>% 
  mutate(total_TDs = TD_pass+TD_rush)

heisman_stats <- joined_stats %>% 
  mutate(heisman_winner = case_when(year == 2004 & playerId == "120511" ~ 1,
                   year == 2006 & playerId == "133648" ~ 1,
                   year == 2007 & playerId == "183484" ~ 1,
                   year == 2008 & playerId == "188934" ~ 1,
                   year == 2010 & playerId == "232016" ~ 1,
                   year == 2011 & playerId == "378497" ~ 1,
                   year == 2012 & playerId == "517475" ~ 1,
                   year == 2013 & playerId == "530308" ~ 1,
                   year == 2014 & playerId == "511459" ~ 1,
                   year == 2016 & playerId == "3916387" ~ 1,
                   year == 2017 & playerId == "550373" ~ 1,
                   year == 2018 & playerId == "3917315" ~ 1,
                   year == 2019 & playerId == "3915511" ~ 1,
                   TRUE ~ 0
                   ))

model <- glm(heisman_winner ~ total_TDs + INT, data = heisman_stats, family = "binomial")
summary <- summary(model)
summary
