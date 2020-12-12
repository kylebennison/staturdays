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
