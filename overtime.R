# Get CFB Data API for Play by Play - Use to generate expected points added (EPA)
rm(list=ls())
options(scipen=999)
library(data.table)
library(tidyverse)
library(RCurl)
library(XML)
library(rjson)
library(jsonlite)
library(stringr)
library(gt)
library(lubridate)

source("Production/source_everything.R")

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data

#get plays data and gmes data
#get plays data and gmes data
plays.master <- get_plays(start_week = 1, end_week = 15, start_year = 2020, end_year = 2021)
drives.master <- get_drives(start_week = 1, end_week = 15, start_year = 2020, end_year = 2021)

#keep only last rolling 15 games maximum
drives.master <- drives.master[,c(1:28)]


#find drives where there was a first down that started between the opponnet's 20 and 30 yard line
#what was the outcome of those drives

drives_with_first_down_in_redzone <- plays.master %>% filter(down==1,
                                                             yards_to_goal < 31,
                                                             yards_to_goal > 19)

drives_with_first_down_in_redzone2 <- drives_with_first_down_in_redzone %>% left_join(drives.master,
                                                by=c("game_id", "drive_id"="id"))

tds<- drives_with_first_down_in_redzone2 %>% 
  filter(drive_result == "TD") %>% 
  group_by(offense.x) %>% 
  count() %>% 
  mutate(metric = "TD")

fgs <- drives_with_first_down_in_redzone2 %>% 
  filter(drive_result == "FG") %>% 
  group_by(offense.x) %>% 
  count() %>% 
  mutate(metric = "FG")

nothing <- drives_with_first_down_in_redzone2 %>% 
  filter(drive_result != "FG", drive_result != "TD") %>% 
  group_by(offense.x) %>% 
  count() %>% 
  mutate(metric = "NOSCORE")

list_of_teams <- drives.master %>% 
  select(offense) %>% 
  distinct() %>% pull()

list_of_options <- c("TD", "FG", "NOSCORE")

master <- crossing(list_of_teams, list_of_options)

combined <- rbindlist(list(tds, fgs, nothing)) 

totals <- combined %>% group_by(offense.x) %>% 
  summarise(opportunities = sum(n))

combined <- combined %>% 
  left_join(totals) %>% 
  mutate(probability = n/opportunities)

lookup_table <- master %>% left_join(combined, by=c("list_of_teams"="offense.x",
                                                    "list_of_options"="metric")) %>% 
  replace_na(list(probability=0))

#upload lookup table to github

overtime_sim <- function(home_team, away_team, start_with_ball) {
  
  #track totals
  team_one_win <- 0
  team_two_win <- 0
  tie <- 0
  more_than_two <- 0
  
  #bring in team information
  home_team_td <- lookup_table %>% 
    filter(list_of_teams == home_team,
           list_of_options == "TD") %>% 
    select(probability) %>% pull()
  
  home_team_fg <- lookup_table %>% 
    filter(list_of_teams == home_team,
           list_of_options == "FG") %>% 
    select(probability) %>% pull()
  
  home_team_ns <- lookup_table %>% 
    filter(list_of_teams == home_team,
           list_of_options == "NOSCORE") %>% 
    select(probability) %>% pull()
  
  away_team_td <- lookup_table %>% 
    filter(list_of_teams == away_team,
           list_of_options == "TD") %>% 
    select(probability) %>% pull()
  
  away_team_fg <- lookup_table %>% 
    filter(list_of_teams == away_team,
           list_of_options == "FG") %>% 
    select(probability) %>% pull()
  
  away_team_ns <- lookup_table %>% 
    filter(list_of_teams == away_team,
           list_of_options == "NOSCORE") %>% 
    select(probability) %>% pull()
  
  for(trial in c(1:10000)) {
    #initialize variables for overtime
    team_one_score <- 0
    team_two_score <- 0
    
    if(start_with_ball == "Home Team") {
      rand_number <- runif(1)
      #home team scores a touchdown
      if(rand_number < home_team_td){
        team_one_score <- team_one_score + 6
        
        #home team scores XP
        rand_number2 <- runif(1)
        if(rand_number2 < .94){
          team_one_score <- team_one_score + 1
        } #end of away team kicking XP 
        
        #home team scored a touchdown, so away team will not kick an XP - increase TD
        #probability by .5*FG probability
        rand_number2 <- runif(1)
        if(rand_number2 < (away_team_td + .5*away_team_fg)) {
          team_two_score <- team_two_score + 6
          
          #away team XP
          rand_number2 <- runif(1)
          if(rand_number2 < .94){
            team_two_score <- team_two_score + 1
          }
        } #end of away team scoring a touchdown
      } #end of home team scoring a touchdown
      else if(rand_number < home_team_td + home_team_fg) {
        team_one_score <- team_one_score + 3
       
         rand_number2 <- runif(1)
         
         if(rand_number2 < away_team_td) {
           team_two_score <- team_two_score + 6
         } #end of away team scoring a TD
         else if (rand_number2 < away_team_td + away_team_fg){
           team_two_score <- team_two_score + 3
         } #end of away team scoring a FG
        
      }#end of home team scoring a FG
      else{
        
        rand_number2 <- runif(1)
        
        if(rand_number2 < away_team_td) {
          team_two_score <- team_two_score + 6
        } #end of away team scoring a TD
        else if (rand_number2 < away_team_td + away_team_fg){
          team_two_score <- team_two_score + 3
        } #end of away team scoring a FG
        
      } #end of home team not scoring
      
    if(team_one_score>team_two_score){
      team_one_win <- team_one_win + 1
      #message("Home team won",team_one_score, team_two_score)
      }
    else if(team_two_score>team_one_score){
      team_two_win <- team_two_win + 1
      #message("Away team won", team_one_score, team_two_score)
      }
    
    else { #need a second overtime
      tie <- tie + 1
      #message("Starting second overtime.")
      rand_number <- runif(1)
      #away team scores a touchdown
      if(rand_number < away_team_td){
        team_two_score <- team_two_score + 6
        
        rand_number2 <- runif(1)
        if(rand_number2 < .435){ #FILL IN TWO POINT PROBABILITY
          team_two_score <- team_two_score + 2
        } #end of away team going for 2
        
        #away team scored a touchdown, so home team will not kick an XP - increase TD
        #probability by .5*FG probability
        rand_number2 <- runif(1)
        if(rand_number2 < (home_team_td + .5*home_team_fg)) {
          team_one_score <- team_one_score + 6
          
          #away team 2-point
          rand_number2 <- runif(1)
          if(rand_number2 < .435){
            team_one_score <- team_one_score + 2
          }
        } #end of home team scoring a touchdown
      } #end of away team scoring a TD
      
      else if(rand_number < away_team_td + away_team_fg) {
        team_two_score <- team_two_score + 3
        
        rand_number2 <- runif(1)
        
        if(rand_number2 < home_team_td) {
          team_one_score <- team_one_score + 6
        } #end of home team scoring a TD
        else if (rand_number2 < home_team_td + home_team_fg){
          team_one_score <- team_one_score + 3
        } #end of home team scoring a FG
        
      }#end of away team scoring a FG
      else{
        
        rand_number2 <- runif(1)
        
        if(rand_number2 < home_team_td) {
          team_one_score <- team_one_score + 6
        } #end of home team scoring a TD
        else if (rand_number2 < home_team_td + home_team_fg){
          team_one_score <- team_one_score + 3
        } #end of home team scoring a FG
        
      } #end of away team not scoring
    
    if(team_one_score>team_two_score){team_one_win <- team_one_win + 1}
    else if(team_two_score>team_one_score){team_two_win <- team_two_win +1}
    else{
      #message("Still tied after two overtimes", team_one_score, team_two_score)
      team_one_win <- team_one_win+.5
      team_two_win <- team_two_win+.5
      more_than_two <- more_than_two + 1}
      
    } #end of second overtime
      
    } #end of home team starting with the ball
    
  #away team starts with ball  
  else{
    
    rand_number <- runif(1)
    #away team scores a touchdown
    if(rand_number < away_team_td){
      team_two_score <- team_two_score + 6
      
      #away team scores XP
      rand_number2 <- runif(1)
      if(rand_number2 < .94){
        team_two_score <- team_two_score + 1
      } #end of away team kicking XP 
      
      #away team scored a touchdown, so home team will not kick an XP - increase TD
      #probability by .5*FG probability
      rand_number2 <- runif(1)
      if(rand_number2 < (home_team_td + .5*home_team_fg)) {
        team_one_score <- team_one_score + 6
        
        #home team XP
        rand_number2 <- runif(1)
        if(rand_number2 < .94){
          team_one_score <- team_one_score + 1
        }
      } #end of home team scoring a touchdown
    } #end of away team scoring a touchdown
    else if(rand_number < away_team_td + away_team_fg) {
      team_two_score <- team_two_score + 3
      
      rand_number2 <- runif(1)
      
      if(rand_number2 < home_team_td) {
        team_one_score <- team_one_score + 6
      } #end of home team scoring a TD
      else if (rand_number2 < home_team_td + home_team_fg){
        team_one_score <- team_one_score + 3
      } #end of home team scoring a FG
      
    }#end of away team scoring a FG
    else{
      
      rand_number2 <- runif(1)
      
      if(rand_number2 < home_team_td) {
        team_one_score <- team_one_score + 6
      } #end of home team scoring a TD
      else if (rand_number2 < home_team_td + home_team_fg){
        team_one_score <- team_one_score + 3
      } #end of home team scoring a FG
      
    } #end of away team not scoring
    
    if(team_one_score>team_two_score){
      team_one_win <- team_one_win + 1
      #message("Home team won",team_one_score, team_two_score)
      }
    else if(team_two_score>team_one_score){
      team_two_win <- team_two_win + 1
      #message("Away team won", team_one_score, team_two_score)
      }
    
    else { #need a second overtime
      tie <- tie + 1
      #message("Starting second overtime.")
      rand_number <- runif(1)
      #home team scores a touchdown
      if(rand_number < home_team_td){
        team_one_score <- team_one_score + 6
        
        rand_number2 <- runif(1)
        if(rand_number2 < .435){ #FILL IN TWO POINT PROBABILITY
          team_one_score <- team_one_score + 2
        } #end of home team going for 2
        
        #home team scored a touchdown, so away team will not kick an XP - increase TD
        #probability by .5*FG probability
        rand_number2 <- runif(1)
        if(rand_number2 < (away_team_td + .5*away_team_fg)) {
          team_two_score <- team_two_score + 6
          
          #away team 2-point
          rand_number2 <- runif(1)
          if(rand_number2 < .435){
            team_two_score <- team_two_score + 2
          }
        } #end of away team scoring a touchdown
      } #end of home team scoring a TD
      
      else if(rand_number < home_team_td + home_team_fg) {
        team_one_score <- team_one_score + 3
        
        rand_number2 <- runif(1)
        
        if(rand_number2 < away_team_td) {
          team_two_score <- team_two_score + 6
        } #end of away team scoring a TD
        else if (rand_number2 < away_team_td + away_team_fg){
          team_two_score <- team_two_score + 3
        } #end of away team scoring a FG
        
      }#end of home team scoring a FG
      else{
        
        rand_number2 <- runif(1)
        
        if(rand_number2 < away_team_td) {
          team_two_score <- team_two_score + 6
        } #end of away team scoring a TD
        else if (rand_number2 < away_team_td + away_team_fg){
          team_two_score <- team_two_score + 3
        } #end of away team scoring a FG
        
      } #end of home team not scoring
      
      if(team_one_score>team_two_score){team_one_win <- team_one_win + 1}
      else if(team_two_score>team_one_score){team_two_win <- team_two_win +1}
      else{
        #message("Still tied after two overtimes", team_one_score, team_two_score)
        team_one_win <- team_one_win+.5
        team_two_win <- team_two_win+.5
        more_than_two <- more_than_two + 1}
      
    } #end of second overtime
    
  } #end of away team starting with the ball
      
  } #end of all overtimes
  
  print(paste0(team_one_win/10000, "-",
          team_two_win/10000,"-",
          tie/10000,"-",
          more_than_two/10000))
} #end of function

overtime_sim(home_team = "Alabama", away_team = "Alabama", start_with_ball = "Away Team")

