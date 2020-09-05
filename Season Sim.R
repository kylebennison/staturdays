library(data.table)
library(lubridate)
library(jsonlite)
library(RCurl)
library(tidyverse)
options(scipen=999)

elo_ratings <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/elo_ratings_historic.csv")
cfb_schedule_url <- "https://api.collegefootballdata.com/games?year=2020&seasonType=regular"

#get the current Elo ratings for all teams
elo_ratings$date <- ymd(elo_ratings$date)
current_elo_ratings <- elo_ratings[elo_ratings[, .I[which.max(date)], by = team]$V1]
current_elo_ratings <- current_elo_ratings[,c(1,3,6)]

#bring in 2020 schedule
schedule <- fromJSON(getURL(cfb_schedule_url))

schedule$start_date <- as_date(schedule$start_date)

already_played <- schedule[!is.na(schedule$home_points),]
already_played$home_result <- ifelse(already_played$home_points > already_played$away_points, 1, 0)
#make alread6y_played length of sims and add szn
already_played_new <- data.table()
for(szn in c(1:3000)){
  already_played$szn <- szn
  already_played_new <- rbind(already_played_new, already_played)
}


to_be_played <- schedule[is.na(schedule$home_points),]

to_be_played <- merge(to_be_played, current_elo_ratings, by.x = "home_team", by.y = "team", all.x = TRUE)
setnames(to_be_played, "elo_rating", "elo_rating_home")
to_be_played <- merge(to_be_played, current_elo_ratings, by.x = "away_team", by.y = "team", all.x = TRUE)
setnames(to_be_played, "elo_rating", "elo_rating_away")

#fill NA to 1200
to_be_played$elo_rating_home[is.na(to_be_played$elo_rating_home)] <- 1200
to_be_played$elo_rating_away[is.na(to_be_played$elo_rating_away)] <- 1200

#add in home field advantage
to_be_played$elo_home_win_prob <- 1/(1+ (10^ ( (to_be_played$elo_rating_away - to_be_played$elo_rating_home + 55)/400)))

#order for sim
to_be_played <- to_be_played[order(to_be_played$start_date),]

results <- data.table()
for(szn in c(1:3000)){
  season_elo_ratings <- current_elo_ratings
  season_results <- data.table()
  message("Simulating Season ", szn, "/3000")
  
  for(wk in c(min(to_be_played$week):max(to_be_played$week))){
    to_be_played_week <- to_be_played[to_be_played$week == wk,]
    to_be_played_week$rand_draw <- runif(nrow(to_be_played_week), 0, 1)
    to_be_played_week$home_result <- ifelse(to_be_played_week$rand_draw < to_be_played_week$elo_home_win_prob, 1, 0)
    to_be_played_week$elo_rating_home_new <- to_be_played_week$elo_rating_home + 85*(to_be_played_week$home_result - to_be_played_week$elo_home_win_prob)
    to_be_played_week$elo_rating_away_new <- to_be_played_week$elo_rating_away + 85*( (1-to_be_played_week$home_result) - (1-to_be_played_week$elo_home_win_prob))
    new_elo <- to_be_played_week[,c("home_team", "away_team", "elo_rating_home_new", "elo_rating_away_new", "start_date")]
    new_home_elo <- new_elo[,c("home_team", "elo_rating_home_new", "start_date")]
    setnames(new_home_elo, c("home_team", "elo_rating_home_new", "start_date"), c("team", "elo_rating", "date"))
    new_away_elo <- new_elo[,c("away_team", "elo_rating_away_new", "start_date")]
    setnames(new_away_elo, c("away_team", "elo_rating_away_new", "start_date"), c("team", "elo_rating", "date"))
    new_elo <- rbind(new_home_elo, new_away_elo)
    season_elo_ratings <- rbind(season_elo_ratings, new_elo)
    season_elo_ratings <- season_elo_ratings[season_elo_ratings[, .I[which.max(date)], by = team]$V1]
    temp_tbp <- to_be_played_week
    temp_tbp$szn <- szn
    season_results <- rbind(season_results, temp_tbp)
  }
  results <- rbind(results, season_results)
}

#process simulated games
summary_results_home <- dcast(results, home_team+szn~home_result, value.var = "home_result")
summary_results_away <- dcast(results, away_team+szn~home_result, value.var = "home_result", )
setnames(summary_results_away, c("away_team","0", "1"), c("team","1", "0"))
setnames(summary_results_home, c("home_team"), c("team"))
combined_results <- rbind(summary_results_home, summary_results_away)

#process already completed games
summary_results_home_completed <- dcast(already_played_new, home_team+szn~home_result, value.var = "home_result")
summary_results_away_completed <- dcast(already_played_new, away_team+szn~home_result, value.var = "home_result")
setnames(summary_results_away_completed, c("away_team","0", "1"), c("team","1", "0"))
setnames(summary_results_home_completed, c("home_team"), c("team"))
combined_results2 <- rbind(summary_results_home_completed, summary_results_away_completed)
combined_results2[is.na(combined_results2$`0`),3] <- 0
combined_results2[is.na(combined_results2$`1`),4] <- 1
#new
combined_results <- rbind(combined_results, combined_results2)


#wins and losses per season
combined_results <- dcast(combined_results, team+szn~., value.var = c("0", "1"), fun.aggregate = sum)
combined_results$win_perc <- combined_results$`1`/(combined_results$`1`+combined_results$`0`)
combined_results$undefeated <- ifelse(combined_results$`0` == 0,1,0)

combined_results_3 <- combined_results[,list(wins = mean(`1`),
                                             losses = mean(`0`),
                                             games = mean(`1`)+mean(`0`), # total games on schedule
                                             win_perc = mean(win_perc),
                                             std_dev_wins = sd(`1`), # std dev of wins (same as losses)
                                             prob_undefeated = mean(undefeated)
                                             ),
                                       by='team']

# Adding a 95th percentile confidence interval for wins and losses, assuming normal distribution
combined_results_4 <- combined_results_3 %>% 
  mutate(upper_95_wins = if_else(wins + std_dev_wins*1.645 > games, games, wins + std_dev_wins*1.645),
         lower_95_wins = if_else(wins - std_dev_wins*1.645 < 0, 0, wins - std_dev_wins*1.645),
         upper_95_losses = if_else(losses + std_dev_wins*1.645 > games, games, losses + std_dev_wins*1.645),
         lower_95_losses = if_else(losses - std_dev_wins*1.645 < 0, 0, losses - std_dev_wins*1.645))
