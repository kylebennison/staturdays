library(nflreadr)
library(dplyr)
library(tidyverse)

dt <- NULL
for(i in c(2006:2022)){
  dt2 <- load_schedules(i)
  dt <- rbind(dt, dt2)
}

dt$alphatical <- ifelse(dt$away_team < dt$home_team, 1, 0)
dt$matchup <- ifelse(dt$alphatical == 1, paste0(dt$away_team, "-", dt$home_team),
                     paste0(dt$home_team, "-", dt$away_team))
dt <- dt %>% filter(result !=0)
dt$winner <- ifelse((dt$result > 0 & dt$result != 0), dt$home_team,
                    dt$away_team)
dt$alphatical_team <- ifelse(dt$away_team < dt$home_team, dt$away_team, dt$home_team)
dt$alphatical_win <- ifelse(dt$alphatical_team==dt$winner, 1, 0)
num_matchups <- dt %>% group_by(season, matchup) %>%  count()
num_matchups <- num_matchups %>% filter(n>=2)

dt3 <- dt %>% inner_join(num_matchups, by=c("season", "matchup"))

dt4 <- NULL
for(i in c(1: nrow(num_matchups))){
  yr <- num_matchups[i,1] %>% pull()
  match <- num_matchups[i,2] %>% pull()
  sample <- dt3 %>% filter(season==yr &
                             matchup==match)
  sample <- sample %>% arrange(week)
  sample <- sample %>% mutate(same_winner = (ifelse(winner==lag(winner), 1,0)))
  sample$same_winner <- sample$same_winner  %>% replace_na(1)
  sample <- sample %>% mutate(win_streak = cumsum(same_winner))
  
  if(sample[1,51] %>% pull() == 3){
    sample$three_wins <- ifelse(sample[3,53]==3, 1, 0)
  } else{
    sample$three_wins <- 0
  }
  
  sample$first_two <- ifelse(sample[2,53]==2, 1, 0)
  sample$game_number <- rank(sample$week)
  sample$streak_team <- sample[1,48]
    
  
  dt4 <- rbind(dt4, sample)
  
  # Calculate first two game's winners expected win prob
  # for third game
}

valid_cases <- dt4 %>% filter(first_two==1 & game_number==3)
valid_cases$streak_team_third_odds <- ifelse(valid_cases$streak_team == valid_cases$home_team,
                                             valid_cases$home_moneyline, valid_cases$away_moneyline)
valid_cases$streak_team_third_line <- ifelse(valid_cases$streak_team == valid_cases$home_team,
                                             valid_cases$spread_line, valid_cases$spread_line*-1)
valid_cases$streak_team_result <- ifelse(valid_cases$streak_team == valid_cases$home_team,
                                         valid_cases$result, valid_cases$result*-1)
valid_cases$streak_team_cover <- ifelse(valid_cases$streak_team_third_line<valid_cases$streak_team_result,
                                        1,0)
valid_cases$streak_team_third_win_prob <- ifelse(valid_cases$streak_team_third_odds < 0,
                                                 (valid_cases$streak_team_third_odds*-1/(valid_cases$streak_team_third_odds*-1+100)), (100/(valid_cases$streak_team_third_odds+100)))
valid_cases$streak_team_third_win_prob <- valid_cases$streak_team_third_win_prob-.02
#8 Games
valid_cases %>% 
  select(matchup, first_two, three_wins, season) %>% unique() %>% 
  summarise(winperc = mean(three_wins))

valid_cases %>% summarise(expected_wins = sum(streak_team_third_win_prob))
valid_cases %>% summarise(actual_wins = sum(three_wins))
valid_cases %>% summarize(cover_perc = mean(streak_team_cover))


x <- dt4 %>% filter(game_number==2)
x %>% summarise(second_game = mean(first_two))

x <- dt4 %>% filter(game_number==3)
x %>% summarise(three = mean(three_wins))


















valid_cases <- dt4 %>% filter(game_number==2)
valid_cases %>% summarise(x=mean(first_two))
