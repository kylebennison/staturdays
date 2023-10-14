library(tidyverse)
library(rtweet)
library(tidyr)
library(tidytext)
library(lubridate)
library(httpuv)
library(zoo)

#can make a request for 18,000 tweets in 15 minuetes
#https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

get_token()

#token <- create_token(
#  app=app_name,
#  consumer_key = consumer_k,
#  consumer_secret = consumer_s,
#  access_token = access_t,
#  access_secret = access_s)

x<-rate_limit(query = "search_tweets")


large_num <- 10000000000
i <- 1


completed <- tibble()


while(i < large_num) {

general_game <- search_tweets("#NationalChampionship", n=700, include_rts = FALSE)
alabama <- search_tweets("#RollTide", n=700, include_rts = FALSE)
georgia <- search_tweets("#GoDawgs", n=700, include_rts = FALSE)

general_game$category <- "#NationalChampionship"
general_game <- general_game %>% select(status_id, created_at, text, category) %>% 
  mutate(created_at = as_datetime(created_at),
         status_id=paste0("id", status_id)) 

alabama$category <- "#RollTide"
alabama <- alabama %>% select(status_id, created_at, text, category) %>% 
  mutate(created_at = as_datetime(created_at),
         status_id=paste0("id", status_id)) 

georgia$category <- "#GoDawgs"
georgia <- georgia %>% select(status_id, created_at, text, category) %>% 
  mutate(created_at = as_datetime(created_at),
         status_id=paste0("id", status_id)) 

master_dt1 <- rbind(general_game, alabama)
master_dt <- rbind(master_dt1, georgia)

completed <- rbind(completed, master_dt)

data.table::fwrite(completed, "C:/Users/drewb/Desktop/completed.csv")

i <- i + 1

message(paste0("Last data collected at: "), Sys.time())
Sys.sleep(150)

}
  

#in new script bring in data, clean, and do all steps below to graph

search_results_refined <- master_dt %>% 
  select(status_id, created_at, text, category) %>% 
  mutate(created_at = as_datetime(created_at)) %>% 
  filter(created_at > "2022-01-08")

sent <- search_results_refined %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(time_minute = round_time(created_at, n="5 minutes", tz="EST"))


sent %>% 
  group_by(status_id, time_minute) %>% 
  summarise(mean_sentiment = mean(value)) %>% 
  group_by(time_minute) %>% 
  summarise(mean_sentiment = mean(mean_sentiment)) %>% 
  ggplot(aes(x=time_minute, y=mean_sentiment)) +
  #geom_line() +
  geom_line(aes(y=rollmean(mean_sentiment, 6, na.pad=TRUE))) +
  theme_bw()

mean(sent$value)  
  
sent2 <- sent %>% group_by(word) %>% add_count() %>% 
  select(word, value, n) %>% 
  unique()
  


post_tweet(status = "Testing from R @kylebeni012, Aqib Talib was great!",
           token = tok)

## Ideas for tweeting matchup graphs

# Schedule script to run daily at 9:30 am

# Filter games data for games played today

# Create plots for those matchups

## Post tweets

# tweet intro
post_tweet(status = "Here are the EPA and Success-Rate edges for today's #CollegeFootball matchups")

# Wait just to make sure it has time to post before next step (not sure if this is necessary)
Sys.sleep(30)

# Post games

for (i in nrow(todays_games)){
  
  # Get status id of that tweet just sent
  status_id <- rtweet::get_my_timeline(n = 1, token = tok)
  reply_id <- status_id$status_id[1]
  
  # Create plot for game[i]
  
  # Reply to previous tweet
  post_tweet(status = paste0(game$home_team, " @ ", game$away_team, ". Kickoff at ", lubridate::tz(game$start_time), "."),
             in_reply_to_status_id = reply_id,
             media = "plot.png")
  
  Sys.sleep(60)
}