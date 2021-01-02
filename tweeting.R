library(tidyverse)
library(rtweet)
library(tidyr)
library(tidytext)
library(lubridate)

app_name <- "test app name4"
consumer_key <- "lh1lzM04TOawNA3Soq9UjxiCq"
consumer_secret <- "8tKD25e7kt8qzsYRYFZHGu89O6r4Hq17fLH0fO8vgZexzXfcX3"
access_token <- "404429205-332mH0SVXxmDDKZcgx8SM2AINxPbg4Odj09cSrR6"
access_secret <- "bfHa6i58YXy4CadhbEAueySMdbUJJfjeeyJyFMYjq9Uty"

#AAAAAAAAAAAAAAAAAAAAAO53KwEAAAAAT20xPm4%2FOgCiCzrqo8MOcjXQUZ0%3DbjOIjjF2YjiPjtYJmG9EU0zcivB2OTErMKBXB9toVrxlaPdhGj

tok <- create_token(app=app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token,
             access_secret = access_secret)

create_

search_results <- search_tweets("Aqib Talib", n=5000, include_rts = FALSE,
                                token = tok)

search_results_refined <- search_results %>% 
  select(status_id, created_at, text) %>% 
  mutate(created_at = as_datetime(created_at)) %>% 
  filter(created_at > "2020-12-19")

sent <- search_results_refined %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(time_minute = round_time(created_at, n="hours", tz="EST"))


sent %>% 
  group_by(status_id, time_minute) %>% 
  summarise(mean_sentiment = mean(value)) %>% 
  group_by(time_minute) %>% 
  summarise(mean_sentiment = mean(mean_sentiment)) %>% 
  ggplot(aes(x=time_minute, y=mean_sentiment)) +geom_line()

mean(sent$value)  
  
sent2 <- sent %>% group_by(word) %>% add_count() %>% 
  select(word, value, n) %>% 
  unique()
  


post_tweet(status = "Testing from R @kylebeni012, Aqib Talib was great!",
           token = tok)
