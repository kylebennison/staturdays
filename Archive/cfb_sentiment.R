library(tidyverse)
library(rtweet)
library(tidyr)
library(tidytext)
library(lubridate)
library(httpuv)
library(zoo)


dt <- read.csv("C:/Users/drewb/Desktop/completed.csv") %>% 
  mutate(created_at = as_datetime(created_at)) %>% 
  unique() %>% 
  filter(category != "#CFBPlayoff")

sent <- dt %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(time_minute = round_time(created_at, n="5 minutes", tz="EST"))


st <- as.POSIXct(strptime("2022-01-10 20:17:00", "%Y-%m-%d %H:%M:%S"))
q1 <- as.POSIXct(strptime("2022-01-10 21:00:00", "%Y-%m-%d %H:%M:%S"))
q2 <- as.POSIXct(strptime("2022-01-10 21:49:00", "%Y-%m-%d %H:%M:%S"))
q3 <- as.POSIXct(strptime("2022-01-10 22:13:00", "%Y-%m-%d %H:%M:%S"))
q3e <- as.POSIXct(strptime("2022-01-10 22:51:00", "%Y-%m-%d %H:%M:%S"))
q4 <- as.POSIXct(strptime("2022-01-10 23:58:00", "%Y-%m-%d %H:%M:%S"))

rand <- as.POSIXct(strptime("2022-01-10 20:00:00", "%Y-%m-%d %H:%M:%S"))

sent %>% 
  group_by(category, time_minute) %>% 
  summarise(mean_sentiment = mean(value)) %>% 
  filter(time_minute >= "2022-01-10 18:30:00") %>% 
  ggplot(aes(x=time_minute, y=mean_sentiment, color=category)) +
  geom_line() +
  #geom_line(aes(y=rollmean(mean_sentiment, 2, na.pad=TRUE))) +
  theme_bw() +
  labs(x="Time",
       y="<-- More Negative -- More Positive -->",
       color="Hashtag",
       title = "Sentiment of CFB National Championship tweets") +
  ylim(-4, 4) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=st, colour="gray") +
  geom_text(aes(x=st, label="Game Start", y=-3.5), colour="blue", angle=90, vjust = 0, text=element_text(size=8)) +
  geom_vline(xintercept=q1, colour="gray") +
  geom_text(aes(x=q1, label="End of Q1", y=-3.5), colour="blue", angle=90, vjust = 0, text=element_text(size=8)) +
  geom_vline(xintercept=q2, colour="gray") +
  geom_text(aes(x=q2, label="End of Q2", y=-3.5), colour="blue", angle=90, vjust = 0, text=element_text(size=8)) +
  geom_vline(xintercept=q3, colour="gray") +
  geom_text(aes(x=q3, label="Start of Q3", y=-3.5), colour="blue", angle=90, vjust = 0, text=element_text(size=8)) +
  geom_vline(xintercept=q3e, colour="gray") +
  geom_text(aes(x=q3e, label="End of Q3", y=-3.5), colour="blue", angle=90, vjust = 0, text=element_text(size=8)) +
  geom_vline(xintercept=q4, colour="gray") +
  geom_text(aes(x=q4, label="End of Q4", y=-3.5), colour="blue", angle=90, vjust = 0, text=element_text(size=8)) +
  scale_color_manual(values = c("#000000", "#046A38","#9E1B32"))
  #annotate("text", x=c, y=0, label= "boat")

#8:02 corso headgear
ggsave("C:/Users/drewb/Desktop/sentiment.png", height = 6, width = 9)
  
