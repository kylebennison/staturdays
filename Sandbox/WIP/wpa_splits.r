source("Production/source_everything.r")

pdata <- get_plays(1,12,2021,2021)
wpa <- add_wpa(pdata)

wpa %>% 
  group_by(offense, down) %>% 
  summarise(avg_wpa = mean(offense_wpa),
            n = n()) %>% 
  filter(down != 0, n > 30) %>% 
  arrange(desc(avg_wpa))

vars <- c("down", "period", "play_type")

for(i in 1:length(vars)){
wpa %>% 
  group_by(offense, offense_conference, vars[i]) %>% 
  summarise(avg_wpa = mean(offense_wpa),
            n = n()) %>% 
  filter(down == 4, n > 60, offense_conference %in% power_5) %>% 
  arrange(desc(avg_wpa)) %>% 
  ggplot(aes(x = reorder(offense, avg_wpa), y = avg_wpa)) +
  geom_col()
}
