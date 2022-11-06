source("Production/source_everything.r")

plays <- get_plays(start_week = 1, 20, 2021, 2021)

colors <- get_colors()

library(ggtext)

prepped <- plays %>% 
  mutate(red_zone = if_else(yards_to_goal <= 20, 1, 0)) %>% 
  group_by(offense, offense_conference, red_zone) %>% 
  summarise(avg_ppa = mean(as.numeric(ppa), na.rm = TRUE),
            n = n()) %>% 
  group_by(offense) %>% 
  mutate(net_dif = avg_ppa - lag(avg_ppa, 1L, order_by = red_zone)) %>% 
  ungroup() %>% 
  filter(n >= 50, red_zone == 1)

for(i in 1:length(power_5)){
  
prepped %>% 
  filter(offense_conference == power_5[i]) %>% 
  left_join(colors, by = c("offense" = "school")) %>% 
  ggplot(aes(x = reorder(offense, net_dif), y = net_dif)) +
  geom_col(aes(fill = color), alpha = .5) +
  ggimage::geom_image(aes(image = light), 
                      size = .05, 
                      by = "width", 
                      asp = 2, 
                      nudge_y = .01) +
  theme(aspect.ratio = 1/2) +
  scale_fill_identity() +
  scale_x_discrete(guide = "none") +
  staturdays_theme +
  theme(axis.title = element_blank(),
        plot.title = element_markdown()) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(labels = scales::number_format(accuracy = .001)) +
  labs(title = "Net PPA Improvement in the <span style='color: red;'>Red Zone</span>",
       subtitle = paste0("Through 2021 Week 6", " ", power_5[i]),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data")

ggsave(filename = paste0("red_zone_improvement_",
                         power_5[i],
                         ".jpg"),
       plot = last_plot(),
       path = "R Plots/",
       height = 200, width = 400, units = "mm",
       dpi = 300)
}
