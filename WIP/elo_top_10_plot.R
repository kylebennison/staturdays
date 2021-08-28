source("Production/source_everything.r")

elo <- fread("Production/elo_ratings_historic.csv")


elo %>% 
  filter(team %in% {elo %>% filter(season==2021) %>% slice_max(order_by = elo_rating, 
                                      n = 10) %>% pull(team)}) %>% 
  filter(season == 2020) %>% 
  group_by(team) %>% 
  mutate(plot_name = if_else(week == 5, 1, 0)) %>% 
  ggplot(aes(x = week, y = elo_rating, color = team)) +
  geom_line() +
  scale_color_viridis_d() +
  staturdays_theme +
  geom_text(aes(label = if_else(plot_name == 1, team, "")),
            position = position_jitter(width = 1,
                                       height = 1),
            fontface = "bold") +
  labs(x = "Week", y = "Elo Rating", color = "Team")

ggsave(plot = last_plot(),
       filename = paste0(today(), "_",
                         "top_10_",
                         "elo_weekly",
                         ".jpeg"),
       height = 400,
       width = 200,
       units = "mm",
       dpi = 300,
       path = "R Plots/")
