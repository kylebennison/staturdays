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
library(ggrepel)

source("Production/source_everything.r")

# Read in 2014-2020 data
# plays.historic <- get_plays(1, 20, 2014, 2020)
# plays.historic <- plays.historic %>% 
#   mutate(across(c(id, game_id, drive_id), .fns = ~ paste0("id_", .x))) # append ids with "id_" to avoid them going into the csv as sci-notation
# data.table::fwrite(plays.historic, "Data/plays_2014_2020.csv")
plays.historic <- data.table::fread("Data/plays_2014_2020.csv", encoding = "UTF-8")

# Make ids characters
plays.master.temp <- plays.historic %>% 
  mutate(id = as.character(id), game_id = as.character(game_id), drive_id = as.character(drive_id))

plays.historic <- plays.master.temp
rm(plays.master.temp)

plays.master <- add_success(plays.historic)

# Checks to make sure data looks okay
# plays_temp_fast %>% filter(pass_rush == "Pass") %>% count(is.na(pass_player))
# 
# plays_temp_fast %>% filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(play_text, pass_player) %>% View()
# 
# plays_temp_fast %>% filter(pass_rush == "Pass" & !is.na(pass_player)) %>% select(pass_player) %>% unique() %>% View()



# Analysis ----------------------------------------------------------------

# before/After Interception -----------------------------------------------

plays.master <- plays.master %>% mutate(
  player_name = case_when(is.na(pass_player) == FALSE ~ pass_player,
                          is.na(pass_player) == TRUE ~ rush_player,
                          TRUE ~ NA_character_)
)

scrimmage_plays <- plays.master %>% 
  filter(play_type %in% scrimmage_plays_all) %>% 
  mutate(pass_attempt = if_else(pass_rush == "Pass" & play_type != "Sack", 1, 0),
         pass_completion = if_else(play_type %in% c("Pass Reception", "Pass Completion", "Passing Touchdown"), 1, 0),
         pass_touchdown = if_else(play_type == "Passing Touchdown", 1, 0),
         pass_intercepted = if_else(play_type %in% c("Pass Interception", "Pass Interception Return", "Interception Return Touchdown"), 1, 0),
         passer_sacked = if_else(play_type == "Sack", 1, 0),
         rush_touchdown = if_else(play_type == "Rushing Touchdown", 1, 0),
         rush_attempt = if_else(pass_rush == "Rush", 1, 0),
         pass_fumbled = if_else(pass_rush == "Pass" & 
                                  play_type %in% c("Fumble Recovery (Opponent)", 
                                                   "Fumble Recovery (Own)", 
                                                   "Fumble Return Touchdown"), 1, 0),
         rush_fumbled = if_else(pass_rush == "Rush" &
                                  play_type %in% c("Fumble Recovery (Opponent)", 
                                                   "Fumble Recovery (Own)", 
                                                   "Fumble Return Touchdown"), 1, 0))

overall_int_rate <- sum(scrimmage_plays$pass_intercepted) / sum(scrimmage_plays$pass_attempt, na.rm = TRUE)
  

# This works
qbs <- plays.master %>% 
  group_by(pass_player) %>% 
  filter(n() > 100) %>% 
  pull(pass_player) %>% 
  unique()

# Calculate passing metrics
qb_plays <- scrimmage_plays %>% 
  filter(player_name %in% qbs)

ints_only <- qb_plays %>% 
  group_by(game_id, offense, player_name) %>% 
  filter(pass_intercepted == 1) %>% 
  arrange(id) %>% 
  slice_head(n = 1) %>% 
  rename(int_id = id) %>% 
  select(game_id, offense, player_name, int_id)

qb_plays_2 <- qb_plays %>% 
  left_join(ints_only, by = c("game_id", "offense", "player_name")) %>% 
  ungroup() %>% 
  arrange(game_id, offense, pass_player, id) %>% 
  group_by(game_id, offense, player_name) %>% 
  mutate(int_thrown = case_when(id < int_id ~ "before",
                                id == int_id ~ "int",
                                id > int_id ~ "after",
                                TRUE ~ "before"))


qb_plays_2 %>% select(id, offense, pass_player, rush_player, play_text, int_thrown)

rm(plays.historic)

# Identify if a QB was benched
benchings <- qb_plays_2 %>% 
  group_by(game_id, offense) %>% 
  mutate(benched = case_when(pass_intercepted == 1 & 
                               (lead(player_name, n = 1L, order_by = id) != player_name) ~ 1,
                             TRUE ~ NA_real_))

# Bench Rate after an interception
bench_rate_all <- sum(benchings$benched, na.rm = TRUE) / sum(benchings$pass_intercepted)

team_colors <- get_colors()

# Most benched players
benchings %>% 
  group_by(player_name, offense) %>% 
  summarise(times_benched = sum(benched, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(times_benched)) %>% 
  slice_max(order_by = times_benched, n = 10) %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  ggplot(aes(x = times_benched, y = reorder(paste0(player_name, " - ", offense), times_benched),
             fill = as.character(color))) +
  geom_col(alpha = .5) +
  labs(title = "Most Benched Players",
       subtitle = "Player was taken out on the next play after throwing an interception",
       x = "Times Benched",
       y = "") +
  staturdays_theme +
  scale_fill_identity()

ggsave(filename = "most_benched.jpg",
       plot = last_plot(),
       path = "R Plots/",
       width = 200,
       height = 200,
       units = "mm",
       dpi = 300)

# Analyze before and after metrics ----------------------------------------

overall_before_after <- benchings %>% 
  group_by(int_thrown) %>% 
  summarise(completion_rate = sum(pass_completion, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE),
            pass_touchdown_rate = sum(pass_touchdown, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE),
            interception_rate = sum(pass_intercepted, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE),
            sack_rate = sum(passer_sacked, na.rm = TRUE)/n(),
            success_rate = sum(success, na.rm = TRUE) /n(),
            pass_rate = sum(pass_attempt, na.rm = TRUE)/sum(sum(pass_attempt, na.rm = TRUE), sum(rush_attempt, na.rm = TRUE), na.rm = TRUE),
            rush_touchdown_rate = sum(rush_touchdown, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE),
            rush_rate = sum(rush_attempt, na.rm = TRUE)/sum(sum(pass_attempt, na.rm = TRUE), sum(rush_attempt, na.rm = TRUE), na.rm = TRUE),
            pass_fumble_rate = sum(pass_fumbled, na.rm = TRUE)/sum(sum(pass_attempt, na.rm = TRUE), sum(passer_sacked, na.rm = TRUE), na.rm = TRUE),
            rush_fumble_rate = sum(rush_fumbled, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE),
            total_touchdown_rate = sum(sum(pass_touchdown, na.rm = TRUE), sum(rush_touchdown, na.rm = TRUE))/sum(pass_attempt, rush_attempt, na.rm = TRUE),
            avg_ppa = mean(ppa, na.rm = TRUE),
            n = n())

# Table of stats
library(reactable)
tidy_before_after <- overall_before_after %>% 
  filter(int_thrown != "int") %>% 
  pivot_longer(cols = -c(int_thrown)) %>% 
  pivot_wider(names_from = int_thrown) %>% 
  rename(stat = name) %>% 
  select(stat, before, after) %>% 
  filter(stat != "n")

reactable::reactable(tidy_before_after,
                     columns = list(
                       stat = colDef(name = "Stat"),
                       before = colDef(name = "Pre-Interception",
                                       format = colFormat(percent = FALSE, digits = 3)),
                       after = colDef(name = "Post-Interception",
                                      format = colFormat(percent = FALSE, digits = 3))
                     ),
                     pagination = FALSE,
                     theme = reactableTheme(
                       style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Roboto, Fira Mono, Chivo, serif/*rtl:Amiri, Georgia, Times New Roman, serif*/;")),
                     striped = TRUE)

# First Int Rate - .025 | After Int. Rate = .0281, slightly higher overall
benchings %>% 
  ungroup() %>% 
  filter(int_thrown != "after") %>% 
  summarise(int_rate = sum(pass_intercepted, na.rm = TRUE) / sum(pass_attempt, na.rm = TRUE),
            ints = sum(pass_intercepted, na.rm = TRUE),
            attempts = sum(pass_attempt, na.rm = TRUE))

# Initial results look a bit weird
# How to calculate the probability of an interception before one has happened vs. after?
## Just do overall interception rate vs. given one has already been thrown
## Filter out all after plays and just calculate the rate of first ints to total before plays
# Pass rate after int is 99% vs. 60% before, makes no sense. Same with rush rate
## issue was I was joining the int_id on passer_name, so rush plays were not getting int_ids and defaulting to "before"
## Could have been avoided by making the catch-all case in case_when named differently, or NA


# Analyze by player to see who handles throwing INTs the best -------------

player_stats <- benchings %>% 
  group_by(player_name, int_thrown) %>% 
  summarise(completion_rate = sum(pass_completion, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE),
            pass_touchdown_rate = sum(pass_touchdown, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE),
            interception_rate = sum(pass_intercepted, na.rm = TRUE)/sum(pass_attempt, na.rm = TRUE),
            sack_rate = sum(passer_sacked, na.rm = TRUE)/n(),
            success_rate = sum(success, na.rm = TRUE) /n(),
            pass_rate = sum(pass_attempt, na.rm = TRUE)/sum(sum(pass_attempt, na.rm = TRUE), sum(rush_attempt, na.rm = TRUE), na.rm = TRUE),
            rush_touchdown_rate = sum(rush_touchdown, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE),
            rush_rate = sum(rush_attempt, na.rm = TRUE)/sum(sum(pass_attempt, na.rm = TRUE), sum(rush_attempt, na.rm = TRUE), na.rm = TRUE),
            pass_fumble_rate = sum(pass_fumbled, na.rm = TRUE)/sum(sum(pass_attempt, na.rm = TRUE), sum(passer_sacked, na.rm = TRUE), na.rm = TRUE),
            rush_fumble_rate = sum(rush_fumbled, na.rm = TRUE)/sum(rush_attempt, na.rm = TRUE),
            total_touchdown_rate = sum(sum(pass_touchdown, na.rm = TRUE), sum(rush_touchdown, na.rm = TRUE))/sum(pass_attempt, rush_attempt, na.rm = TRUE),
            avg_ppa = mean(ppa, na.rm = TRUE),
            n = n())

player_lag <- player_stats %>% 
  filter(int_thrown != "int",
         n >= 100) %>% 
  group_by(player_name) %>% 
  mutate(n_cnt = n()) %>% 
  filter(n_cnt == 2) %>% # Remove players with not enough data
  group_by(player_name) %>% 
  arrange(player_name, desc(int_thrown)) %>% 
  mutate(across(.cols = -c(int_thrown, n, n_cnt), 
                .fns = ~ .x - lag(.x, n = 1L),
                .names = "{.col}_{.fn}")) %>% 
  filter(int_thrown == "after")


# Data Vis ----------------------------------------------------------------

# 2020 QBs
qbs_2020 <- plays.master %>% 
  filter(year == 2020) %>% 
  group_by(pass_player) %>% 
  filter(n() > 100) %>% 
  pull(pass_player) %>% 
  unique()

# Column chart
player_lag %>% 
  filter(player_name %in% qbs_2020) %>% 
  ggplot(aes(x = reorder(player_name, completion_rate_1), y = completion_rate_1)) +
  geom_col(aes(fill = if_else(completion_rate_1 > 0, "blue", "red")),
           alpha = .7) +
  coord_flip() +
  scale_fill_identity() +
  staturdays_theme +
  theme(axis.text.y = element_text(size = 8),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Completion Rate",
       title = "Handling Adversity",
       subtitle = paste0("Change in completion rate after throwing first INT\n",
                         "Career stats for QBs that played in 2020\n",
                         "Min 100 attempts before and after an interception"),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data"
       )

ggsave(filename = "qb_comp_col_chart_2020.jpg",
       plot = last_plot(),
       path = "R Plots/",
       width = 200,
       height = 600,
       units = "mm",
       dpi = 300)

# Column chart - PPA
player_lag %>% 
  filter(player_name %in% qbs_2020) %>% 
  ggplot(aes(x = reorder(player_name, avg_ppa_1), y = avg_ppa_1)) +
  geom_col(aes(fill = if_else(avg_ppa_1 > 0, "blue", "red")),
           alpha = .7) +
  coord_flip() +
  scale_fill_identity() +
  geom_text(aes(label = round(avg_ppa_1, 3)),
            nudge_y = if_else({player_lag %>% 
                                filter(player_name %in% qbs_2020) %>% 
                                .$avg_ppa_1} > 0, .05, -.05)) +
  staturdays_theme +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(labels = scales::number_format(accuracy = .001)) +
  labs(y = "Average PPA (Predicted Points Added)",
       title = "Handling Adversity",
       subtitle = paste0("Change in average PPA per play after throwing first INT\n",
                         "Career stats for QBs that played in 2020\n",
                         "Min 100 attempts before and after an interception"),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data"
  )

ggsave(filename = "qb_ppa_col_chart_2020.jpg",
       plot = last_plot(),
       path = "R Plots/",
       width = 200,
       height = 400,
       units = "mm",
       dpi = 300)

# Column chart - Sacks
player_lag %>% 
  filter(player_name %in% qbs_2020) %>% 
  ggplot(aes(x = reorder(player_name, sack_rate_1), y = sack_rate_1)) +
  geom_col(aes(fill = if_else(sack_rate_1 > 0, "red", "blue")),
           alpha = .7) +
  coord_flip() +
  scale_fill_identity() +
  geom_text(aes(label = round(sack_rate_1, 3)),
            nudge_y = if_else({player_lag %>% 
                filter(player_name %in% qbs_2020) %>% 
                .$sack_rate_1} > 0, .005, -.005)) +
  staturdays_theme +
  theme(axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(y = "Change in Sack Rate",
       title = "Increased Pressure",
       subtitle = paste0("Change in QB sack rate after throwing first INT\n",
                         "Career stats for QBs that played in 2020\n",
                         "Min 100 attempts before and after an interception"),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data"
  )

ggsave(filename = "qb_ppa_col_chart_2020_sack.jpg",
       plot = last_plot(),
       path = "R Plots/",
       width = 200,
       height = 400,
       units = "mm",
       dpi = 300)

# 2D Scatterplot
player_lag %>% 
  ggplot(aes(x = avg_ppa_1, y = total_touchdown_rate_1)) +
  geom_point(color = staturdays_colors("lightest_blue"),
             aes(alpha = if_else(avg_ppa_1 > .1 |
                                   avg_ppa_1 < -.1 |
                                   total_touchdown_rate_1 > .05 |
                                   total_touchdown_rate_1 < -.05, .8, .1))) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  ggrepel::geom_text_repel(aes(label = if_else(avg_ppa_1 > .1 |
                                                 avg_ppa_1 < -.1 |
                                                 total_touchdown_rate_1 > .05 |
                                                 total_touchdown_rate_1 < -.05,
                                               player_name,
                                               "")),
                           force = 10) +
  staturdays_theme +
  labs(y = "Change in Total Touchdown Rate (Passing + Rushing)",
       x = "Change in Average PPA (Predicted Points Added)",
       title = "Changing Performance After an INT",
       subtitle = paste0("Career stats for QBs 2014-2020\n",
                         "Min 100 attempts before and after an interception"),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data"
  ) +
  theme(axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = .001)) +
  scale_x_continuous(labels = scales::number_format(accuracy = .001)) +
  annotate(geom = "label", x = .2, y = .075, label = "More TDs\nBetter Play",
           fill = staturdays_colors("orange"),
           color = "white") +
  annotate(geom = "label", x = -.55, y = -.085,
           label = "Less TDs\nWorse Play",
           fill = staturdays_colors("orange"),
           color = "white") +
  coord_cartesian(clip = "off")

ggsave(filename = "qb_ppa_td_scatter_plot.jpg",
       plot = last_plot(),
       path = "R Plots/",
       width = 200,
       height = 200,
       units = "mm",
       dpi = 300)

# 2D Scatterplot - consistency
player_lag %>% 
  ggplot(aes(x = avg_ppa_1, y = total_touchdown_rate_1)) +
  geom_point(color = staturdays_colors("lightest_blue"),
             aes(alpha = if_else(abs(avg_ppa_1) < .05 &
                                   abs(total_touchdown_rate_1) < .01, .8, .1))) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  ggrepel::geom_text_repel(aes(label = if_else(abs(avg_ppa_1) < .05 &
                                                 abs(total_touchdown_rate_1) < .01,
                                               player_name,
                                               "")),
                           force = 10,
                           max.overlaps = 45) +
  staturdays_theme +
  labs(y = "Change in Total Touchdown Rate (Passing + Rushing)",
       x = "Change in Average PPA (Predicted Points Added)",
       title = "Changing Performance After an INT",
       subtitle = paste0("Career stats for QBs 2014-2020\n",
                         "Min 100 attempts before and after an interception"),
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data"
  ) +
  theme(axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        legend.position = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = .001)) +
  scale_x_continuous(labels = scales::number_format(accuracy = .001)) +
  annotate(geom = "label", x = .2, y = .075, label = "More TDs\nBetter Play",
           fill = staturdays_colors("orange"),
           color = "white") +
  annotate(geom = "label", x = -.55, y = -.085,
           label = "Less TDs\nWorse Play",
           fill = staturdays_colors("orange"),
           color = "white") +
  coord_cartesian(clip = "off")

ggsave(filename = "qb_ppa_td_scatter_plot_consistent.jpg",
       plot = last_plot(),
       path = "R Plots/",
       width = 200,
       height = 200,
       units = "mm",
       dpi = 300)
  

# Facet Names Before and After
player_facet <- player_stats %>% 
  filter(int_thrown != "int",
         n >= 100) %>% 
  group_by(player_name) %>% 
  mutate(n_cnt = n()) %>% 
  filter(n_cnt == 2) %>% # Remove players with not enough data
  group_by(player_name) %>% 
  arrange(player_name, desc(int_thrown)) %>% 
  mutate(across(.cols = -c(int_thrown, n, n_cnt), 
                .fns = ~ .x - lag(.x, n = 1L),
                .names = "{.col}_{.fn}"))

player_facet %>% 
  group_by(player_name) %>% 
  mutate(improve = case_when(completion_rate_1 > 0 | 
                             lead(completion_rate_1) > 0 ~ "1",
                             completion_rate_1 <= 0 |
                               lead(completion_rate_1) <= 0 ~ "0",
                             TRUE ~ "missing")) %>% 
  filter(player_name %in% qbs_2020) %>% 
  ggplot(aes(x = reorder(int_thrown, desc(int_thrown)), y = completion_rate, group = player_name)) +
  geom_point() +
  geom_path(arrow = arrow(length = unit(2, "mm")),
            aes(color = if_else(improve == "1", 
                                "blue", 
                                "red"))) +
  facet_wrap(vars(player_name),
             ncol = 5) +
  scale_color_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_discrete(labels = c("Before", "After")) +
  staturdays_theme +
  labs(y = "Completion %",
       title = "Change in Completion Rate Post-INT",
       subtitle = "QBs who played in 2020",
       caption = "@kylebeni012 for @staturdays | Data: @cfb_data") +
  theme(plot.title = element_text(size = 25),
        axis.title.x = element_blank())

ggsave(filename = "qb_cmp_before_after_2020.jpg",
       plot = last_plot(),
       path = "R Plots/",
       width = 200,
       height = 400,
       units = "mm",
       dpi = 300)

