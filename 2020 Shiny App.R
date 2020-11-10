### Load necessary packages ###
library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(scales)
library(DT)
#library(rjson)
library(jsonlite)
library(htmlwidgets)
library(gt)
library(data.table)
library(RCurl)
library(XML)
library(stringr)
library(ggimage)
library(grid)
library(png)
library(bit64)
library(reactable)

# Read in existing data for 2020 from Github
plays.historic <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/2020_plays_ytd.csv")

# Make ids characters
plays.master.temp <- plays.historic %>% 
  mutate(id = as.character(id, game_id, drive_id))

plays.historic <- plays.master.temp
rm(plays.master.temp)

# Get max week from data to see what to check for from the cfbdata API
max_week <- plays.historic %>% pull(week) %>% max()

# Plays by percentile to help guide defining explosive
percentile_explosiveness <- plays.historic %>% group_by(pass_rush) %>% 
  summarise(avg_yds = mean(yards_gained), 
            stand_dev = sd(yards_gained), 
            percentile = quantile(yards_gained, .90))

explosive_pass <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Pass") %>% pull(percentile))
explosive_rush <- as.numeric(percentile_explosiveness %>% filter(pass_rush == "Rush") %>% pull(percentile))


# Required Themes and Data ------------------------------------------------

#Staturdays Colors

staturdays_col_list <- c(
  lightest_blue = "#5c6272",
  lighter_blue = "#4c5872",
  light_blue = "#394871",
  medium_blue = "#22345a",
  dark_blue = "#041e42",
  orange = "#de703b",
  sign = "#1e1e1e",
  white = "#FFFFFF"
)

staturdays_palette <- c("#041e42", "#22345a", "#394871", "#4c5872", "#5c6272", "#de703b")

staturdays_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (staturdays_col_list)
  
  staturdays_col_list[cols]
}

staturdays_theme <- theme(plot.caption = element_text(size = 12, hjust = 1, color = staturdays_colors("orange")), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 30, face = "bold"),
                          plot.subtitle = element_text(color = staturdays_colors("light_blue"), size = 20),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          axis.title = element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.title = element_text(color = staturdays_colors("lighter_blue"), size = 16, face = "bold"),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 14),
                          panel.background = element_blank(),
                          panel.grid = element_line(color = "#d6d6d6"),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_line(color = "#d6d6d6")
)

# Team Colors and Logos
team_colors <- fromJSON(getURL("https://api.collegefootballdata.com/teams/fbs?year=2020"))

team_colors <- team_colors %>% unnest(cols = logos) %>% 
  mutate(logo_color = if_else(str_detect(logos, "dark"), "dark", "light")) %>% 
  pivot_wider(names_from = logo_color, values_from = logos)

# Logo
#logo <- grid::rasterGrob(png::readPNG("C:/Users/Kyle/Documents/Kyle/Staturdays/Logo Final/4thdownmarkerlogo.png"), interpolate = T)

# Power 5 List

power_5 <- c("ACC", "Big 12", "Big Ten", "Pac-12", "SEC")

scrimmage_plays_all <- 
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Own)", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown"
  )

scrimmage_plays_non_turnover <-
  c(
    "Rush", 
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Rushing Touchdown", 
    "Sack", 
    "Fumble Recovery (Own)"
  )

scrimmage_plays_turnover <-
  c(
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown", 
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown",
    "Safety"
  )

scrimmage_plays_pass <-
  c(
    "Pass Reception", 
    "Pass Incompletion", 
    "Pass Completion", 
    "Passing Touchdown", 
    "Sack",
    "Pass Interception", 
    "Pass Interception Return", 
    "Interception Return Touchdown"
  )

scrimmage_plays_rush <-
  c(
    "Rush", 
    "Rushing Touchdown"
  )

no_action_plays <- 
  c(
    "Timeout",
    "End Period",
    "End of Half",
    "End of Game",
    "Kickoff"
  )

scrimmage_plays_kicks <- 
  c(
    "Punt",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Field Goal Good",
    "Blocked Field Goal",
    "Missed Field Goal Return"
  )

base_url_plays <- "https://api.collegefootballdata.com/plays?" # Base URL to work off of
base_url_games <- "https://api.collegefootballdata.com/games?" # Base URL for games data
base_url_drives <- "https://api.collegefootballdata.com/drives?" # Base URL for drives data


# Pull in new games from API ----------------------------------------------

start_week <- max_week + 1

plays.master = data.frame()
for (j in 2020:2020) {
  for (i in start_week:15) {
    cat('Loading Plays', j, 'Week', i, '\n')
    full_url_plays <- paste0(base_url_plays, "seasonType=both&", "year=", as.character(j), "&","week=", as.character(i)) # Concatenating year and week
    full_url_plays_encoded <- URLencode(full_url_plays) # If there are spaces in query, formats them correctly
    plays <- fromJSON(getURL(full_url_plays_encoded)) # Pull in API using url
    if(is_empty(plays) == F){
      clockcolumns <- plays %>% unnest_legacy(clock) # Takes clock data out as it's own columns
      plays <- plays %>%
        select(-clock) %>%
        as_tibble() %>%
        mutate(minutes = clockcolumns$minutes, seconds = clockcolumns$seconds) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
        mutate_at(c("minutes", "seconds"), ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
      plays <- tibble(plays)
      plays$week = i
      plays$year = j
      plays.master = rbind(plays.master, plays, make.row.names=TRUE)
    }
  }
}
if(is_empty(plays.master)==F){rm(clockcolumns, plays)}

games.master = data.frame()
for (j in 2020:2020) {
  for (i in start_week:15) {
    cat('Loading Games', j, 'Week', i, '\n')
    full_url_games <- paste0(base_url_games, "year=", as.character(j), "&week=", as.character(i), "&seasonType=both")
    full_url_games_encoded <- URLencode(full_url_games)
    games <- fromJSON(getURL(full_url_games_encoded))
    games <- as_tibble(games)
    games.master = rbind(games.master, games)
  }
}

drives.master = data.frame()
for (j in 2020:2020) {
  cat('Loading Drives', j, '\n')
  full_url_drives <- paste0(base_url_drives, "seasonType=both&", "year=", as.character(j))
  full_url_drives_encoded <- URLencode(full_url_drives)
  drives <- fromJSON(getURL(full_url_drives_encoded))
  start_time_columns <- as_tibble(drives$start_time) # Takes clock data out as it's own columns
  end_time_columns <- as_tibble(drives$end_time)
  elapsed_time_columns <- as_tibble(drives$elapsed)
  drives <- drives %>% 
    select(-start_time, -end_time, -elapsed) %>% 
    as_tibble() %>% 
    mutate(start_minutes = start_time_columns$minutes, start_seconds = start_time_columns$seconds, 
           end_minutes = end_time_columns$minutes, end_seconds = end_time_columns$seconds,
           elapsed_minutes = elapsed_time_columns$minutes, elapsed_seconds = elapsed_time_columns$seconds,) %>% # Drop old clock dataframe, make a tibble, and add on each individual column of minutes and seconds
    mutate_at(
      c("start_minutes", "start_seconds", "end_minutes", "end_seconds", "elapsed_minutes", "elapsed_seconds"),
      ~replace(., is.na(.), 0)) # need to turn NAs in clock into 0s
  drives <- as_tibble(drives)
  drives.master = rbind(drives.master, drives)
}

# Do Calculations ---------------------------------------------------------

if(is_empty(plays.master) == F) {

## Mutate Plays with First Downs (Smart), Pass/Rush, Success
plays.master_temp <- plays.master %>% 
  mutate(play_specifics = play_type) %>% 
  mutate(first_down = 
           case_when(
             play_type %in% scrimmage_plays_all ~
               ifelse(yards_gained >= distance 
                      & lead(down, n=1, order_by = id) == 1 
                      & lead(offense, n = 1, order_by = id) == offense 
                      & lead(drive_id, n = 1, order_by = id) == drive_id 
                      & (!play_specifics %in% scrimmage_plays_turnover), TRUE, 
                      ifelse(play_specifics %in% c("Passing Touchdown", "Rushing Touchdown") 
                             | ((str_detect(play_text, "TOUCHDOWN") 
                                 | str_detect(play_text, "1ST down")) 
                                & (!play_specifics %in% scrimmage_plays_turnover)), TRUE, FALSE)),
             TRUE ~ NA
           )
  )

plays.master <- plays.master_temp
rm(plays.master_temp)
# Calculate loss of yards on turnovers
plays.master_temp <- plays.master %>% 
  mutate(turnover_yards = case_when(play_type %in% scrimmage_plays_turnover ~ 70L - lead(yards_to_goal, order_by = id), #subtract starting field position of next drive from avg. starting field position to find how many yards that turnover cost you
                                    TRUE ~ 0L))

plays.master_temp <- plays.master_temp %>% mutate(yards_gained = case_when(play_type %in% scrimmage_plays_turnover ~ -turnover_yards,
                                                                           TRUE ~ yards_gained))

plays.master <- plays.master_temp
rm(plays.master_temp)

# Calculate avg. starting field pos on turnovers vs. normal
#drives.master %>% summarise(mean(start_yards_to_goal)) # 70 avg.

# plays.master %>% mutate(lead_start_yards_to_goal = 
#                           case_when(play_type %in% scrimmage_plays_turnover ~ lead(yards_to_goal, order_by = id),
#                                     TRUE ~ 0L)) %>% 
#   filter(play_type %in% scrimmage_plays_turnover) %>% 
#   group_by(play_type) %>% 
#   filter(!str_detect(play_type,"Touchdown")) %>% 
#   summarise(avg_yds_to_goal = mean(lead_start_yards_to_goal), count = n()) # 54.5 avg, so a turnover is worth -15.5 yds. vs. avg.

# Classify the multiple play types into a simpler just rush or pass
plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_pass] <- "Pass"
plays.master$pass_rush[plays.master$play_type %in% scrimmage_plays_rush] <- "Rush"
# Rush Fumble Rows
rush_rows <- plays.master %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", 
                               "Fumble Recovery (Opponent)", 
                               "Fumble Return Touchdown", 
                               "Safety"), 
         str_detect(play_text, "run") 
         & !str_detect(play_text, "kick") 
         & !str_detect(play_text, "punt")) %>% 
  mutate(pass_rush = "Rush")
# Pass Fumble Rows
pass_rows <- plays.master %>% 
  filter(play_specifics %in% c("Fumble Recovery (Own)", "Fumble Recovery (Opponent)"), str_detect(play_text, "pass") | str_detect(play_text, "sack")) %>% 
  mutate(pass_rush = "Pass")
# Change fumbles to a pass or rush
plays.master[which(plays.master$id %in% rush_rows$id), "pass_rush"] <- "Rush"
plays.master[which(plays.master$id %in% pass_rows$id), "pass_rush"] <- "Pass"

#Add Success Column
plays.master_temp <- plays.master %>% 
  mutate(success = 
           case_when(
             play_type %in% scrimmage_plays_turnover ~ 0,
             down == 1 & yards_gained >= 0.5 * distance ~ 1,
             down == 2 & yards_gained >= 0.7 * distance ~ 1,
             down == 3 & yards_gained >= 1 * distance ~ 1,
             down == 4 & yards_gained >= 1 * distance ~ 1,
             str_detect(play_text, "1ST down") == TRUE ~ 1,
             str_detect(play_text, "TD") == TRUE ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master_temp
rm(plays.master_temp)
##

## Add Line Yards Stat

plays.master.temp <- plays.master %>% 
  mutate(lineYards = 
           case_when(
             pass_rush == "Rush" & yards_gained < 0 ~ yards_gained * 1.2,
             pass_rush == "Rush" & yards_gained > 0 & yards_gained <= 4 ~ yards_gained * 1,
             pass_rush == "Rush" & yards_gained > 4 & yards_gained <= 10 ~ 4 + yards_gained * 0.5,
             pass_rush == "Rush" & yards_gained > 10 ~ 7,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

## Garbage Time Indicator WIP

plays.master.temp <- plays.master %>% 
  mutate(garbage_time = 
           case_when(
             period == 2 & abs(offense_score - defense_score) > 38 ~ 1,
             period == 3 & abs(offense_score - defense_score) > 28 ~ 1,
             period == 4 & abs(offense_score - defense_score) > 22 ~ 1,
             period == 4 & minutes < 2 & abs(offense_score - defense_score) > 16 ~ 1,
             TRUE ~ 0
           )
  )
plays.master <- plays.master.temp
rm(plays.master.temp)
##

# Passing vs. Standard Downs, Explosive Plays
plays.master.temp <- plays.master %>% 
  mutate(passing_down = case_when(down == 2 & distance >= 7 ~ 1,
                                  down == 3 & distance >= 5 ~ 1,
                                  down == 4 & distance >= 5 ~ 1,
                                  TRUE ~ 0),
         explosive = case_when(play_specifics %in% scrimmage_plays_turnover ~ 0,
                               yards_gained >= explosive_pass 
                               & play_specifics %in% scrimmage_plays_non_turnover 
                               & pass_rush == "Pass" ~ 1,
                               yards_gained >= explosive_rush
                               & play_specifics %in% scrimmage_plays_non_turnover 
                               & pass_rush == "Rush" ~ 1,
                               TRUE ~ 0))

plays.master <- plays.master.temp
rm(plays.master.temp)

# Create a master clock completely in seconds
plays.master.temp <- plays.master %>% 
  mutate(clock_in_seconds = 2700-(900*(period-1)) + minutes*60 + seconds)

plays.master <- plays.master.temp
rm(plays.master.temp)

# Read in expected success rates
success_expected <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/expected_success_rate.csv")

# Join to plays
plays.master_temp <- plays.master %>% 
  left_join(success_expected, by = c("down", "distance")) %>% 
  select(-count)

plays.master <- plays.master_temp
rm(plays.master_temp)
}
# Rbind to Historic and Update --------------------------------------------
plays.master.temp <- rbind(plays.historic, plays.master)
plays.master <- plays.master.temp
rm(plays.master.temp)
rm(plays.historic)


# Calculate Summary Tables ------------------------------------------------
succ_rate_off <- plays.master %>% 
  filter(down != 0) %>% 
  group_by(offense, offense_conference, down) %>% 
  summarise(succ_rate = mean(success), off_play_count = n()) %>% 
  left_join(team_colors, by = c("offense" = "school"))

succ_rate_def <- plays.master %>% 
  filter(down != 0) %>% 
  group_by(defense, defense_conference, down) %>% 
  summarise(succ_rate = mean(success), def_play_count = n()) %>% 
  left_join(team_colors, by = c("defense" = "school"))

# Explosiveness
explosive_summary <- plays.master %>% 
  filter(!play_specifics %in% no_action_plays, is.na(pass_rush) == F) %>% 
  filter(!play_specifics %in% c("Punt", "Blocked Punt", "Blocked Punt Touchdown")) %>% 
  group_by(offense) %>% 
  mutate(team_explosive_rate = mean(explosive)) %>% 
  group_by(offense, offense_conference, pass_rush) %>% 
  summarise(explosive_rate = mean(explosive), team_explosive_rate = mean(team_explosive_rate), count = n())

# Turnover Yards
turnover_yds <- plays.master %>% 
  filter(play_type %in% scrimmage_plays_turnover) %>% 
  group_by(offense) %>% 
  summarise(avg_turnover_yards = -mean(turnover_yards), offense_conference, count = n()) %>% 
  left_join(team_colors, by = c("offense" = "school"))

# Starting Field Position
off_field_pos <- drives.master %>% 
  group_by(offense, offense_conference) %>% 
  summarise(avg_start_field_pos = mean(start_yards_to_goal))

def_field_pos <- drives.master %>% 
  group_by(defense, defense_conference) %>% 
  summarise(avg_start_field_pos = mean(start_yards_to_goal))

field_pos <- left_join(off_field_pos, def_field_pos, by = c("offense" = "defense"), suffix = c("_off", "_def")) %>% 
  mutate(net_field_pos = avg_start_field_pos_def - avg_start_field_pos_off) %>% 
  group_by(offense) %>% 
  left_join(team_colors, by = c("offense" = "school")) %>% 
  ungroup()

# Pass and Rush Rates by Passing Down over Average
pass_rate_vs_avg_by_down <- plays.master %>% group_by(passing_down) %>% 
  filter(play_type %in% scrimmage_plays_all) %>% 
  mutate(cfb_pass_rate = mean(pass_rush == "Pass")) %>% 
  group_by(offense, offense_conference, passing_down) %>% 
  summarise(pass_rate = mean(pass_rush == "Pass"), cfb_pass_rate = mean(cfb_pass_rate)) %>% 
  mutate(pass_vs_avg = (pass_rate - cfb_pass_rate)/(cfb_pass_rate)) %>% 
  pivot_wider(names_from = c("passing_down"), values_from = c("pass_rate", "cfb_pass_rate", "pass_vs_avg")) %>% 
  select(offense,
         offense_conference,
         pass_rate_standard_downs = pass_rate_0,
         cfb_pass_standard = cfb_pass_rate_0,
         pass_vs_avg_standard = pass_vs_avg_0,
         pass_rate_passing_downs = pass_rate_1,
         cfb_pass_passing = cfb_pass_rate_1,
         pass_vs_avg_passing = pass_vs_avg_1
         )

# UI ----------------------------------------------------------------------

ui <- navbarPage(title = "Staturdays | CFB Stats and Analysis",
                 tabPanel(title = "Team Plots",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput(inputId = "conference", label = "Choose which conferences to plot", 
                                             choices = unique(plays.master$offense_conference),
                                             selected = "Big Ten"),
                              numericInput(inputId = "startweek", label = "Start Week", value = 1, min = 1, max = max(plays.master$week), step = 1),
                              numericInput(inputId = "endweek", label = "End Week", value = max(plays.master$week), min = 1, max = max(plays.master$week), step = 1)
                            ),
                            mainPanel(fluidRow(
                              column(
                                h1("Offense"), plotOutput(outputId = "success_rate_off", width = "100%", height = "1000px"), width = 6
                              ),
                              column(
                                h1("Defense"), plotOutput(outputId = "success_rate_def", width = "100%", height = "1000px"), width = 6
                              )
                              ),
                              plotOutput(outputId = "explosiveness"),
                              plotOutput(outputId = "turnover_yards"),
                              plotOutput(outputId = "field_position"),
                                      tags$p("A shiny app by ",
                                             tags$a("Kyle Bennison", href="https://www.linkedin.com/in/kylebennison", target="_blank"), 
                                             " - ", 
                                             tags$a("@kylebeni012", href="https://www.twitter.com/kylebeni012", target="_blank")),
                                      tags$p("Data - ", 
                                             tags$a("@CFB_Data" , href="https://www.collegefootballdata.com", target="_blank")),
                                      tags$p("For more college football stats and analytics visit ", 
                                            tags$a("Staturdays.com" , href="https://www.staturdays.com", target="_blank")),
                                      )
                          )
                          ),
                 tabPanel(title = "Competitive Tendencies",
                          reactableOutput(outputId = "pass_rate"))
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Summary Stats
  
  ## Success Rate
  # Offense
  success_rate_offense <- reactive({
    succ_rate_off %>% 
      filter(offense_conference %in% input$conference) %>% 
      group_by(down) %>% 
      mutate(rank = rank(desc(succ_rate), ties.method = "min"))
  }
  )
  # Defense
  success_rate_defense <- reactive({
    succ_rate_def %>% 
      filter(defense_conference %in% input$conference) %>% 
      group_by(down) %>% 
      mutate(rank = rank(succ_rate, ties.method = "min"))
  }
  )
  
  # Explosiveness
  explosiveness <- reactive({
    explosive_summary %>% 
      filter(offense_conference %in% input$conference)
  })
  
  # Turnover Yards
  turnover_yards <- reactive({
    turnover_yds %>% 
      filter(offense_conference %in% input$conference)
  })
  
  # Field Position
  field_position <- reactive({
    field_pos %>% 
      filter(offense_conference %in% input$conference) %>% 
      mutate(first_rank = rank(desc(net_field_pos), ties.method = "min"))
  })

# Plots -------------------------------------------------------------------
  
  # Success Rate Plot - OFF
  output$success_rate_off <- renderPlot({
    success_rate_offense() %>% 
      group_by(offense) %>% 
      ggplot(aes(x = rank, y = succ_rate, fill = color)) +
      geom_col(position = "dodge") +
      geom_image(aes(image = light), size = .1, by = "width", asp = 2, nudge_y = .01) +
      theme(aspect.ratio = 1/2) +
      facet_wrap(vars(down), nrow = 4) +
      scale_x_reverse(breaks = seq(1:max(success_rate_offense()$rank))) +
      scale_fill_identity() +
      geom_label(aes(label = off_play_count), nudge_y = -.25, size = 3, fill = "white") +
      labs(title = paste0(input$conference," \nSuccess Rate - ", max(plays.master$year)),
           subtitle = "Percent of plays successful \nand # of Plays",
           caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
           x = "Ranking",
           y = "Success Rate") +
      staturdays_theme +
      scale_y_continuous(labels = percent)
      
  })
  
  # Success Rate Plot - DEF
  output$success_rate_def <- renderPlot({
    success_rate_defense() %>% 
      group_by(defense) %>% 
      ggplot(aes(x = rank, y = succ_rate, fill = color)) +
      geom_col(position = "dodge") +
      geom_image(aes(image = light), size = .1, by = "width", asp = 2, nudge_y = .01) +
      theme(aspect.ratio = 1/2) +
      facet_wrap(vars(down), nrow = 4) +
      scale_x_reverse(breaks = seq(1:max(success_rate_defense()$rank))) +
      scale_fill_identity() +
      geom_label(aes(label = def_play_count), nudge_y = -.25, size = 3, fill = "white") +
      labs(title = paste0(input$conference," \nSuccess Rate - ", max(plays.master$year)),
           subtitle = "Percent of plays successful \nand # of Plays",
           caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
           x = "Ranking",
           y = "Success Rate") +
      staturdays_theme +
      scale_y_continuous(labels = percent)
    
  })
  
  output$explosiveness <- renderPlot({
    explosiveness() %>% 
      pivot_wider(names_from = pass_rush, values_from = c(explosive_rate, count)) %>% 
      left_join(team_colors, by = c("offense" = "school")) %>% 
      ggplot(aes(x = explosive_rate_Pass, y = explosive_rate_Rush)) +
      geom_image(aes(image = light), size = .1, by = "width", asp = 1.5, alpha = 0.8) +
      theme(aspect.ratio = 1/1.5) +
      scale_x_continuous(labels = percent, limits = c(0, max(explosiveness()$explosive_rate))) +
      scale_y_continuous(labels = percent, limits = c(0, max(explosiveness()$explosive_rate))) +
      geom_abline(linetype = "dashed", color = staturdays_colors("orange")) +
      annotate(geom = "label", x = max(explosiveness()$explosive_rate) * (1/6), y = max(explosiveness()$explosive_rate) * (5/6), label = "Explosive \nRushing", 
               fill = staturdays_colors("orange"), color = "white", alpha = .75) +
      annotate(geom = "label", x = max(explosiveness()$explosive_rate) * (5/6), y = max(explosiveness()$explosive_rate) * (1/6), label = "Explosive \nPassing", 
               fill = staturdays_colors("orange"), color = "white", alpha = .75) +
      labs(title = "Explosiveness on \nOffense",
           subtitle = "Percent of explosive \nruns and passes,\ndefined as 90th percentile plays",
           caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
           x = paste0("Explosive Pass Rate (>= ", explosive_pass," yds)"),
           y = paste0("Explosive Rush Rate (>= ", explosive_rush," yds)")) +
      staturdays_theme +
      #annotation_custom(logo, xmin = .16, xmax = .24, ymin = .02, ymax = 0.07) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = unit(c(1,1,1,1), "lines"))
  })
  
  # Turnover Yards
  output$turnover_yards <- renderPlot({
    turnover_yards() %>% 
      ggplot(aes(x = avg_turnover_yards, y = count)) +
      geom_image(aes(image = light), size = .1, by = "width", asp = 1.5, alpha = 0.8) +
      theme(aspect.ratio = 1/1.5) +
      scale_x_continuous() +
      scale_y_continuous() +
      annotate(geom = "label", x = max(turnover_yards()$avg_turnover_yards)*.8, y = max(turnover_yards()$count)*1.25, label = "Turnovers in \nfavorable positions",
               fill = staturdays_colors("orange"), color = "white", alpha = 0.75) +
      annotate(geom = "label", x = min(turnover_yards()$avg_turnover_yards)*.8, y = max(turnover_yards()$count)*1.25, label = "Turnovers in \nunfavorable positions",
               fill = staturdays_colors("orange"), color = "white", alpha = 0.75) +
      annotate(geom = "label", x = mean(turnover_yards()$avg_turnover_yards), y = max(turnover_yards()$count)*1.5, label = "Average starting field position is at \nown 30. A turnover that puts opponent at \ntheir own 40 would be -10 Turnover Yds.",
               fill = staturdays_colors("light_blue"), color = "white", alpha = 0.75, size = 3) +  
      staturdays_theme +
      labs(title = paste0(input$conference, " Average \nTurnover Yards"),
           subtitle = "Free yards given up \nto opponents on turnovers",
           caption = "@staturdays | @kylebeni012 - Data: @cfb_data",
           x = "Average Net Yards on Turnovers",
           y = "# of Turnovers") +
      coord_cartesian(clip = "off") +
      theme(plot.margin = unit(c(1,1,1,1), "lines"))
  })
  
  # Field Position
  output$field_position <- renderPlot({
    field_position() %>% 
      ggplot(aes(x = net_field_pos, y = first_rank)) +
      geom_image(aes(image = light), size = .1, by = "width", asp = 1.5, alpha = 0.8) +
      theme(aspect.ratio = 1/1.5) +
      geom_vline(xintercept = 0, linetype = "dashed", color = staturdays_colors("orange")) +
      annotate(geom = "label", x = min(field_position()$net_field_pos)*.8, y = 3, label = "Worse field position \nthan opponents",
               fill = staturdays_colors("orange"), color = "white") +
      annotate(geom = "label", x = max(field_position()$net_field_pos)*.8, y = max(field_position()$first_rank)*.8, label = "Better field position \nthan opponents",
               fill = staturdays_colors("orange"), color = "white") +
      labs(title = paste0(input$conference, " \nNet Field Positions"),
           subtitle = "Negative is bad",
           x = "Net Field Position",
           y = "Rank") +
      staturdays_theme +
      coord_cartesian(clip = "off") +
      theme(plot.margin = unit(c(1,1,1,1), "lines"))
  })
  

# Competitive Tendencies Tab ----------------------------------------------

  # Pass Rate
  output$pass_rate <- renderReactable({
    reactable(pass_rate_vs_avg_by_down, 
              columnGroups = list(
                colGroup(name = "Standard Downs", columns = c("pass_rate_standard_downs", "cfb_pass_standard", "pass_vs_avg_standard")),
                colGroup(name = "Passing Downs", columns = c("pass_rate_passing_downs", "cfb_pass_passing", "pass_vs_avg_passing"))
              ),
              columns = list(
                offense = colDef(name = "Offense"), 
                offense_conference = colDef(name = "Conference"), 
                pass_rate_standard_downs = colDef(name = "Pass Rate", format = colFormat(percent = T, digits = 1)), 
                cfb_pass_standard = colDef(name = "CFB Avg.", format = colFormat(percent = T, digits = 1)), 
                pass_vs_avg_standard = colDef(name = "Diff. vs. Avg.", format = colFormat(percent = T, digits = 1)), 
                pass_rate_passing_downs = colDef(name = "Pass Rate", format = colFormat(percent = T, digits = 1)), 
                cfb_pass_passing = colDef(name = "CFB Avg.", format = colFormat(percent = T, digits = 1)),
                pass_vs_avg_passing = colDef(name = "Diff vs. Avg.", format = colFormat(percent = T, digits = 1))),
              searchable = T,
              pagination = F)
    })
  
}

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
