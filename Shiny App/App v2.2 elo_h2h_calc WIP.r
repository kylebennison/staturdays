library(reactable)
library(tidyverse)
library(ggplot2)
library(data.table)
library(devtools)

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/source_everything.R")

colors <- data.table::fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/colors_logos.csv",
                            encoding = "UTF-8",
                            na.strings = "")

orange_pal <- function(x) grDevices::rgb(grDevices::colorRamp(c("#e6bba5", "#de703b"))(x), maxColorValue = 255)
dunkin_pal <- function(x) grDevices::rgb(grDevices::colorRamp(c("#861388", "#E6E6E9", "#de703b"))(x), maxColorValue = 255)
cool_pal <- c("#FFEFC1","#EBD4BB","#D7BAB5","#C39FB0","#AF84AA","#9B6AA4","#874F9E")
forest_palette <- c("#99ddc8","#95bf74","#659b5e","#556f44","#283f3b")
mint_palette <- c("#a0eec0","#8ae9c1","#86cd82","#72a276","#666b6a")
green_red_pal <- c("#d60d0d", "#FFFFFF", "#0dd686")
any_pal <- function(x, pal) grDevices::rgb(grDevices::colorRamp(c(pal))(x), maxColorValue = 255)

logos <- colors %>% 
  select(school, light)

elo_master <- get_elo() %>% 
  left_join(colors %>% select(school, color), by = c("team" = "school"))

elo_ratings <- elo_master %>% 
  group_by(team) %>% 
  slice_max(order_by = date, n = 1L) %>% 
  ungroup() %>% 
  mutate(rank = rank(desc(elo_rating), ties.method = "min"))

elo_ratings <- elo_ratings %>% 
  left_join(logos, by = c("team" = "school")) %>% 
  select(rank, team, light, everything())

# Add last-week's elo ranking to this week's
elo_last_week <- elo_master %>% 
  group_by(team) %>% 
  slice_max(order_by = date, n = 2L) %>% 
  slice_min(order_by = date, n = 1L) %>% 
  ungroup() %>% 
  mutate(rank = rank(desc(elo_rating), ties.method = "min")) %>% 
  select(team, elo_rating, rank) %>% 
  rename(last_week_elo = elo_rating,
         last_week_rank = rank)

elo_ratings <- elo_ratings %>% 
  left_join(elo_last_week, by = "team") %>% 
  mutate(elo_change = round(elo_rating - last_week_elo, digits = 0),
         rank_change = -(rank - last_week_rank)) %>% 
  select(-c(last_week_elo, last_week_rank))

# Calculate win probabilities this week
calendar <- data.table::fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Data/calendar.csv")
current_week <- calendar %>% 
  filter(lastGameStart >= lubridate::now()) %>% 
  pull(week) %>% 
  min()
current_year <- max(elo_ratings$season)


games_url <- "https://github.com/kylebennison/staturdays/raw/master/Data/games_2021.rds"
games_rds <- readRDS(gzcon(url(games_url)))

games_this_week <- games_rds %>% 
  filter(season == current_year,
         week == current_week)

calc_expected_score <- function(team_rating, opp_team_rating){
  quotient_home <- 10^((team_rating)/400)
  quotient_away <- 10^((opp_team_rating)/400)
  return(expected_score_home <- quotient_home / (quotient_home + quotient_away))
}

win_probs <- games_this_week %>% 
  left_join(elo_ratings, by = c("home_team" = "team")) %>% 
  left_join(elo_ratings, by = c("away_team" = "team"),
            suffix = c("_home", "_away")) %>% 
  mutate(home_elo_wp = calc_expected_score(elo_rating_home + if_else(neutral_site == TRUE,
                                                                     0L,
                                                                     55L), 
                                           elo_rating_away),
         away_elo_wp = 1 - home_elo_wp,
         start_date = lubridate::as_datetime(start_date),
         start_date = lubridate::with_tz(start_date, "America/New_York"),
         conference = if_else(home_conference == away_conference, home_conference,
                              paste0(away_conference, " @ ", home_conference)))

# Get betting
betting <- get_betting(current_year, current_year, current_week, current_week)

# Join to win_probs
win_probs <- win_probs %>% 
  left_join(betting, by = "id") %>% 
  mutate(homeMoneyline = if_else(homeMoneyline < 0, 
                                 as.character(homeMoneyline),
                                 paste0("+", homeMoneyline)),
         awayMoneyline = if_else(awayMoneyline < 0, 
                                 as.character(awayMoneyline),
                                 paste0("+", awayMoneyline))) %>% 
  filter(start_date >= lubridate::now())

win_probs_w_lines <- win_probs %>% 
  mutate(home_implied_odds = case_when(str_detect(homeMoneyline, "-") == TRUE ~ abs(as.integer(homeMoneyline))/(abs(as.integer(homeMoneyline)) + 100),
                                       str_detect(homeMoneyline, "-") == FALSE ~ 100/(abs(as.integer(homeMoneyline))+100),
                                       TRUE ~ 0),
         away_implied_odds = case_when(str_detect(awayMoneyline, "-") == TRUE ~ abs(as.integer(awayMoneyline))/(abs(as.integer(awayMoneyline)) + 100),
                                       str_detect(awayMoneyline, "-") == FALSE ~ 100/(abs(as.integer(awayMoneyline))+100),
                                       TRUE ~ 0)) %>% 
  filter(is.na(homeMoneyline) == FALSE)

# Calc expected value on Elo bets

expected_value_tbl <- win_probs_w_lines %>% 
  mutate(home_diff = home_elo_wp - home_implied_odds,
         away_diff = away_elo_wp - away_implied_odds,
         home_win_10d_bet = if_else(str_detect(homeMoneyline, "-") == TRUE,
                                    (abs(as.double(homeMoneyline) - 100)) / (abs(as.double(homeMoneyline)) / 10),
                                    (as.double(homeMoneyline) + 100) / 10),
         away_win_10d_bet = if_else(str_detect(awayMoneyline, "-") == TRUE,
                                    (abs(as.double(awayMoneyline) - 100)) / (abs(as.double(awayMoneyline)) / 10),
                                    (as.double(awayMoneyline) + 100) / 10),
         home_exp_value = ((home_win_10d_bet - 10) * home_elo_wp) - (10 * (1-home_elo_wp)),
         away_exp_value = ((away_win_10d_bet - 10) * away_elo_wp) - (10 * (1-away_elo_wp))) %>% 
  filter(home_exp_value > 1 | away_exp_value > 1) %>% 
  filter(start_date >= lubridate::now()) %>% 
  select(id, start_date, home_team, light_home, home_elo_wp, home_implied_odds, home_exp_value,
         away_team, light_away, away_elo_wp, away_implied_odds, away_exp_value)

# Home
home_exp_val <- expected_value_tbl %>% 
  select(id, start_date, contains("home")) %>% 
  mutate(home_away = "home") %>% 
  rename(team = home_team,
         light = light_home,
         elo_wp = home_elo_wp,
         implied_odds = home_implied_odds,
         exp_value = home_exp_value)

# Away
away_exp_val <- expected_value_tbl %>% 
  select(id, start_date, contains("away")) %>% 
  mutate(home_away = "away") %>% 
  rename(team = away_team,
         light = light_away,
         elo_wp = away_elo_wp,
         implied_odds = away_implied_odds,
         exp_value = away_exp_value)

exp_val_rbind <- rbind(home_exp_val, away_exp_val) %>% 
  arrange(id, start_date, home_away) 

# Add last game result to Elo Rating table
# Most recent home result for each team
home_top <- games_rds %>% 
  mutate(team = home_team) %>% 
  group_by(team) %>% 
  filter(!is.na(home_points)) %>% 
  slice_max(order_by = start_date, n = 1L)

# Most recent away result for each team
away_top <- games_rds %>% 
  mutate(team = away_team) %>% 
  group_by(team) %>% 
  filter(!is.na(home_points)) %>% 
  slice_max(order_by = start_date, n = 1L)

# Row-bind latest home and away games
bottom <- home_top %>% rbind(away_top) %>% 
  slice_max(order_by = start_date, n = 1L)

# Get last game result for each team
last_game_result <- bottom %>% 
  mutate(result = case_when(team == home_team & 
                              home_points > away_points ~ paste0("W ",
                                                                 away_team,
                                                                 " ",
                                                                 home_points,
                                                                 "-",
                                                                 away_points),
                            team == home_team & 
                              home_points < away_points ~ paste0("L ",
                                                                 away_team,
                                                                 " ",
                                                                 away_points,
                                                                 "-",
                                                                 home_points),
                            team == away_team & 
                              home_points < away_points ~ paste0("W ",
                                                                 home_team,
                                                                 " ",
                                                                 away_points,
                                                                 "-",
                                                                 home_points),
                            team == away_team & 
                              home_points > away_points ~ paste0("L ",
                                                                 home_team,
                                                                 " ",
                                                                 home_points,
                                                                 "-",
                                                                 away_points),
                            TRUE ~ "failed")) %>% 
  select(team, result)

elo_ratings <- elo_ratings %>% 
  left_join(last_game_result, by = "team")

#Bring in data for overtime sim and the overtime function
lookup_table <- data.table::fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/overtime_lookup_table.csv")
overtime_sim <- source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/overtime_function_only.R")

# UI ----------------------------------------------------------------------

ui <- shiny::navbarPage(title = "Staturdays",
                        shiny::navbarMenu(title = "Elo",
                                          shiny::tabPanel(title = "Expected Values",
                                                          tags$head(
                                                            tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")),
                                                          h2("Positive Expected Value Bets"),
                                                          h3(htmltools::HTML("Expected <span style='color: #0dd686; font-weight: bold'>Profit</span>/<span style='color: #d60d0d; font-weight: bold'>Loss</span> Based on $10 Bet<br>",
                                                               current_year, "<b>", "Week", current_week, "</b>")),
                                                          reactable::reactableOutput(outputId = "expected_values"),
                                                          htmltools::HTML("<p><sup>1</sup>WP = Win Probability</p>",
                                                                   "<p><sup>2</sup>Expected Value based on profit or loss from a $10 bet</p>")),
                                          shiny::tabPanel(title = "Win Probabilities This Week",
                                                          reactable::reactableOutput(outputId = "elo_win_probs")),
                                          shiny::tabPanel(title = "Elo Ratings",
                                                          reactable::reactableOutput(outputId = "elo_ratings")),
                                          shiny::tabPanel(title = "Graph Teams",
                                                          shiny::selectizeInput(inputId = "elo_plot_teams",
                                                                                label = "Choose one or more teams to plot",
                                                                                choices = unique(elo_ratings$team),
                                                                                multiple = TRUE,
                                                                                selected = elo_ratings[which(elo_ratings$elo_rating == max(elo_ratings$elo_rating)),2]),
                                                          shiny::plotOutput(outputId = "elo_plot")),
                                          shiny::tabPanel(title = "Head-to-Head Predictor",
                                                          fluidRow(
                                                            column(2,
                                                                   align = "center",
                                                                   selectizeInput(inputId = "elo_home",
                                                                                  label = "Home",
                                                                                  choices = unique(elo_ratings$team),
                                                                                  multiple = FALSE,
                                                                                  selected = {elo_ratings %>% filter(rank == 1)}[1,2])),
                                                            column(8,
                                                                   align = "center",
                                                                   reactable::reactableOutput(outputId = "elo_h2h")),
                                                            column(2,
                                                                   align = "center",
                                                                   selectizeInput(inputId = "elo_away",
                                                                                  label = "Away",
                                                                                  choices = unique(elo_ratings$team),
                                                                                  multiple = FALSE,
                                                                                  selected = {elo_ratings %>% filter(rank == 2)}[1,2]))
                                                          ),
                                                          fluidRow(
                                                            column(12, align = "center",
                                                                   radioButtons(inputId = "neutral_field",
                                                                                label = "Is the game being played at a neutral site?",
                                                                                choices = c("Yes", "No"),
                                                                                selected = "No"))
                                                          ))),
                        shiny::tabPanel(title = "Overtime Simulator",
                                        fluidRow(
                                          column(6, align="center",
                                          shiny::selectizeInput(inputId = "overtime_select_home",
                                                              label = "Home Team",
                                                              choices = unique(lookup_table$list_of_teams),
                                                              multiple = FALSE,
                                                              selected = "Alabama")
                                          ),
                                          column(6, align="center",
                                        shiny::selectizeInput(inputId = "overtime_select_away",
                                                              label = "Away Team",
                                                              choices = unique(lookup_table$list_of_teams),
                                                              multiple = FALSE,
                                                              selected = "Georgia")
                                          )
                                        ),
                                        fluidRow(
                                          column(12, align="center",
                                        tags$b(radioButtons("dist", "Who is on offense first?",
                                                     c("Home Team" = "Home Team",
                                                       "Not Sure Yet" = "unsure",
                                                       "Away Team" = "Away Team"),
                                                     selected = "unsure",
                                                     inline = TRUE)),
                                        tags$hr()
                                          )
                                        ),
                                        fluidRow(
                                          column(12, align="center",
                                        shiny::htmlOutput(outputId = "home_overtime_win"))
                                        )
                                        )
                        )

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  home <- reactive({
      elo_ratings %>%
        filter(team %in% c(input$elo_home))
  })
  
  away <- reactive({
      elo_ratings %>%
        filter(team %in% c(input$elo_away)) %>% 
      rename_with(~ paste0(.x, "_away"))
  })
  
  joined <- reactive({
    cbind(home(), away()) %>% 
    select(team, light, elo_rating, team_away, light_away, elo_rating_away)
  })
    
  binded <- reactive({
    joined() %>%
      mutate(
        home_elo_wp = calc_expected_score(
          elo_rating + if_else(input$neutral_field == "Yes",
                               0L,
                               55L),
          elo_rating_away
        ),
        away_elo_wp = 1 - home_elo_wp
      )
  })
  
  output$elo_h2h <- renderReactable({
    reactable(binded() %>% select(elo_rating, home_elo_wp, light, light_away, away_elo_wp, elo_rating_away),
              columns = list(
                elo_rating = colDef(name = "Home Elo"),
                home_elo_wp = colDef(name = "Home Elo WP",
                                     format = colFormat(percent = TRUE, digits = 1),
                                     style = function(value) {
                                       normalized <- (value) / (1)
                                       color <- any_pal(normalized, green_red_pal)
                                       list(background = color, "font-weight" = "bold",
                                            color = if_else(normalized > .9 | normalized < .1, 
                                                            "#ffffff", 
                                                            "#000000"))
                                     }),
                light = colDef(name = "",
                               cell = function(value, index) {
                                 image <- htmltools::img(src = value, height = "100px", alt = "")
                                 htmltools::tagList(
                                   div(style = list(display = "flex", flexDirection = "row", justifyContent = "space-between",
                                                    "align-items" = "center", gap = "0px 10px"),
                                       div(image)
                                   )
                                 )
                               }),
                light_away = colDef(name = "",
                                    cell = function(value, index) {
                                      image <- htmltools::img(src = value, height = "100px", alt = "")
                                      htmltools::tagList(
                                        div(style = list(display = "flex", flexDirection = "row", justifyContent = "space-between",
                                                         "align-items" = "center", gap = "0px 10px"),
                                            div(image)
                                        )
                                      )
                                    }),
                away_elo_wp = colDef(name = "Away Elo WP",
                                     format = colFormat(percent = TRUE, digits = 1),
                                     style = function(value) {
                                       normalized <- (value) / (1)
                                       color <- any_pal(normalized, green_red_pal)
                                       list(background = color, "font-weight" = "bold",
                                            color = if_else(normalized > .9 | normalized < .1, 
                                                            "#ffffff", 
                                                            "#000000"))
                                     }),
                elo_rating_away = colDef(name = "Away Elo")
              ),
              defaultColDef = colDef(format = colFormat(digits = 0),
                                     headerClass = "header",
                                     class = "number"))
  })
  
  output$home_overtime_win <- renderUI({
    
    if(input$dist == "Home Team" | input$dist == "Away Team") {
    
    overtime_results <- overtime_sim(home_team=input$overtime_select_home, away_team = input$overtime_select_away,
                                     start_with_ball = input$dist)
    }
    else{
      overtime_results1 <- overtime_sim(home_team=input$overtime_select_home, away_team = input$overtime_select_away,
                                        start_with_ball = "Home Team")
      overtime_results2 <- overtime_sim(home_team=input$overtime_select_home, away_team = input$overtime_select_away,
                                        start_with_ball = "Away Team")

      overtime_results <- list(.5*as.numeric(overtime_results1[1])+.5*as.numeric(overtime_results2[1]),
                               .5*as.numeric(overtime_results1[2])+.5*as.numeric(overtime_results2[2]),
                               .5*as.numeric(overtime_results1[3])+.5*as.numeric(overtime_results2[3]),
                               .5*as.numeric(overtime_results1[4])+.5*as.numeric(overtime_results2[4]))
    
      }
    str1 <- paste0(input$overtime_select_home ," Win Probability: ", round(100*as.numeric(overtime_results[1]),2),"%")
    str2 <- paste0(input$overtime_select_away," Win Probability: ", round(100*as.numeric(overtime_results[2]),2), "%")
    str3 <- paste0("Probability of more than 1 overtime period: ", round(100*as.numeric(overtime_results[3]),2), "%")
    str4 <- paste0("Probability of a 2-point attempt shootout: ", round(100*as.numeric(overtime_results[4]),2), "%")
    HTML(paste(tags$b(str1), str3, str4, tags$b(str2), sep = '<br/>'))
    
  })
  
  output$expected_values <- renderReactable(
    reactable(exp_val_rbind,
              columns = list(
                start_date = colDef(name = "Start Time (EST)",
                                    cell = function(x, index) {
                                      home_away <- exp_val_rbind[index, "home_away"]
                                      if(home_away == "away"){
                                        format(x, "%I:%M%p %a %b %d, %Y")
                                      } else {
                                        format(x, " ")
                                      }
                                    }),
                team = colDef(name = "Home"),
                light = colDef(name = "",
                                    cell = function(value, index) {
                                      image <- htmltools::img(src = value, height = "50px", alt = "")
                                      home_away <- exp_val_rbind[index, "home_away"] %>% stringr::str_to_title()
                                      htmltools::tagList(
                                        div(style = list(display = "flex", flexDirection = "row", justifyContent = "space-between",
                                                         "align-items" = "center", gap = "0px 10px"),
                                          div(image),
                                          div(home_away, style = list("font-family" = "Roboto Mono"))
                                          )
                                      )
                                    }),
                elo_wp = colDef(name = "Elo WP",
                                     class = "number",
                                style = function(value) {
                                  normalized <- (value) / (1)
                                  color <- any_pal(normalized, green_red_pal)
                                  list(background = color, "font-weight" = "bold",
                                       color = if_else(normalized > .9 | normalized < .1, 
                                                       "#ffffff", 
                                                       "#000000"))
                                }),
                implied_odds = colDef(name = "Implied WP",
                                           class = "number",
                                      style = function(value) {
                                        normalized <- (value) / (1)
                                        color <- any_pal(normalized, green_red_pal)
                                        list(background = color, "font-weight" = "bold",
                                             color = if_else(normalized > .9 | normalized < .1, 
                                                             "#ffffff", 
                                                             "#000000"),
                                             borderRight = "2px solid #000000")
                                      }),
                exp_value = colDef(name = "Expected Value",
                                        format = colFormat(currency = "USD"),
                                        class = "number",
                                        style = function(value) {
                                          normalized <- (value - min(exp_val_rbind$exp_value)) / (max(exp_val_rbind$exp_value) - min(exp_val_rbind$exp_value))
                                          color <- any_pal(normalized, green_red_pal)
                                          list(background = color, "font-weight" = "bold",
                                               color = if_else(normalized > .9 | normalized < .1, 
                                                               "#ffffff", 
                                                               "#000000"))
                                        }),
                home_away = colDef(show = FALSE),
                id = colDef(show = FALSE)
              ),
              theme = reactableTheme(
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Roboto, Fira Mono, Chivo, serif/*rtl:Amiri, Georgia, Times New Roman, serif*/;"),
                headerStyle = list(
                  borderColor = "#000000",
                  "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
                  "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)")
                )),
              defaultColDef = colDef(format = colFormat(digits = 1, percent = TRUE),
                                     headerClass = "header"),
              rowStyle = function(index){
                if (exp_val_rbind[index, "home_away"] == "home") {
                  list(borderBottom = "3px solid #828282")
                }
              },
              searchable = TRUE,
              defaultPageSize = 30,
              defaultSorted = c("start_date", "id", "home_away"),
              pagination = FALSE,
              striped = TRUE,
              borderless = FALSE,
              fullWidth = FALSE,
              #groupBy = c("id"),
              defaultExpanded = TRUE
              )
  )
  
  output$elo_win_probs <- renderReactable(
    reactable(win_probs %>% 
                select(start_date, away_team, awayMoneyline, light_away, away_elo_wp, home_elo_wp, light_home, homeMoneyline, home_team, conference),
              columns = list(
                start_date = colDef(name = "Start Time (EST)",
                                    cell = function(x) format(x, "%I:%M%p %a %b %d, %Y")),
                away_team = colDef(name = "Away"),
                awayMoneyline = colDef(name = "Moneyline"),
                light_away = colDef(name = "",
                                    cell = function(value) {
                                      image <- htmltools::img(src = value, height = "50px", alt = "")
                                      htmltools::tagList(
                                        htmltools::div(style = list("text-align" = "center"),
                                                       htmltools::div(style = list(display = "inline-block", width = "25px"), 
                                                                      image))
                                      )
                                    }),
                away_elo_wp = colDef(name = "Win Probability",
                                     style = function(value) {
                                       normalized <- (value) / (1)
                                       color <- dunkin_pal(normalized)
                                       list(background = color, "font-weight" = "bold",
                                            color = if_else(normalized < .3 | normalized > .7, 
                                                            "#ffffff", 
                                                            "#000000"))
                                     }),
                home_elo_wp = colDef(name = "Win Probability",
                                     style = function(value) {
                                       normalized <- (value) / (1)
                                       color <- dunkin_pal(normalized)
                                       list(background = color, "font-weight" = "bold",
                                            color = if_else(normalized < .3 | normalized > .7, 
                                                            "#ffffff", 
                                                            "#000000"))
                                     }),
                light_home = colDef(name = "",
                                    cell = function(value) {
                                      image <- htmltools::img(src = value, height = "50px", alt = "")
                                      htmltools::tagList(
                                        htmltools::div(style = list("text-align" = "center"),
                                        htmltools::div(style = list(display = "inline-block", width = "25px"), 
                                                       image))
                                      )
                                    }),
                homeMoneyline = colDef(name = "Moneyline"),
                home_team = colDef(name = "Home"),
                conference = colDef(name = "Conference")
              ),
              theme = reactableTheme(
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Roboto, Fira Mono, Chivo, serif/*rtl:Amiri, Georgia, Times New Roman, serif*/;")),
              defaultSortOrder = "asc",
              defaultSorted = c("start_date"),
              defaultColDef = colDef(format = colFormat(digits = 1, percent = TRUE),
                                     headerClass = "header"),
              searchable = TRUE,
              minRows = 30,
              defaultPageSize = 30,
              pagination = FALSE,
              striped = TRUE))
  
  output$elo_ratings <- renderReactable(reactable(elo_ratings %>% select(-c(season, color)),
                                                  columns = list(
                                                    rank = colDef(name = "Rank",
                                                                  width = 60),
                                                    team = colDef(name = "Team"),
                                                    light = colDef(name = "",
                                                                   cell = function(value) {
                                                                     image <- htmltools::img(src = value, height = "50px", alt = "")
                                                                     htmltools::tagList(
                                                                       htmltools::div(style = list(display = "inline-block", width = "25px"), 
                                                                                      image)
                                                                     )
                                                                   }),
                                                    conference = colDef(name = "Conference"),
                                                    elo_rating = colDef(name = "Rating",
                                                                        style = function(value) {
                                                                          normalized <- (value - min(elo_ratings$elo_rating)) / (max(elo_ratings$elo_rating) - min(elo_ratings$elo_rating))
                                                                          color <- any_pal(normalized, forest_palette)
                                                                          list(background = color, "font-weight" = "bold",
                                                                               color = if_else(normalized > .7, 
                                                                                               "#ffffff", 
                                                                                               "#000000"))
                                                                        }),
                                                    week = colDef(name = "Week"),
                                                    date = colDef(name = "Date Updated",
                                                                  format = colFormat(date = TRUE)),
                                                    elo_change = colDef(name = "Δ Elo",
                                                                        style = function(value) {
                                                                          normalized <- (value - min(elo_ratings$elo_change)) / 
                                                                            (max(elo_ratings$elo_change) - min(elo_ratings$elo_change))
                                                                          color <- any_pal(normalized, green_red_pal)
                                                                          list(background = color, "font-weight" = "bold")
                                                                        }),
                                                    rank_change = colDef(name = "Δ Rank"),
                                                    result = colDef(name = "Last Game")
                                                    ),
                                                  theme = reactableTheme(
                                                    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Roboto, Fira Mono, Chivo, serif/*rtl:Amiri, Georgia, Times New Roman, serif*/;")),
                                                  defaultSortOrder = "desc",
                                                  defaultSorted = c("elo_rating"),
                                                  defaultColDef = colDef(format = colFormat(digits = 0),
                                                                         headerClass = "header"),
                                                  searchable = TRUE,
                                                  minRows = 30,
                                                  defaultPageSize = 30,
                                                  pagination = FALSE,
                                                  striped = TRUE
                                                  ))
  
  elo_plot_data <- shiny::reactive({elo_master %>% 
      filter(team %in% input$elo_plot_teams)})
  
  output$elo_plot <- shiny::renderPlot({
    ggplot(elo_plot_data(), aes(x = week, y = elo_rating)) +
      geom_line(size = 2,
                aes(color = color)) +
      geom_text(aes(label = if_else(week == elo_plot_data()$week %>% max(), team, "")),
                nudge_x = .5) +
      scale_color_identity() +
      scale_x_continuous(breaks = (0:15), labels = (0:15), limits = c(0,15)) +
      staturdays_theme +
      labs(x = "Week",
           y = "Elo Rating")
    }
    )
  
}

# Run ---------------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
