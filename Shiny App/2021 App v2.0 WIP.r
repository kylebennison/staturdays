library(reactable)
library(tidyverse)
library(ggplot2)
library(data.table)
library(devtools)

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/source_everything.R")

colors <- data.table::fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/colors_logos.csv",
                            encoding = "UTF-8")

orange_pal <- function(x) grDevices::rgb(grDevices::colorRamp(c("#e6bba5", "#de703b"))(x), maxColorValue = 255)
dunkin_pal <- function(x) grDevices::rgb(grDevices::colorRamp(c("#861388", "#E6E6E9", "#de703b"))(x), maxColorValue = 255)

logos <- colors %>% 
  select(school, light)

elo_master <- get_elo()

elo_ratings <- elo_master %>% 
  group_by(team) %>% 
  slice_max(order_by = date, n = 1L) %>% 
  ungroup() %>% 
  mutate(rank = rank(desc(elo_rating), ties.method = "min"))

elo_ratings <- elo_ratings %>% 
  left_join(logos, by = c("team" = "school")) %>% 
  select(rank, team, light, everything())

# Calculate win probabilities this week
calendar <- get_anything("https://api.collegefootballdata.com/calendar", 2021, 2021, key = my_key)
current_week <- calendar %>% 
  filter(lastGameStart >= lubridate::today()) %>% 
  pull(week) %>% 
  min()
current_year <- max(elo_ratings$season)

games_this_week <- get_games(current_year, current_year, current_week, current_week)

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
         start_date = lubridate::with_tz(start_date, "America/New_York"))

#Bring in data for overtime sim and the overtime function
lookup_table <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/overtime_lookup_table.csv")
overtime_sim <- source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/overtime_function_only.R")

# UI ----------------------------------------------------------------------

ui <- shiny::navbarPage(title = "Staturdays",
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
                                        ),
                        
                        shiny::navbarMenu(title = "Elo",
                                          shiny::tabPanel(title = "Win Probabilities This Week",
                                                          reactable::reactableOutput(outputId = "elo_win_probs")),
                                          shiny::tabPanel(title = "Elo Ratings",
                                                          reactable::reactableOutput(outputId = "elo_ratings")),
                                          shiny::tabPanel(title = "Graph Teams",
                                                          shiny::selectizeInput(inputId = "elo_plot_teams",
                                                                                label = "Choose one or more teams to plot",
                                                                                choices = unique(elo_ratings$team),
                                                                                multiple = TRUE,
                                                                                selected = "Alabama"),
                                                          shiny::plotOutput(outputId = "elo_plot"))))

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
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
  
  output$elo_win_probs <- renderReactable(
    reactable(win_probs %>% 
                select(start_date, away_team, light_away, away_elo_wp, home_elo_wp, light_home, home_team),
              columns = list(
                start_date = colDef(name = "Start Time",
                                    format = colFormat(datetime = TRUE)),
                away_team = colDef(name = "Away"),
                light_away = colDef(name = "",
                                    cell = function(value) {
                                      image <- htmltools::img(src = value, height = "50px", alt = "")
                                      htmltools::tagList(
                                        htmltools::div(style = list(display = "inline-block", width = "25px"), 
                                                       image)
                                      )
                                    }),
                away_elo_wp = colDef(name = "Win Probability",
                                     style = function(value) {
                                       normalized <- (value) / (1)
                                       color <- dunkin_pal(normalized)
                                       list(background = color, "font-weight" = "bold")
                                     }),
                home_elo_wp = colDef(name = "Win Probability",
                                     style = function(value) {
                                       normalized <- (value) / (1)
                                       color <- dunkin_pal(normalized)
                                       list(background = color, "font-weight" = "bold")
                                     }),
                light_home = colDef(name = "",
                                    cell = function(value) {
                                      image <- htmltools::img(src = value, height = "50px", alt = "")
                                      htmltools::tagList(
                                        htmltools::div(style = list(display = "inline-block", width = "25px"), 
                                                       image)
                                      )
                                    }),
                home = colDef(name = "Home")
              ),
              theme = reactableTheme(
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Roboto, Fira Mono, Chivo, serif/*rtl:Amiri, Georgia, Times New Roman, serif*/;")),
              defaultSortOrder = "asc",
              defaultSorted = c("start_date"),
              defaultColDef = colDef(format = colFormat(digits = 1, percent = TRUE)),
              searchable = TRUE,
              minRows = 30,
              defaultPageSize = 30,
              pagination = FALSE,
              striped = TRUE))
  
  output$elo_ratings <- renderReactable(reactable(elo_ratings %>% select(-season),
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
                                                                          color <- orange_pal(normalized)
                                                                          list(background = color, "font-weight" = "bold")
                                                                        }),
                                                    week = colDef(name = "Week"),
                                                    date = colDef(name = "Date Updated",
                                                                  format = colFormat(date = TRUE))),
                                                  theme = reactableTheme(
                                                    style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif, Roboto, Fira Mono, Chivo, serif/*rtl:Amiri, Georgia, Times New Roman, serif*/;")),
                                                  defaultSortOrder = "desc",
                                                  defaultSorted = c("elo_rating"),
                                                  defaultColDef = colDef(format = colFormat(digits = 0)),
                                                  searchable = TRUE,
                                                  minRows = 30,
                                                  defaultPageSize = 30,
                                                  pagination = FALSE,
                                                  striped = TRUE
                                                  ))
  
  elo_plot_data <- shiny::reactive({elo_master %>% 
      filter(team %in% input$elo_plot_teams)})
  
  output$elo_plot <- shiny::renderPlot({
    ggplot(elo_plot_data(), aes(x = week, y = elo_rating, color = team)) +
      geom_line() +
      scale_x_continuous(breaks = (0:15), labels = (0:15), limits = c(0,15)) +
      staturdays_theme
    }
    )
  # font-family: "Chivo", "Fira Mono", serif/*rtl:Amiri, Georgia, "Times New Roman", serif*/;
}

# Run ---------------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
