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

# Read in tables
explosive <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Shiny%20App/csvs/explosive_summary.csv")
field_pos <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Shiny%20App/csvs/field_pos.csv")
pass_rate_by_down <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Shiny%20App/csvs/pass_rate_by_down.csv")
pass_rate_vs_avg_by_down <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Shiny%20App/csvs/pass_rate_vs_avg_by_down.csv")
succ_rate <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Shiny%20App/csvs/succ_rate.csv")
turnover_yds <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Shiny%20App/csvs/turnover_yds.csv")
yards_per_att_joined <- fread("https://raw.githubusercontent.com/kylebennison/staturdays/master/Shiny%20App/csvs/yards_per_att_joined.csv")

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

staturdays_palette <- c("#5c6272", "#ffffff", "#de703b")

staturdays_ramp <- function(x) rgb(colorRamp(c(staturdays_palette))(x), maxColorValue = 255)

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

# UI ----------------------------------------------------------------------

ui <- navbarPage(title = "Staturdays | CFB Stats and Analysis",
                 tabPanel(title = "Team Plots",
                          sidebarLayout(
                            sidebarPanel(
                              selectizeInput(inputId = "conference", label = "Choose which conferences to plot", 
                                             choices = unique(succ_rate$team_conference),
                                             selected = "Big Ten",
                                             multiple = T)
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
                 navbarMenu("Competitive Tendencies",
                   tabPanel(title = "Pass Rate by Situation",
                            reactableOutput(outputId = "pass_rate")),
                   tabPanel(title = "Down and Distance",
                            fluidRow(selectizeInput(inputId = "down", label = "Select a down", choices = c(1L, 2L, 3L, 4L), selected = 1L, multiple = T,
                                                    options = list(placeholder = "Select a down"))),
                            fluidRow(reactableOutput(outputId = "pass_rate_by_down"))
                            )
                 )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Summary Stats
  
  ## Success Rate
  # Offense
  success_rate_offense <- reactive({
    succ_rate %>% 
      filter(stat == "offense_success_rate", team_conference %in% c(input$conference)) %>% 
      group_by(down) %>% 
      mutate(rank = rank(desc(succ_rate), ties.method = "min"))
  }
  )
  # Defense
  success_rate_defense <- reactive({
    succ_rate %>% 
      filter(stat == "defense_success_rate", team_conference %in% c(input$conference)) %>% 
      group_by(down) %>% 
      mutate(rank = rank(succ_rate, ties.method = "min"))
  }
  )
  
  # Explosiveness
  explosiveness <- reactive({
    explosive %>% 
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
  
  # Pass Rate by Down 
  pass_rate_by_down_tbl <- reactive({
    pass_rate_by_down %>% 
      pivot_longer(cols = matches("_[0-9]"), names_to = "down", values_to = "test") %>% 
      mutate(down_num = str_extract(down, "[0-9]")) %>% 
      mutate(down = str_sub(down, start = 1L, end = -3L)) %>% 
      pivot_wider(names_from = down, values_from = test) %>%
      filter(down_num %in% input$down)
  })

# Plots -------------------------------------------------------------------
  
  # Success Rate Plot - OFF
  output$success_rate_off <- renderPlot({
    success_rate_offense() %>% 
      group_by(team) %>% 
      ggplot(aes(x = rank, y = succ_rate, fill = color)) +
      geom_col(position = "dodge") +
      geom_image(aes(image = light), size = .1, by = "width", asp = 2, nudge_y = .01) +
      theme(aspect.ratio = 1/2) +
      facet_wrap(vars(down), nrow = 4) +
      scale_x_reverse(breaks = seq(1:max(success_rate_offense()$rank))) +
      scale_fill_identity() +
      geom_label(aes(label = play_count), nudge_y = -.25, size = 3, fill = "white") +
      labs(title = paste0(c(input$conference)," \nSuccess Rate - ", year(today())), # need to have a table that stores metadata about week and year that data is through
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
      group_by(team) %>% 
      ggplot(aes(x = rank, y = succ_rate, fill = color)) +
      geom_col(position = "dodge") +
      geom_image(aes(image = light), size = .1, by = "width", asp = 2, nudge_y = .01) +
      theme(aspect.ratio = 1/2) +
      facet_wrap(vars(down), nrow = 4) +
      scale_x_reverse(breaks = seq(1:max(success_rate_defense()$rank))) +
      scale_fill_identity() +
      geom_label(aes(label = play_count), nudge_y = -.25, size = 3, fill = "white") +
      labs(title = paste0(input$conference," \nSuccess Rate - ", year(today())),
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
           x = paste0("Explosive Pass Rate (>= ", "20"," yds)"), # Need this data stored in a csv, replacing with 20 for now
           y = paste0("Explosive Rush Rate (>= ", "12"," yds)")) + # Need this data stored in a csv, replacing with 12 for now
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

  # Pass Rate by Situation
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
                pass_vs_avg_standard = colDef(name = "Diff. vs. Avg.", format = colFormat(percent = T, digits = 1),
                                              style = function(value) {
                                                normalized <- ((value - min(pass_rate_vs_avg_by_down$pass_vs_avg_standard)) / (max(pass_rate_vs_avg_by_down$pass_vs_avg_standard) - min(pass_rate_vs_avg_by_down$pass_vs_avg_standard)))
                                                color <- staturdays_ramp(normalized)
                                                list(background = color, color = "black")
                                              }), 
                pass_rate_passing_downs = colDef(name = "Pass Rate", format = colFormat(percent = T, digits = 1)), 
                cfb_pass_passing = colDef(name = "CFB Avg.", format = colFormat(percent = T, digits = 1)),
                pass_vs_avg_passing = colDef(name = "Diff vs. Avg.", format = colFormat(percent = T, digits = 1), 
                                             style = function(value) {
                                               normalized <- ((value - min(pass_rate_vs_avg_by_down$pass_vs_avg_passing)) / (max(pass_rate_vs_avg_by_down$pass_vs_avg_passing) - min(pass_rate_vs_avg_by_down$pass_vs_avg_passing)))
                                               color <- staturdays_ramp(normalized)
                                               list(background = color, color = "black")
                                             })),
              searchable = T,
              pagination = F,
              highlight = T)
    })
  
  output$pass_rate_by_down <- renderReactable({
    reactable(pass_rate_by_down_tbl(),
              columns = list(
                pass_rate = colDef(name = "Pass Rate"),
                avg_distance = colDef(name = "Avg. Distance to Go")
              ),
              searchable = T,
              pagination = F,
              highlight = T
              )
  })
  
}

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
