source("Production/source_everything.r")

colors <- get_colors()

orange_pal <- function(x) grDevices::rgb(grDevices::colorRamp(c("#e6bba5", "#de703b"))(x), maxColorValue = 255)

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


# UI ----------------------------------------------------------------------

ui <- shiny::navbarPage(title = "Staturdays",
                        shiny::tabPanel(title = "Overtime Simulator"),
                        shiny::navbarMenu(title = "Elo",
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
