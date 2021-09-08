source("Production/source_everything.r")

elo_ratings <- get_elo()


# UI ----------------------------------------------------------------------

ui <- shiny::navbarPage(title = "Staturdays",
                        shiny::tabPanel(title = "Overtime Simulator"),
                        shiny::tabPanel(title = "Elo Ratings",
                                        reactable::reactableOutput(outputId = "elo_ratings")))

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  output$elo_ratings <- renderReactable(reactable(elo_ratings))
  # font-family: "Chivo", "Fira Mono", serif/*rtl:Amiri, Georgia, "Times New Roman", serif*/;
}

# Run ---------------------------------------------------------------------

shiny::shinyApp(ui = ui, server = server)
