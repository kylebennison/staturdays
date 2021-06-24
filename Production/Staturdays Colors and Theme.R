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
                          panel.grid = element_blank(), # no gridlines. If you want them, use element_line()
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_line(color = "#d6d6d6")
)
