library(tidyverse)
library(showtext)
font_add_google(name = "Roboto Mono")
showtext_auto()

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
forest_palette <- c("99ddc8","95bf74","659b5e","556f44","283f3b")
mint_palette <- c("a0eec0","8ae9c1","86cd82","72a276","666b6a")
orange_pal <- function(x) grDevices::rgb(grDevices::colorRamp(c("#e6bba5", "#de703b"))(x), maxColorValue = 255)
dunkin_pal <- function(x) grDevices::rgb(grDevices::colorRamp(c("#861388", "#E6E6E9", "#de703b"))(x), maxColorValue = 255)
cool_pal <- c("FFEFC1","EBD4BB","D7BAB5","C39FB0","AF84AA","9B6AA4","874F9E")

staturdays_ramp <- function(x) rgb(colorRamp(c(staturdays_palette))(x), maxColorValue = 255)

staturdays_colors <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (staturdays_col_list)
  
  staturdays_col_list[cols]
}

staturdays_theme <- theme(plot.caption = element_text(size = 24, hjust = 1, color = staturdays_colors("orange"),
                                                      family = "Roboto Mono"), 
                          plot.title = element_text(color = staturdays_colors("dark_blue"), size = 60, face = "bold",
                                                    family = "Roboto Mono"),
                          plot.subtitle = element_text(color = staturdays_colors("light_blue"), size = 40),
                          axis.text = element_text(color = staturdays_colors("lightest_blue"), size = 28,
                                                   family = "Roboto Mono"),
                          axis.title = element_text(color = staturdays_colors("lighter_blue"), size = 32, face = "bold",
                                                    family = "Roboto Mono"),
                          legend.title = element_text(color = staturdays_colors("lighter_blue"), size = 32, face = "bold"),
                          legend.text = element_text(color = staturdays_colors("lightest_blue"), size = 28),
                          panel.grid.minor = element_blank(),
                          axis.ticks = element_line(color = "#d6d6d6"),
                          plot.background = element_rect(fill = "#cfcfcf"),
                          panel.background = element_rect(fill = "#cfcfcf"),
                          legend.background = element_rect(fill = "#cfcfcf"),
                          panel.grid = element_line(color = "#b3b3b3"),
                          legend.key = element_blank()
                          )
