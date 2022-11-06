## Libraries
library(tidyverse) # all the things
library(ggExtra)   # marginal plots
library(ggtext)    # color your text
library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
library(scales)    # helper functions from ggplot2

## Parameters ----------------
# pass-tracker file name
data_file_name <- "psu_iowa.csv"
# Output file name
completion_file_name <- "completion.png"
# Team
team <- "Penn State"
# What to do with other passes, I think of grouped as accurate passes or not the QB's fault
# "Grouped": Completions, Drops, DPI, Pass breakup are counted as complete
#            Incompletions, OPI, interceptions are counted as incomplete
# "Filtered": Filters the passes to just completions and incompletions
pass_categories <- "Grouped"

# Plot Text
# Color scheme: one of "white","black","UCLA"
color_scheme <- "Penn State"
## ---------------------


## 
back_col <- case_when(color_scheme == "white" ~ "white",
                      color_scheme == "black" ~ "black",
                      color_scheme == "Penn State" ~ "#041E42",
                      TRUE ~ "white")
front_col <- case_when(color_scheme == "white" ~ "black",
                       color_scheme == "black" ~ "white",
                       color_scheme == "Penn State" ~ "#D3D3D3",
                       TRUE ~ "black")

heat_colors <- grDevices::colorRampPalette(c("#800026FF", "#FC4E2AFF", "#FEB24CFF", "#FFFFCCFF"))(13)

heat_palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1)

heat_colors_interpolated <- colorRampPalette(paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = -1))(13)

add_field <- function(back_col = "white",
                      front_col = "black") {
  not_div_5 <- function(x) {
    # select only elements of the vector not divisible by 5
    x[x %% 5 != 0]
  }
  
  center_df <- tibble(
    #x_coord = c(rep(-3.1, 60), rep(3.1, 60)),
    x_coord = c(rep(-20/3, 60), rep(20/3, 60)),
    y_coord = seq(-14, 59, 1) %>% rep(2) %>% not_div_5(),
    text = "--"
  )
  
  # line labels
  annotate_df <- tibble(
    x_coord = c(17.88, -17.88) %>% rep(each = 5),
    y_coord = seq(10, 50, 10) %>% rep(2),
    text = seq(10, 50, 10) %>% rep(2) %>% str_replace("(.)(.)", "\\1 \\2"),
    rotation = c(90, 270) %>% rep(each = 5)
  )
  
  # yardlines
  yardline_df <- tibble(
    y = seq(-15, 60, 5),
    yend = seq(-15, 60, 5),
    x = rep(-56 / 2, 16),
    xend = rep(56 / 2, 16)
  )
  
  # sidelines
  sideline_df <- tibble(
    y = c(-15.15, -15.15),
    yend = c(60.15, 60.15),
    x = c(-56 / 2, 56 / 2),
    xend = c(-56 / 2, 56 / 2)
  )
  list(
    coord_cartesian(
      xlim = c(-53.333 / 2, 53.333 / 2),
      ylim = c(-15, 60)
    ),
    geom_text(
      data = annotate_df, aes(label = text, angle = rotation),
      color = front_col, size = 8
    ),
    geom_segment(
      data = yardline_df, color = front_col, size = 1,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_segment(
      x = -56 / 2, y = 0, xend = 56 / 2, yend = 0,
      color = "blue", size = 1, alpha = 0.5
    ),
    geom_segment(
      data = sideline_df, color = front_col, size = 2,
      aes(x = x, y = y, xend = xend, yend = yend)
    ),
    geom_text(
      data = center_df,
      aes(label = text), color = front_col, vjust = 0.32
    ),
    theme_void(),
    theme(
      strip.text = element_text(size = 20, color = front_col),
      plot.background = element_rect(fill = back_col, color = NA),
      legend.position = "none",
      plot.margin = unit(c(2, 1, 0.5, 1), unit = "cm"),
      plot.caption = element_text(color = front_col),
      plot.title = element_text(color = front_col),
      plot.subtitle = element_text(color = front_col),
      panel.background = element_rect(fill = back_col, color = NA),
      panel.border = element_blank()
    )
  )
}

pass_tracker <- read_csv("C:/Users/drewb/Downloads/psu_iowa.csv")
heatmap_file_name <- "levis_completions_heatmap.png"
title <- "Will Levis - Penn State QB"
subtitle <- "Pass Completions against Iowa"
png(heatmap_file_name, width = 6, height = 8, units = "in",res = 300)

pass_tracker %>% 
  filter(qb == "Will Levis", result == "Complete") %>% 
  ggplot(aes(x_coord,y_coord)) +
  geom_density_2d_filled(aes(color = ..level..,fill = ..level..),
                         contour_var = "ndensity", # normalize across facets
                         breaks = seq(0.1, 1.0, length.out = 13)) +
  add_field(back_col = back_col,front_col = front_col) +
  scale_fill_manual(values = c(heat_colors_interpolated), aesthetics = c("fill", "color")) +
  labs(title = title,subtitle = subtitle)+
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5))

dev.off()






















qb_density_compare <- function(pass_df, qb1_name, qb2_name, n = 200){
  pass_df <- pass_df %>% 
    rename(name = result)
  # filter to qb1
  qb1 <- pass_df %>% 
    select(x_coord, y_coord, name) %>% 
    filter(str_detect(name, qb1_name))
  
  #filter to qb2
  qb2 <- pass_df %>% 
    select(x_coord, y_coord, name) %>% 
    filter(str_detect(name, qb2_name))
  
  # get x/y coords as vectors
  qb1_x <- pull(qb1, x_coord)
  qb1_y <- pull(qb1, y_coord)
  
  # get x/y coords as vectors
  qb2_x <- pull(qb2, x_coord)
  qb2_y <- pull(qb2, y_coord)
  
  # get x and y range to compute comparisons across
  x_rng = c(-28,28)#range(c(qb1_x, qb2_x))
  y_rng = c(-15,55)#range(c(qb1_y, qb2_y)) 
  
  # Explicitly calculate bandwidth for future use
  bandwidth_x <- MASS::bandwidth.nrd(c(qb1_x, qb2_x))
  bandwidth_y <- MASS::bandwidth.nrd(c(qb1_y, qb2_y))
  
  bandwidth_calc <- c(bandwidth_x, bandwidth_y)
  
  # Calculate the 2d density estimate over the common range
  d2_qb1 = MASS::kde2d(qb1_x, qb1_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  d2_qb2 = MASS::kde2d(qb2_x, qb2_y, h = bandwidth_calc, n=n, lims=c(x_rng, y_rng))
  
  # create diff df
  qb_diff <- d2_qb1
  
  # matrix subtraction density from qb2 from qb1
  qb_diff$z <- d2_qb1$z - d2_qb2$z
  
  # add matrix col names
  colnames(qb_diff$z) = qb_diff$y
  
  #### return tidy tibble ####
  qb_diff$z %>% 
    # each col_name is actually the y_coord from the matrix
    as_tibble() %>% 
    # add back the x_coord
    mutate(x_coord= qb_diff$x) %>% 
    pivot_longer(-x_coord, names_to = "y_coord", values_to = "z") %>% 
    mutate(y_coord = as.double(y_coord),
           bandwidth = list(bandwidth_calc),
           comparison = glue::glue("{qb1_name} (QB1) vs {qb2_name} (QB2)"))
  
}

completion_names <- case_when(pass_categories == "Grouped" ~ c("Complete","Drop","Pass Breakup","DPI"),
                              pass_categories == "Filtered" ~ "Complete",
                              TRUE ~ "Complete")
incompletion_names <- case_when(pass_categories == "Grouped" ~ c("Incomplete","Interception","OPI"),
                              pass_categories == "Filtered" ~ "Incomplete",
                              TRUE ~ "Incomplete")

png(completion_file_name, width = 6, height = 8, units = "in",res = 300)
pass_tracker %>% 
  mutate(result = case_when(result %in% completion_names ~ "Complete",
                            result %in% incompletion_names ~ "Incomplete")) %>% 
  filter(team == team) %>% 
  qb_density_compare("Complete", "Incomplete", n = 200) %>% 
  ggplot(aes(x_coord, y_coord)) +
  
  # add core heatmap - note that geom_raster or geom_tile both work
  geom_raster(aes(x_coord, y_coord, fill=z))  +
  
  # add contour polygon lines around the most dense points
  stat_contour(aes(color=..level.., z = z)) +
  
  # add a fill gradient from low (blue) to high (red) 
  # with white as the zero midpoint
  scale_fill_gradient2(low="blue",mid="white", high="red", midpoint=0) +
  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  # drop the legends
  guides(color=FALSE, fill = FALSE) +
  add_field() +
  labs(title = title,
       subtitle = "Color is more <span style='color:red'>**Completions**</span> or more <span style='color:blue'>**Incompletions**</span>") +
  # add some customizations to the plot
  theme(legend.position = "top", legend.key.width = unit(2, "cm"),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5),
        plot.caption = element_text(face = "bold")) 
dev.off()