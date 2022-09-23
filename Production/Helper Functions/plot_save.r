plot_save <- function(plot = last_plot(), filename, filetype = "jpg", size = "horizontal"){
  
  #' Saves plot
  #' plot: a ggplot object
  #' filename: the name of the file. today's date will be appended to the front
  #' filetype: either "jpg" or "png"
  #' size: either "horizontal" or "square"
  
  if(missing(filename)){
    
    warning("Must provide a filename")
    
  } else {
    
    ggsave(filename = paste0(lubridate::today(),
                             "_",
                             filename,
                             ".",
                             filetype),
           plot = plot,
           path = "R Plots/",
           width = if(size == "square") 200 else 400,
           height = 200,
           units = "mm",
           dpi = 300)
    
  }
  
}
