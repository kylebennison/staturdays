library(rvest)
library(XML)
library(RCurl)
library(rlist)

url <- getURL("https://www.spotrac.com/nfl/cap/2021/")
text <- readHTMLTable(url)

tbl <- list.clean(text, fun = is.null, recursive = FALSE)

df <- as.data.frame(tbl)

library(stringr)
library(tidyverse)

df2 <- df %>% 
  mutate(totalcap = str_replace(NULL.Total.Cap, "[^\\$]*\\$", "")) %>% 
  mutate(totalcap = str_replace_all(totalcap, ",", "")) %>% 
  mutate(totalcap = as.integer(totalcap))

df2$totalcap %>% mean()

df2 %>% 
  mutate(costperwin = totalcap/(as.double(NULL.Win..)*17)) %>% 
  select(costperwin) %>% pull() %>% median()
