# Daily betting pull

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/source_everything.R")

betting <- get_betting(2021,2021)

setwd("C:/Users/Kyle/Documents/Kyle/Staturdays/Staturdays Github/Github/staturdays")

saveRDS(betting, "Data/betting_2021.rds")
