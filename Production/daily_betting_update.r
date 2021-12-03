# Daily betting pull

source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/source_everything.R")

betting <- get_betting(2021,2021)

saveRDS(betting, "Data/betting_2021.rds")
