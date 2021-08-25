# Single source file which sources all other useful source files at once

# API wrapper
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/cfbd_api_key_function.R")
# Staturdays themes + colors
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Staturdays%20Colors%20and%20Theme.R")
# Games
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/get_games_api.R")
# Plays
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/get_plays_and_add_success_features_api.R")
# Drives
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/get_drives_api.R")
# Play Types and Conferences
source("https://raw.githubusercontent.com/kylebennison/staturdays/master/Production/Play%20Types%20and%20Power%20Conference%20Names.R")
