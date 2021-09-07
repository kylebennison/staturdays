# Single source file which sources all other useful source files at once

# API wrapper
source("Production/cfbd_api_key_function.R")
# Staturdays themes + colors
source("Production/Staturdays Colors and Theme.R")
# Games
source("Production/get_games_api.R")
# Plays
source("Production/get_plays_api_v2.0.r")
# Drives
source("Production/get_drives_api.R")
# Elo Ratings
source("Production/get_elo.r")
# Add Success and Other Features
source("Production/add_success_features_v1.0.r")
# Play Types and Conferences
source("Production/Play Types and Power Conference Names.R")
# Get team colors, names, logos, etc.
source("Production/get_colors_api.r")
# Get anything else
source("Production/get_anything.r")
