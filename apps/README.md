# Apps

This directory holds standalone applications for staturdays CFB.

They are defined below:

1. win-probability: Model for predicting a team's pregame and realtime (play-by-play) win probability.
2. in-game-wp: play-by-play win probability model
3. fantasy-points: predict yearly and weekly NFL fantasy points
4. nfl-scores: app for displaying live nfl scores compactly
5. over-under: model for predicting the total
6. spread: model for predicting the spread.

Future Services:
1. expected-value: use the wp, spread, and o/u models to identify favorable odds on games.

# TODO:
- move preprocess for all similar pregame models (wp, spread, O/U) to a shared util, then split train by application.
