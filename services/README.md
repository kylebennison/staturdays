This directory holds standalone services for staturdays CFB.

They are defined below:

1. pregame-wp: Model for predicting a team's pregame win probability.

Future Services:
1. spread-pred: predict the spread of a game.
2. over-under: predict the total score of the game.
3. expected-value: use the wp, spread, and o/u models to identify favorable odds on games.

# TODO:
- move preprocess for all similar pregame models (wp, spread, O/U) to a shared util, then split train by service.
