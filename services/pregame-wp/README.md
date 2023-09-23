Predict a team's win probability pregame.

# TODO

## Features
- QB games experience
- QB injured/benched (check if more than 1 QB played in previous game AND it wasn't because it was a blowout)
- Head coach games experience with team
- multiple rolling windows (4, 8, 12)
- increase training data size

## Models
- how good of a model can we make without spread?
- neural network
- cross validate train, ensure not overfitting to validation set
  - for a set of hyperparams, fit 5x and take average MAE.
  - Then, choose the hyperparams that have the lowest avg. MAE across validation sets.