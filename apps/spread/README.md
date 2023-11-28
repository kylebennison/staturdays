Predict a team's win probability pregame.

# Set environment model
- The first time these scripts are run, you will need to set an environment variable.
- Ensure you are in in the virtual environment.
- On Windows in the `cmd` prompt, enter `set CFBD_API=YOUR_API_KEY`.
- `YOUR_API_KEY` does not need to be in quotes.
- To confirm your environment variable is set, enter `set` into your `cmd` prompt; a list of all of your environment variables will be returned.
- If your environment variable is not set, `os.getenv("CFBD_API")` will return `None`.

# Run

```bash
source .venv/Scripts/Activate
cd services/pregame-wp
```

## Train

## Predict
```bash
python preprocess.py --run_type predict
```

Then run `predict.ipynb`. Predictions will be saved to your clipboard.


# TODO

## Features
- QB games experience
- QB injured/benched (check if more than 1 QB played in previous game AND it wasn't because it was a blowout)
- Head coach games experience with team
- multiple rolling windows (4, 8, 12)
- increase training data size

### Incorporating Play Features
1. Load play-level data
2. Sum totals by game and by offense, and by game and by defense (for defensive stats)
3. Join sums with game table by home/away team
4. apply a rolling mean/sum/std., etc. to calculate game-aggregated play stats for the past N games

## Models
- how good of a model can we make without spread (and home_wp_pregame which must use spread because they're 95% correlated)?
- neural network
- cross validate train, ensure not overfitting to validation set
  - for a set of hyperparams, fit 5x and take average MAE.
  - Then, choose the hyperparams that have the lowest avg. MAE across validation sets.
