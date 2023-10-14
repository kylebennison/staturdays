"""

Ref: https://blog.collegefootballdata.com/talking-tech-building-an-artifical-neural-network-to/

"""
import cfbd
import numpy as np
import pandas as pd
import os
import lightgbm as lgbm
from sklearn.model_selection import train_test_split, KFold, cross_validate
import optuna
import pickle
from datetime import datetime

from sklearn.decomposition import TruncatedSVD
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline

import statistics

X_train, X_test, y_train, y_test = (
    pd.read_parquet(path="./data/X_train.parquet"),
    pd.read_parquet(path="./data/X_test.parquet"),
    pd.read_parquet(path="./data/y_train.parquet"),
    pd.read_parquet(path="./data/y_test.parquet"),
)

# variables to remove from training data
exclude = [
    "id",
    "season",
    "year",
    "season_type",
    "start_date",
    "completed",
    "home_ml",
    "away_ml",
    "spread_open",
    "over_under_open",
    "team_home",
    "team_away",
    "home_post_wp",
    "away_post_wp",
    "home_team",
    "away_team",
    "home_points",
    "away_points",
    # "srs_rating_home",
    # "srs_ranking_home",
    # "srs_rating_away",
    # "srs_ranking_away",
]

keep = [
       "home_elo",
       "away_elo",
       "spread",
       "home_wp_pregame"
       ]

store_mae = []
for fold in range(5):
    X_tr, X_v, y_tr, y_v = train_test_split(X_train[keep], y_train, test_size=0.2)

    # Model
    lgbm_reg = lgbm.LGBMRegressor(
        # constants
        boosting_type="gbdt",
        learning_rate=0.001,
        n_estimators=10000,
        objective="mae",
        early_stopping_round=10,
        # tuned
        reg_alpha=3.9740293140705107,
        reg_lambda=1.881540488130314,
        colsample_bytree=1.0,  # percent of features to randomly select for use on each tree
        subsample=0.6,  # percent of data to sample without resampling. only works if bagging_freq is also set
        bagging_freq=1,  # bag by subsample every kth tree
        max_depth=4,
        num_leaves=873,  # should be less than 2^max_depth. Should be significantly less to avoid overfitting
        min_child_samples=42,
        min_data_per_groups=92,
        # meta
        force_col_wise=True,
    )
    
    lgbm_reg.fit(
        X=X_tr,
        y=y_tr,
        eval_set=(X_v, y_v),
    )
    
    y_preds = lgbm_reg.predict(X_v)

    y_preds = pd.Series(y_preds, name="preds")

    eval_df = pd.DataFrame(
        {"preds": y_preds, "actual": y_v["margin"].reset_index(drop=True)}
    )
    eval_df["mae"] = abs(eval_df["preds"] - eval_df["actual"])
    store_mae.append(eval_df["mae"].mean())
    
print("Average MAE: ", statistics.mean(store_mae))
    

feat_imp = pd.DataFrame(
    {"feat": lgbm_reg.feature_name_, "imp": lgbm_reg.feature_importances_}
).sort_values(by="imp", ascending=False)

feat_imp.plot(x="feat", y="imp", kind="barh", figsize=(5, 15))



import matplotlib.pyplot as plt
import numpy as np


def abline(slope, intercept):
    """Plot a line from slope and intercept"""
    axes = plt.gca()
    x_vals = np.array(axes.get_xlim())
    y_vals = intercept + slope * x_vals
    plt.plot(x_vals, y_vals, "--")


eval_df.plot(x="preds", y="actual", kind="scatter")
abline(1, 0)

eval_df.assign(bucket=round(eval_df["preds"], 0)).groupby("bucket")["actual"].agg(
    ["mean", "count"]
).query("count > 10")["mean"].plot()

eval_df["spread"] = X_test.spread.reset_index(drop=True)
eval_df["spread_mae"] = abs(eval_df["spread"] - eval_df["actual"])
eval_df.filter(like="mae").describe()

straight_up = (np.sign(eval_df["preds"]) == np.sign(eval_df["actual"])).sum() / len(
    eval_df
)

straight_up_vs_spread = straight_up - (
    (np.sign(eval_df["spread"]) == np.sign(eval_df["actual"])).sum() / len(eval_df)
)

ats = (
    abs(eval_df["preds"] - eval_df["actual"])
    < abs(eval_df["spread"] - eval_df["actual"])
).sum() / len(eval_df["spread"].dropna())



date = format(datetime.today(), "%Y-%m-%d-%H-%M")
model_name = f"model-{date}"

eval_dict = {
    "best_mae": lgbm_reg.best_score_["valid_0"]["l1"],
    "ats": ats,
    "straight_up": straight_up,
    "straight_up_vs_spread": straight_up_vs_spread,
    "top_feats": feat_imp.head(10),
}

eval_dict


with open(f"./models/eval-{model_name}.txt", "w") as f:
    f.write(str(eval_dict))


    iter = []

from sklearn.metrics import mean_absolute_error


def objective(trial, X=X_train, y=y_train, data=None, target=None):
    param = {
        "metric": "mae",
        "random_state": 0,
        "n_estimators": 10000,
        "learning_rate": 0.001,
        "subsample_freq": 1,
        "early_stopping_rounds": 10,
        "force_col_wise": True,
        "reg_alpha": trial.suggest_float("reg_alpha", 1e-3, 10.0, log=True),
        "reg_lambda": trial.suggest_float("reg_lambda", 1e-3, 10.0, log=True),
        "colsample_bytree": trial.suggest_categorical(
            "colsample_bytree", [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
        ),
        "subsample": trial.suggest_categorical(
            "subsample", [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]
        ),
        "bagging_freq": trial.suggest_categorical("bagging_freq", [1, 2, 3, 5, 10]),
        "max_depth": trial.suggest_int("max_depth", 1, 6),
        "num_leaves": trial.suggest_int("num_leaves", 2, 1000),
        "min_child_samples": trial.suggest_int("min_child_samples", 1, 50),
        "min_data_per_group": trial.suggest_int("min_data_per_group", 1, 100),
        "cat_smooth": trial.suggest_float("cat_smooth", 1e-3, 25),
    }
    model = lgbm.LGBMRegressor(**param, verbosity=-1)

    kf = KFold(n_splits=5, shuffle=True, random_state=0)
    avg_mae = []
    avg_iter = []

    for i, (train_index, test_index) in enumerate(kf.split(X=X, y=y)):
        X_test, y_test = X.loc[test_index, :], y.loc[test_index, :]
        X_train, y_train = X.loc[train_index, :], y.loc[train_index, :]

        model.fit(
            X_train.drop(columns=exclude),
            y_train,
            eval_set=[(X_test.drop(columns=exclude), y_test)],
        )

        avg_iter.append(model.best_iteration_)

        preds = model.predict(X_test.drop(columns=exclude))

        mae = mean_absolute_error(y_true=y_test, y_pred=preds)

        avg_mae.append(mae)

    # Avg. results over K folds
    avg_mae = np.mean(avg_mae)
    avg_iter = np.mean(avg_iter)

    # Save avg. number of rounds for reference
    iter.append(avg_iter)

    return avg_mae


study = optuna.create_study(direction="minimize")
study.optimize(objective, n_trials=25)
print("Number of finished trials:", len(study.trials))
print("Best trial:", study.best_trial.params)

study.best_value

best_iter = iter[study.best_trial.number]
best_iter

study.best_params

# Model
lgbm_reg = lgbm.LGBMRegressor(
    # constants
    boosting_type="gbdt",
    learning_rate=0.001,
    n_estimators=10000,
    objective="mae",
    random_state=0,
    early_stopping_round=10,
    # tuned
    **study.best_params,
    # meta
    force_col_wise=True,
)

lgbm_reg.fit(
    X=X_tr.drop(columns=exclude),
    y=y_tr,
    eval_set=(X_v.drop(columns=exclude), y_v),
)


eval_df["mae"].describe()

y_preds = lgbm_reg.predict(X_test.drop(columns=exclude))
y_preds = pd.Series(y_preds, name="preds")
eval_df = pd.DataFrame(
    {"preds": y_preds, "actual": y_test["margin"].reset_index(drop=True)}
)
eval_df["mae"] = abs(eval_df["preds"] - eval_df["actual"])
eval_df["mae"].describe()

# Join train, valid, and test
X = pd.concat([X_train, X_test], axis=0)
y = pd.concat([y_train, y_test], axis=0)

# Train
# Model
lgbm_reg = lgbm.LGBMRegressor(
    # constants
    boosting_type="gbdt",
    learning_rate=0.001,
    n_estimators=round(best_iter),
    objective="mae",
    random_state=0,
    # tuned
    **study.best_params,
    # meta
    force_col_wise=True,
)

lgbm_reg.fit(
    X=X.drop(columns=exclude),
    y=y,
)

with open(f"./models/{model_name}.pickle", "wb") as f:
    pickle.dump(lgbm_reg, f)