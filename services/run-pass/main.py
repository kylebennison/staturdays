""" run-pass model

Given a game-state using play-by-play data, predict run or pass.

TODO:
    - implement predict-only run

"""
import logging
import math
import pickle
import sys
from datetime import datetime
from pathlib import Path
import json
import yaml

import lightgbm as lgbm
import numpy as np
import optuna
import pandas as pd
from sklearn.metrics import accuracy_score, log_loss
from sklearn.model_selection import KFold

sys.path.append("../..")
from cfbpy import plays  # noqa: E402

logging.basicConfig()
logger = logging.getLogger(name="preprocess")
logger.setLevel(logging.DEBUG)

DEFAULT_PARAMS = {
    "reg_alpha": 2.2002764766987744,
    "reg_lambda": 0.42984378942542767,
    "colsample_bytree": 1.0,
    "subsample": 0.8,
    "bagging_freq": 3,
    "max_depth": 7,
    "num_leaves": 635,
    "min_child_samples": 39,
}


def read_data() -> pd.DataFrame:
    """Read plays data into memory as a pandas DataFrame.

    Args:
        None

    Returns:
        pandas.DataFrame: raw play-by-play data
    """
    if args.use_data:
        df = pd.read_parquet("df_raw.parquet")
    else:
        df = plays.get_plays(
            years=range(args.train_start_year, args.test_year + 1), weeks=range(1, 16)
        )
        df.to_parquet("df_raw.parquet")

    return df


def filter_plays(df) -> pd.DataFrame:
    """Filter out non-play types and kickoffs from data

    Args:
        df (pandas.DataFrame):
            A plays dataframe.

    Returns:
        pandas.DataFrame: a DataFrame with kickoffs and non-plays removed.

    """
    # Filter non-plays and kickoffs
    df = df[~df.play_type.isin(plays.NON_PLAY)]
    df = df[~df.play_type.isin(plays.KICKOFFS)]
    df = df.reset_index(drop=True)
    return df


def add_lag_features(df) -> pd.DataFrame:
    """Add lag features for `pass` and `rush` for the last 5 plays."""
    # Score/margin are post-play, so need lagging score/margin.
    # Get previous n playcalls as features
    df["pass_lag_1"] = df.groupby("offense")["pass"].shift(1, fill_value=0)
    df["pass_lag_2"] = df.groupby("offense")["pass"].shift(2, fill_value=0)
    df["pass_lag_3"] = df.groupby("offense")["pass"].shift(3, fill_value=0)
    df["pass_lag_4"] = df.groupby("offense")["pass"].shift(4, fill_value=0)
    df["pass_lag_5"] = df.groupby("offense")["pass"].shift(5, fill_value=0)
    df["rush_lag_1"] = df.groupby("offense")["rush"].shift(1, fill_value=0)
    df["rush_lag_2"] = df.groupby("offense")["rush"].shift(2, fill_value=0)
    df["rush_lag_3"] = df.groupby("offense")["rush"].shift(3, fill_value=0)
    df["rush_lag_4"] = df.groupby("offense")["rush"].shift(4, fill_value=0)
    df["rush_lag_5"] = df.groupby("offense")["rush"].shift(5, fill_value=0)
    return df


def fit_model(
    X,
    y,
    test_set: tuple | None = None,
    eval_set: tuple | None = None,
    n_estimators: int = 2000,
    tuned_params: dict | None = None,
) -> (
    tuple[lgbm.LGBMClassifier, np.float64, np.float64, pd.DataFrame, int | None]
    | lgbm.LGBMClassifier
):
    """Fit a model and optionally evaluate it on an evaluation and test set.

    Args:
        X (pandas.DataFrame):
            DataFrame of features.
        y (pandas.Series):
            Series of targets
        test_set (tuple | None):
            tuple of X and y test DataFrame and test Series
        eval_set (tuple | None):
            tuple of X and y validation DataFrame and validation Series
            (used for early stopping monitoring)
        n_estimators (int):
            Number of boosted trees to fit.
        tuned_params (dict | None):
            dict of tuned parameters to use in place of defaults.

    Returns:
        (
            tuple[lgbm.LGBMClassifier, np.float64, np.float64, pd.DataFrame, int | None]
            | lgbm.LGBMClassifier
        ):
            The model, plus test accuracy, test logloss, feature importances, and best round before stopping.

    """
    params = tuned_params or DEFAULT_PARAMS
    early_stopping = 10 if eval_set else None
    model = lgbm.LGBMClassifier(
        # constants
        boosting_type="gbdt",
        learning_rate=0.01,
        n_estimators=n_estimators,
        objective="binary",
        random_state=0,
        early_stopping_round=early_stopping,
        # tuned
        **params,
        # meta
        force_col_wise=True,
        n_jobs=-1,
        verbosity=-1,
    )

    logger.info(f"Starting model fitting for {n_estimators} iterations...")
    model.fit(
        X=X,
        y=y,
        eval_set=eval_set,
    )

    if eval_set:
        best_iter = model.best_iteration_
        logger.info(
            f"Best Iteration, Score: {best_iter, model.best_score_['valid_0']['binary_logloss']}"
        )
    else:
        best_iter = None

    # Eval
    # Feature Importance
    feat_imp = pd.DataFrame(
        {"feat": model.feature_name_, "imp": model.feature_importances_}
    ).sort_values(by="imp", ascending=False)

    if test_set:
        X_test, y_test = test_set
        y_preds = model.predict(X_test)
        y_preds = pd.Series(y_preds, name="preds")
        eval_df = pd.DataFrame(
            {"preds": y_preds, "actual": y_test[args.target].reset_index(drop=True)}
        )

        test_acc = accuracy_score(y_true=eval_df["actual"], y_pred=eval_df["preds"])
        y_preds = model.predict_proba(X_test)[:, 1]
        y_preds = pd.Series(y_preds, name="preds")
        eval_df = pd.DataFrame(
            {"preds": y_preds, "actual": y_test[args.target].reset_index(drop=True)}
        )

        test_logloss = log_loss(y_true=eval_df["actual"], y_pred=eval_df["preds"])
        logger.info(f"{test_acc =}, {test_logloss =}")

        return model, test_acc, test_logloss, feat_imp, best_iter

    else:
        return model


def log_model(model, model_name, test_acc, test_logloss, feat_imp):
    """Log model evaluation metrics as a json file"""
    eval_dict = {
        "valid_logloss": model.best_score_["valid_0"]["binary_logloss"],
        "test_logloss": test_logloss,
        "test_accuracy": test_acc,
        "top_feats": feat_imp.head(10).to_dict(),
    }

    Path("./models/").mkdir(parents=True, exist_ok=True)

    with open(f"./models/eval-{model_name}.json", "w") as f:
        json.dump(eval_dict, f)


def tune_model(X, y, X_valid, y_valid, score_to_beat):
    """Tune model for 5 folds and record the best parameters"""
    # Hyperparam Tuning
    iter = []

    def objective(
        trial,
        X=X.reset_index(drop=True),
        y=y.reset_index(drop=True),
    ):
        param = {
            "metric": "binary_logloss",
            "random_state": 0,
            "n_estimators": 2000,
            "learning_rate": 0.01,
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
            "max_depth": trial.suggest_int("max_depth", 1, 9),
            "num_leaves": trial.suggest_int("num_leaves", 2, 1000),
            "min_child_samples": trial.suggest_int("min_child_samples", 1, 50),
        }
        model = lgbm.LGBMClassifier(**param, verbosity=-1)

        kf = KFold(n_splits=5, shuffle=True, random_state=0)
        avg_logloss = []
        avg_iter = []

        for i, (train_index, test_index) in enumerate(kf.split(X=X, y=y)):
            X_test, y_test = X.loc[test_index, :], y.loc[test_index, :]
            X_train, y_train = X.loc[train_index, :], y.loc[train_index, :]

            model.fit(
                X_train,
                y_train[args.target],
                eval_set=(X_valid, y_valid[args.target]),
            )

            avg_iter.append(model.best_iteration_)

            preds = model.predict_proba(X_test)[:, 1]

            logloss = log_loss(y_true=y_test[args.target], y_pred=preds)

            avg_logloss.append(logloss)

        # Avg. results over K folds
        avg_logloss = np.mean(avg_logloss)
        avg_iter = np.mean(avg_iter)

        # Save avg. number of rounds for reference
        iter.append(avg_iter)

        return avg_logloss

    study = optuna.create_study(direction="minimize", study_name="pass-run")

    study.optimize(objective, n_trials=20)

    print("Number of finished trials:", len(study.trials))

    print("Best trial:", study.best_trial.params)
    assert score_to_beat > study.best_value
    best_iter = round(iter[study.best_trial.number])

    return study.best_trial.params, study.best_value, best_iter


def save_model(model, model_name):
    """Save model object as a pickle"""
    model_path = f"./models/{model_name}.pickle"
    with open(model_path, "wb") as f:
        pickle.dump(model, f)

    logger.info(f"Model saved to {model_path}.")


def update_predict_config(model_name, new_logloss, target):
    """Check if new metrics are better than the current best model, and if so, update metrics in config file."""
    with open("./predict_config.yaml", "r") as f:
        predict_config = yaml.safe_load(f)

    if new_logloss < predict_config[f"test_logloss_{target}"]:
        predict_config[f"test_logloss_{target}"] = float(
            new_logloss
        )  # convert to float or yaml throws an error since it's originally np.float64
        predict_config[f"model_name_{target}"] = model_name

        with open("./predict_config.yaml", "w") as f:
            yaml.safe_dump(predict_config, f)

    else:
        logger.info(
            f"{new_logloss = }, old logloss = {predict_config[f'test_logloss_{target}']}. Keeping existing model."
        )


def main():
    logger.info("Starting main.")
    df = read_data()
    df = plays.add_features(df)

    df = filter_plays(df)

    df = add_lag_features(df)

    X = df[
        [
            "year",
            "week",
            "presnap_offense_score",
            "presnap_defense_score",
            "presnap_offense_margin",
            "pass_lag_1",
            "pass_lag_2",
            "pass_lag_3",
            "pass_lag_4",
            "pass_lag_5",
            "rush_lag_1",
            "rush_lag_2",
            "rush_lag_3",
            "rush_lag_4",
            "rush_lag_5",
            "down",
            "distance",
            "yards_to_goal",
            "period",
            "offense_timeouts",
            "defense_timeouts",
            "clock_in_seconds",
            "pass",
            "rush",
        ]
    ]
    X = X.dropna()

    # Shuffle data
    X = X.sample(frac=1)

    # Split train/test
    X_train = X[X["year"] < args.test_year]
    X_test = X[X["year"] == args.test_year]

    # Set train columns
    TRAIN_COLS = [
        "presnap_offense_margin",
        "presnap_offense_score",
        "presnap_defense_score",
        "down",
        "distance",
        "yards_to_goal",
        "period",
        "offense_timeouts",
        "defense_timeouts",
        "clock_in_seconds",
    ]
    # Split x/y
    y_train, y_test = X_train[["pass", "rush"]], X_test[["pass", "rush"]]
    X_train = X_train[TRAIN_COLS]
    X_test = X_test[TRAIN_COLS]

    # Create validation set
    train_rows = math.floor(len(X_train) * 0.75)
    X_train, X_valid = X_train.iloc[0:train_rows, :], X_train.iloc[train_rows:, :]
    y_train, y_valid = y_train.iloc[0:train_rows, :], y_train.iloc[train_rows:, :]
    # Training
    # Model

    # Fit an initial model
    model, test_acc, test_logloss, feat_imp, best_iter = fit_model(
        X=X_train,
        y=y_train[args.target],
        test_set=(X_test, y_test),
        eval_set=(X_valid, y_valid[args.target]),
    )
    # Log Model Results

    date = format(datetime.today(), "%Y-%m-%d-%H-%M")
    model_name = f"{args.target}-model-{date}"

    log_model(
        model=model,
        model_name=model_name,
        test_acc=test_acc,
        test_logloss=test_logloss,
        feat_imp=feat_imp,
    )

    update_predict_config(
        model_name=model_name, new_logloss=test_logloss, target=args.target
    )

    if args.do_tune:
        best_params, best_value, best_iter = tune_model(
            X=X_train,
            y=y_train,
            X_valid=X_valid,
            y_valid=y_valid,
            score_to_beat=test_logloss,
        )
        # Retrain with updated hyperparams
        # Model
        model, new_acc, new_logloss, new_feat_imp, best_iter = fit_model(
            X=X_train,
            y=y_train[args.target],
            test_set=(X_test, y_test),
            eval_set=(X_valid, y_valid[args.target]),
            tuned_params=best_params,
        )

        # Confirm test results indeed improved
        # Score to beat
        model_improved = new_logloss < test_logloss
        if not model_improved:
            logger.warning("Model did not improve with tuned hyperparameters!")
        else:
            log_model(
                model=model,
                model_name=f"tuned-{model_name}",
                test_acc=new_acc,
                test_logloss=new_logloss,
                feat_imp=new_feat_imp,
            )
            update_predict_config(
                model_name=model_name, new_logloss=new_logloss, target=args.target
            )
    # New results
    # Retrain Model on Full Data
    # Join train, valid, and test
    X = pd.concat([X_train, X_valid, X_test], axis=0)
    y = pd.concat([y_train, y_valid, y_test], axis=0)
    # Train
    # Model
    logger.info("Retraining on full dataset.")
    params = best_params if args.do_tune else DEFAULT_PARAMS
    model = fit_model(
        X=X, y=y[args.target], n_estimators=best_iter, tuned_params=params
    )

    # Save Model
    save_model(model=model, model_name=model_name)

    logger.info("Done.")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()

    parser.add_argument("--run_type", type=str, default="predict")
    parser.add_argument("--train_start_year", type=int, default=2020)
    parser.add_argument("--test_year", type=int, default=2022)
    parser.add_argument("--predict_year", type=int, default=2023)
    parser.add_argument("--do_tune", type=bool, default=False)
    parser.add_argument("--use_data", type=bool, default=True)
    parser.add_argument("--target", type=str, choices=["pass", "rush"])

    args = parser.parse_args()
    logger.info(f"{args=}")
    main()
