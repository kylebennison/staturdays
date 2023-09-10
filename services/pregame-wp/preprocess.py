"""

Ref: https://blog.collegefootballdata.com/talking-tech-building-an-artifical-neural-network-to/

"""
import cfbd
import numpy as np
import pandas as pd
import os
from sklearn.model_selection import train_test_split
from python.cfbd_api_utils import games, offseason

import logging

logging.basicConfig()
logger = logging.getLogger(name="preprocess")
logger.setLevel(logging.INFO)

configuration = cfbd.Configuration()
configuration.api_key["Authorization"] = os.getenv("CFBD_API")
configuration.api_key_prefix["Authorization"] = "Bearer"

api_config = cfbd.ApiClient(configuration)
teams_api = cfbd.TeamsApi(api_config)
ratings_api = cfbd.RatingsApi(api_config)
games_api = cfbd.GamesApi(api_config)
stats_api = cfbd.StatsApi(api_config)
betting_api = cfbd.BettingApi(api_config)
player_api = cfbd.PlayersApi(api_config)
run_type = "predict"


def main(
    run_type: str,
):
    logger.info("Starting main.")
    if run_type == "train":
        df = games.get_games([range(2015, 2023)])
    if run_type == "predict":
        df = games.get_games(2023)

    # Define feature groupings
    excluded = [
        "id",
        "year",
        "week",
        "home_team",
        "away_team",
        "margin",
        "home_points",
        "away_points",
    ]
    cat_features = ["home_conference", "away_conference", "neutral_site"]
    cont_features = [
        c for c in df.columns.to_list() if c not in cat_features and c not in excluded
    ]
    logger.debug(f"{cont_features = }")
    target = ["margin"]

    power_5 = ["Big Ten", "ACC", "SEC", "Big 12", "Pac-12"]

    df["home_conference"] = np.where(
        df["home_conference"].isin(power_5), df["home_conference"], "Other"
    )
    df["away_conference"] = np.where(
        df["away_conference"].isin(power_5), df["away_conference"], "Other"
    )
    # Clean up features
    cat_df = pd.get_dummies(df[cat_features], drop_first=True)
    df = pd.concat([df, cat_df], axis=1)
    df = df.drop(columns=cat_features)

    # Returning
    returning_df = offseason.get_returning(2023)
    returning_df = returning_df.fillna(returning_df.mean(numeric_only=True))
    df = pd.merge(
        df,
        returning_df,
        how="left",
        left_on=["year", "home_team"],
        right_on=["year", "team"],
    )
    df = pd.merge(
        df,
        returning_df,
        how="left",
        left_on=["year", "away_team"],
        right_on=["year", "team"],
        suffixes=["_home", "_away"],
    )

    # Clean column names
    df.columns = [c.lower().replace(" ", "_").replace("-", "_") for c in df.columns]
    test_year = df.year.max()
    test_df = df.query(f"year == {test_year}")
    train_df = df.query(f"year != {test_year}")

    if run_type == "train":
        X_train, y_train = (
            train_df.loc[:, ~train_df.columns.isin(excluded + target)],
            train_df[target],
        )
        X_test, y_test = (
            test_df.loc[:, ~test_df.columns.isin(excluded + target)],
            test_df[target],
        )
    elif run_type == "predict":
        X_test = test_df.loc[:, ~test_df.columns.isin(excluded + target)]

    # Split train and valid
    X_train, X_valid, y_train, y_valid = train_test_split(
        X_train, y_train, test_size=0.2, random_state=0
    )

    logger.info("Done.")

    if run_type == "train":
        return (X_train, X_valid, y_train, y_valid, X_test, y_test)
    elif run_type == "predict":
        return X_test


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()

    parser.add_argument("--run_type", type=str, default="predict")

    args = parser.parse_args()
    logger.info(f"{args=}")
    main(**vars(args))
