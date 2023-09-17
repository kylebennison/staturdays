"""

Ref: https://blog.collegefootballdata.com/talking-tech-building-an-artifical-neural-network-to/

"""
import cfbd
import numpy as np
import pandas as pd
import os
import sys
from sklearn.model_selection import train_test_split
import logging

sys.path.append("../../")

from python.cfbd_api_utils import games, offseason, ratings  # noqa E402

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


def main(
    run_type: str,
    train_start_year: int = 2015,
    predict_year: int = 2023,
):
    logger.info("Starting main.")
    logger.info(f"{run_type = }")
    logger.info(f"{train_start_year = }")
    logger.info(f"{predict_year = }")
    if run_type == "train":
        YEARS = list(range(train_start_year, predict_year))
        df = games.get_games(years=YEARS)
    if run_type == "predict":
        YEARS = predict_year
        df = games.get_games(YEARS)

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

    # Feature: Returning players
    returning_df = offseason.get_returning(YEARS)

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

    # Feature: Portal
    # TODO

    # Feature: Prev-season PPA
    ppa = ratings.get_ppa_season(years=YEARS)

    # for 2022 games we want the 2021 ppa, so increment ppa year by 1 for joining
    ppa["year"] = ppa["year"] + 1

    df = pd.merge(
        df,
        ppa.rename(columns={"team": "home_team"}),
        how="left",
        on=["year", "home_team"],
    )
    df = pd.merge(
        df,
        ppa.rename(columns={"team": "away_team"}),
        how="left",
        on=["year", "away_team"],
        suffixes=["_home", "_away"],
    )

    # Feature: Prev-season SP+
    sp = ratings.get_sp(years=YEARS)

    # for 2022 games we want the 2021 sp+, so increment year by 1 for joining
    sp["year"] = sp["year"] + 1

    df = pd.merge(
        df,
        sp.rename(columns={"team": "home_team"}),
        how="left",
        on=["year", "home_team"],
    )
    df = pd.merge(
        df,
        sp.rename(columns={"team": "away_team"}),
        how="left",
        on=["year", "away_team"],
        suffixes=["_home", "_away"],
    )

    # Feature: Prev-season SRS
    srs = ratings.get_srs(years=YEARS)

    # for 2022 games we want the 2021 srs, so increment srs year by 1 for joining
    srs["year"] = srs["year"] + 1

    df = pd.merge(
        df,
        srs.rename(columns={"team": "home_team"}),
        how="left",
        on=["year", "home_team"],
    )
    df = pd.merge(
        df,
        srs.rename(columns={"team": "away_team"}),
        how="left",
        on=["year", "away_team"],
        suffixes=["_home", "_away"],
    )

    # Feature: Pregame WP
    wp = games.get_pregame_wp(years=YEARS)

    logger.info(f"{df.id.head() = }")
    logger.info(f"{wp.id.head() = }")
    logger.info(f"{df.dtypes = }")
    logger.info(f"{wp.dtypes = }")
    df = pd.merge(
        df, wp, how="left", on=["id", "home_team", "away_team", "year", "week"]
    )

    logger.info(f"{df.home_wp_pregame.head() = }")

    # Clean column names
    df.columns = [c.lower().replace(" ", "_").replace("-", "_") for c in df.columns]
    test_year = df.year.max()
    test_df = df.query(f"year == {test_year}")
    train_df = df.query(f"year != {test_year}")

    if run_type == "train":
        X_train, y_train = (
            train_df.loc[:, ~train_df.columns.isin(target)],
            train_df[target],
        )
        X_test, y_test = (
            test_df.loc[:, ~test_df.columns.isin(target)],
            test_df[target],
        )
    elif run_type == "predict":
        X_test = test_df.loc[:, ~test_df.columns.isin(target)]

    logger.info("Done.")

    if run_type == "train":
        # Split train and valid
        # TODO: Split train and valid on year as well instead of randomly
        X_train, X_valid, y_train, y_valid = train_test_split(
            X_train, y_train, test_size=0.2, random_state=0
        )
        X_train.to_parquet(path="./X_train.parquet")
        X_valid.to_parquet(path="./X_valid.parquet")
        y_train.to_parquet(path="./y_train.parquet")
        y_valid.to_parquet(path="./y_valid.parquet")
        X_test.to_parquet(path="./X_test.parquet")
        y_test.to_parquet(path="./y_test.parquet")
        return (X_train, X_valid, y_train, y_valid, X_test, y_test)
    elif run_type == "predict":
        X_test.to_parquet(path="./X_predict.parquet")
        return X_test


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()

    parser.add_argument("--run_type", type=str, default="predict")
    parser.add_argument("--train_start_year", type=int, default=2015)
    parser.add_argument("--predict_year", type=int, default=2023)

    args = parser.parse_args()
    logger.info(f"{args=}")
    main(**vars(args))
