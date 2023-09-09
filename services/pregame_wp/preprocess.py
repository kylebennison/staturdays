"""

Ref: https://blog.collegefootballdata.com/talking-tech-building-an-artifical-neural-network-to/

"""
import cfbd
import numpy as np
import pandas as pd
import os
from sklearn.model_selection import train_test_split
from sklearn.decomposition import TruncatedSVD
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline

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
    games = []
    lines = []
    if run_type == "train":
        start = 2015
        end = 2023

    elif run_type == "predict":
        start = 2023
        end = 2024

    for year in range(start, end):
        logger.info(f"Getting year {year}")
        response = games_api.get_games(year=year)
        games = [*games, *response]

        response = betting_api.get_lines(year=year)
        lines = [*lines, *response]
    # Filter data
    if run_type == "train":
        games = [
            g
            for g in games
            if g.home_conference is not None
            and g.away_conference is not None
            and g.home_points is not None
            and g.away_points is not None
        ]
        len(games)
    elif run_type == "predict":
        games = [
            g
            for g in games
            if g.home_conference is not None
            and g.away_conference is not None
            and g.home_points is None
            and g.away_points is None
        ]
        len(games)
    # TODO: Can keep more features as desired
    games = [
        dict(
            id=g.id,
            year=g.season,
            week=g.week,
            neutral_site=g.neutral_site,
            home_team=g.home_team,
            home_conference=g.home_conference,
            home_points=g.home_points,
            home_elo=g.home_pregame_elo,
            away_team=g.away_team,
            away_conference=g.away_conference,
            away_points=g.away_points,
            away_elo=g.away_pregame_elo,
        )
        for g in games
    ]

    # Add spread to games object if a consensus spread is available
    for game in games:
        game_lines = [line for line in lines if line.id == game["id"]]

        if len(game_lines) > 0:
            game_line = [
                line for line in game_lines[0].lines if line.provider == "consensus"
            ]

            if len(game_line) > 0 and game_line[0].spread is not None:
                game["spread"] = float(game_line[0].spread)

            elif len(game_line) == 0:
                game_spread = np.mean(
                    [
                        line.spread
                        for line in game_lines[0].lines
                        if line.spread is not None
                    ]
                )
                game["spread"] = float(game_spread)

    # Filter out games without spread
    games = [
        g
        for g in games
        if "spread" in g and g["spread"] is not None and not np.isnan(g["spread"])
    ]
    if run_type == "train":
        for game in games:
            game["margin"] = game["away_points"] - game["home_points"]
    if run_type == "train":
        df = pd.DataFrame.from_records(games).dropna()
    else:
        df = pd.DataFrame.from_records(games)

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
    returning = player_api.get_returning_production(year=2023)
    returning_list = [
        dict(
            team=g.team,
            year=g.season,
            passing_usage=g.passing_usage,
            percent_passing_ppa=g.percent_passing_ppa,
            percent_ppa=g.percent_ppa,
            percent_receiving_ppa=g.percent_receiving_ppa,
            percent_rushing_ppa=g.percent_rushing_ppa,
            receiving_usage=g.receiving_usage,
            rushing_usage=g.rushing_usage,
            total_passing_ppa=g.total_passing_ppa,
            total_ppa=g.total_ppa,
            total_receiving_ppa=g.total_receiving_ppa,
            total_rushing_ppa=g.total_rushing_ppa,
        )
        for g in returning
    ]
    returning_df = pd.DataFrame.from_records(returning_list)
    returning_df = returning_df.fillna(returning_df.mean(numeric_only=True))
    pl = make_pipeline(StandardScaler(), TruncatedSVD(n_components=2, random_state=0))
    key_cols = ["team", "year"]
    svd_cols = [c for c in returning_df if c not in key_cols]
    pl_res = pl.fit_transform(returning_df.loc[:, svd_cols])
    svd_df = pd.DataFrame(pl_res, columns=["returning_svd_1", "returning_svd_2"])
    returning_df = returning_df.drop(columns=svd_cols)
    returning_df = pd.concat([returning_df, svd_df], axis=1)
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
