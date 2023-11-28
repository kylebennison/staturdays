"""

Ref: https://blog.collegefootballdata.com/talking-tech-building-an-artifical-neural-network-to/

TODO:

- QB games experience
- QB injured/benched (check if more than 1 QB played in previous game AND it wasn't because it was a blowout)
- Head coach games experience with team
- multiple rolling windows (4, 8, 12)
- increase training data size
- add standard deviation of scoring wherever possible

"""
import cfbd
import numpy as np
import pandas as pd
import os
import sys
import logging
from typing import Union

sys.path.append("../../")

from cfbpy import games, offseason, ratings  # noqa E402

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


def calc_rolling_results(df: pd.DataFrame, windows: Union[int, list]) -> pd.DataFrame:
    # TODO: Add arg weighting function to use
    # Pivot home and away games
    logger.info("Starting rolling calculations.")
    schedule = df.melt(
        id_vars=[
            "id",
            "week",
            "year",
            "start_date",
            "home_points",
            "away_points",
            "home_elo",
            "away_elo",
        ],
        var_name="home_away",
        value_vars=["home_team", "away_team"],
        value_name="team",
    ).sort_values("id")

    # Cumsum wins
    schedule["win"] = np.where(
        (
            (schedule["home_points"] > schedule["away_points"])
            & (schedule["home_away"] == "home_team")
        )
        | (
            (schedule["home_points"] < schedule["away_points"])
            & (schedule["home_away"] == "away_team")
        ),
        1,
        0,
    )
    schedule["total_wins"] = schedule.groupby(["team", "year"])["win"].cumsum()
    schedule["total_wins"] = schedule.groupby(["team", "year"])["total_wins"].shift(
        1, fill_value=0
    )
    schedule["points"] = np.where(
        schedule["home_away"] == "home_team",
        schedule["home_points"],
        schedule["away_points"],
    )
    schedule["opp_points"] = np.where(
        schedule["home_away"] == "home_team",
        schedule["away_points"],
        schedule["home_points"],
    )
    schedule["opp_elo"] = np.where(
        schedule["home_away"] == "home_team", schedule["away_elo"], schedule["home_elo"]
    )

    # Calc rolling mean for points, opponent points, and opponent elo for previous 12 games
    # TODO: Could specify a function to win_type to weight more recent games more
    final = pd.DataFrame()
    if type(windows) is int:
        windows = [windows]
    for window in windows:
        roll = (
            schedule.groupby("team", as_index=False)[
                ["points", "opp_points", "opp_elo"]
            ]
            .rolling(window=window, min_periods=1)
            .agg(["mean", "std"])
            .droplevel(0, axis=0)
        )
        roll.columns = roll.columns.map("_".join)

        roll = schedule.merge(roll, how="left", left_index=True, right_index=True)

        roll_cols = [x for x in roll.columns if "_mean" in x or "_std" in x]

        new_names = [
            x + f"_{window}" for x in roll.columns if "_mean" in x or "_std" in x
        ]

        # rename col with window
        roll[new_names] = roll[roll_cols].add_suffix(f"_{window}")

        roll = roll.drop(columns=roll_cols)

        roll[new_names] = roll.groupby("team")[new_names].shift(1, fill_value=0)

        # Concat with final DF
        final = pd.concat([final, roll], axis=1)

    # Redefine list of columns we want
    final_names = [x for x in final.columns if "_mean" in x or "_std" in x]

    # Keep only relevant columns
    final = final[["id", "team", "total_wins"] + final_names]

    # Drop dupe ID and team names
    final = final.loc[:, ~final.columns.duplicated()]

    logger.info("Finished rolling calculations.")

    return final


def main(
    run_type: str,
    train_start_year: int = 2014,
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

    # power_5 = ["Big Ten", "ACC", "SEC", "Big 12", "Pac-12"]

    df[cat_features] = df[cat_features].astype("category")

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
    # NOTE: Excluding for now as 2023 srs is not available
    # srs = ratings.get_srs(years=YEARS)

    # # for 2022 games we want the 2021 srs, so increment srs year by 1 for joining
    # srs["year"] = srs["year"] + 1

    # df = pd.merge(
    #     df,
    #     srs.rename(columns={"team": "home_team"}),
    #     how="left",
    #     on=["year", "home_team"],
    # )
    # df = pd.merge(
    #     df,
    #     srs.rename(columns={"team": "away_team"}),
    #     how="left",
    #     on=["year", "away_team"],
    #     suffixes=["_home", "_away"],
    # )

    # Feature: Previous week's poll ranking
    # TODO

    # Feature: Pregame WP
    wp = games.get_pregame_wp(years=YEARS)

    df = pd.merge(
        df, wp, how="left", on=["id", "home_team", "away_team", "year", "week"]
    )

    # Feature: Past game results
    schedule = calc_rolling_results(df, [4, 8, 12])

    # Join to games
    df = df.merge(
        schedule.rename(columns={"team": "home_team"}),
        how="left",
        on=["id", "home_team"],
    )
    df = df.merge(
        schedule.rename(columns={"team": "away_team"}),
        how="left",
        on=["id", "away_team"],
        suffixes=["_home", "_away"],
    )

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
        X_train.to_parquet(path="./data/X_train.parquet")
        y_train.to_parquet(path="./data/y_train.parquet")
        X_test.to_parquet(path="./data/X_test.parquet")
        y_test.to_parquet(path="./data/y_test.parquet")
        return (X_train, y_train, X_test, y_test)
    elif run_type == "predict":
        X_test.to_parquet(path="./data/X_predict.parquet")
        return X_test


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()

    parser.add_argument("--run_type", type=str, default="predict")
    parser.add_argument("--train_start_year", type=int, default=2014)
    parser.add_argument("--predict_year", type=int, default=2023)

    args = parser.parse_args()
    logger.info(f"{args=}")
    main(**vars(args))
