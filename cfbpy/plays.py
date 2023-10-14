"""

Ref: https://blog.collegefootballdata.com/talking-tech-building-an-artifical-neural-network-to/

"""
import cfbd
import pandas as pd
import numpy as np
import os
from typing import Union, Iterable

import logging

import requests

logging.basicConfig()
logger = logging.getLogger(name="preprocess")
logger.setLevel(logging.INFO)

configuration = cfbd.Configuration()
configuration.api_key["Authorization"] = os.getenv("CFBD_API")
configuration.api_key_prefix["Authorization"] = "Bearer"

api_config = cfbd.ApiClient(configuration)
plays_api = cfbd.PlaysApi(api_config)

API_KEY = os.getenv("CFBD_API")
PREFIX = "Bearer"
URL_PREFIX = "https://api.collegefootballdata.com/plays?"


PLAY_TYPES = [
    # Pass
    "Pass",
    "Pass Completion",
    "Two Point Pass",
    "Pass Reception",
    "Pass Incompletion",
    "Passing Touchdown",
    "Sack",
    "Interception",
    "Pass Interception",
    "Pass Interception Return",
    "Interception Return Touchdown",
    # Rush
    "Rush",
    "Rushing Touchdown",
    "Two Point Rush",
    # Score (Unknown)
    "2pt Conversion",
    "Offensive 1pt Safety",
    # Turnover (Unknown)
    "Fumble Recovery (Own)",
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown",
    "Safety",
    "Defensive 2pt Conversion",
    # Kick
    "Field Goal Good",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Kickoff",
    "Kickoff Return (Offense)",
    "Punt",
    "Blocked PAT",
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt Return",
    "Punt Return Touchdown",
    "Kickoff Return Touchdown",
    "Extra Point Good",
    "Extra Point Missed",
    "Kickoff Return (Defense)",
    # No-Play
    "Penalty",
    "Timeout",
    "Start of Period",
    "End Period",
    "End of Half",
    "Uncategorized",
    "placeholder",
    "End of Game",
    "End of Regulation",
]

PASS_PLAYS = [f for f in PLAY_TYPES if "Pass" in f or "Intercept" in f or "Sack" in f]

RUSH_PLAYS = [f for f in PLAY_TYPES if "Rush" in f]

KICK_PLAYS = [
    # Kick
    "Field Goal Good",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Kickoff",
    "Kickoff Return (Offense)",
    "Punt",
    "Blocked PAT",
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt Return",
    "Punt Return Touchdown",
    "Kickoff Return Touchdown",
    "Extra Point Good",
    "Extra Point Missed",
    "Kickoff Return (Defense)",
]

NON_KICK_PLAYS = PASS_PLAYS + RUSH_PLAYS

NON_PLAY = [
    "Penalty",
    "Timeout",
    "Start of Period",
    "End Period",
    "End of Half",
    "Uncategorized",
    "placeholder",
    "End of Game",
    "End of Regulation",
]

TURNOVER = [f for f in PLAY_TYPES if "Interception" in f] + [
    "Fumble Recovery (Opponent)",
    "Fumble Return Touchdown",
    "Safety",
]

SCORING = [
    f
    for f in PLAY_TYPES
    if "Touchdown" in f
    or "Conversion" in f
    or "2pt" in f
    or "Two Point" in f
    or "Safety" in f
    or "Field Goal Good" in f
    or "Extra Point Good" in f
]


def get_plays(
    years: Union[int, Iterable],
    weeks: Union[int, Iterable],
    classification: str = "fbs",
):
    plays = []

    if type(years) is int:
        years = [years]

    if type(weeks) is int:
        weeks = [weeks]

    for year in years:
        for week in weeks:
            logger.info(f"Getting plays for year {year} week {week}")
            URL = (
                URL_PREFIX
                + f"seasonType=both&year={year}&week={week}&classification={classification}"
            )
            res = requests.get(
                url=URL, headers={"Authorization": f"{PREFIX} {API_KEY}"}
            )
            json = res.json()
            plays.extend(json)
            # plays = [*plays, *json]

    df = pd.DataFrame.from_records(plays)

    return df


def add_features(df: pd.DataFrame):
    """Add extra features to the plays data."""

    # General
    df["margin"] = df["away_score"] - df["home_score"]
    df["home_leading"] = df["margin"] < 0

    # Play Type helpers
    df["interception"] = df["play_text"].str.contains("intercept", case=False)
    df["fumble"] = df["play_text"].str.contains("fumble", case=False)
    df["possession_change"] = df["offense"] != df.groupby("game_id")["offense"].shift(
        -1
    )
    df["rush"] = np.where(
        df["play_type"].isin(RUSH_PLAYS), 1, 0
    )  # TODO: Also scrape play text for "RUSH"
    df["pass"] = np.where(
        df["play_type"].isin(PASS_PLAYS), 1, 0
    )  # TODO: scrape play text for "pass"
    df["turnover"] = np.where(df["play_type"].isin(TURNOVER), 1, 0)

    # Score
    df["home_score"] = np.where(
        df["offense"] == df["home"], df["offense_score"], df["defense_score"]
    )
    df["away_score"] = np.where(
        df["offense"] == df["away"], df["offense_score"], df["defense_score"]
    )
    df["offense_scoring_play"] = (
        # if they're home the home_score increased, or if they're away the away_score increased
        (
            (df["offense"] == df["home"])
            & (df["home_score"] > df.groupby("game_id")["home_score"].shift(1))
        )
        | (
            (df["offense"] == df["away"])
            & (df["away_score"] > df.groupby("game_id")["away_score"].shift(1))
        )
    )
    df["defense_scoring_play"] = (
        # if they're home the home_score increased, or if they're away the away_score increased
        (
            (df["defense"] == df["home"])
            & (df["home_score"] > df.groupby("game_id")["home_score"].shift(1))
        )
        | (
            (df["defense"] == df["away"])
            & (df["away_score"] > df.groupby("game_id")["away_score"].shift(1))
        )
    )

    # Success Rate Helpers
    df["pct_distance_gained"] = df["yards_gained"] / df["distance"]
    df["pct_goal_gained"] = df["yards_gained"] / df["yards_to_goal"]
    df["first_down"] = np.select(
        condlist=[
            # First down offense
            (
                df["play_text"].str.contains(
                    "FIRST DOWN|1ST down", case=False, regex=True
                )
            )
            & (df["offense"].shift(-1) == df["offense"]),
            # Touchdown offense
            (df["offense_scoring_play"]),
        ],
        choicelist=[
            1,
            1,
        ],
        default=0,
    )

    # Success
    df["success"] = np.select(
        condlist=[
            (df["pct_distance_gained"] >= 0.5) & (df["down"] == 1),
            (df["pct_distance_gained"] >= 0.70) & (df["down"] == 2),
            (df["pct_distance_gained"] >= 1) & (df["down"] >= 3),
            (df["offense_scoring_play"]),
        ],
        choicelist=[
            1,
            1,
            1,
            1,
        ],
        default=0,
    )

    return df


def summarise_plays(df: pd.DataFrame):
    """Summarise plays data at the team level."""
    ...


def sum_plays(df: pd.DataFrame):
    """Sum play stats for teams at the game level."""
    ...


def _get_starting_pos(df: pd.DataFrame):
    """Get avg starting position for the offense from a plays dataframe."""
    return (
        df.pipe(lambda df: df[df["play_number"] == 1])  # Each drive starts at 1
        .groupby(["offense"])
        .agg(
            avg_starting_pos=("yards_to_goal", "mean"),
            std_starting_pos=("yards_to_goal", "std"),
        )
    )


def _get_stats(df: pd.DataFrame):
    return df.groupby(["offense"], as_index=False).agg(
        avg_margin=("margin", "mean"),
        std_margin=("margin", "std"),
        avg_success=("success", "mean"),
        avg_pct_distance=("pct_distance_gained", "mean"),
        avg_pct_goal=("pct_goal_gained", "mean"),
        avg_distance=("distance", "mean"),
        avg_yds_gained=("yards_gained", "mean"),
        rushes=("rush", "sum"),
        passes=("pass", "sum"),
        ints=("interception", "sum"),
        fumbles=("fumble", "sum"),
    )


def _success_by_down(df: pd.DataFrame):
    """Take plays input and return success sums by down on offense and defense."""
    downs_df = (
        df[(df["down"] > 0) & (df["down"] < 5)]
        .groupby(["offense", "game_id", "down"], as_index=False)
        .agg(
            successes=("success", "sum"),
            attempts=("success", "count"),
        )
        .pivot(index=["offense", "game_id"], columns="down")
    )
    downs_df.columns = [x + "_down_" + str(y) for x, y in downs_df.columns]

    def_downs_df = (
        df[(df["down"] > 0) & (df["down"] < 5)]
        .groupby(["defense", "game_id", "down"], as_index=False)
        .agg(
            successes_allowed=("success", "sum"),
            attempts_faced=("success", "count"),
        )
        .pivot(index=["defense", "game_id"], columns="down")
    )

    def_downs_df.columns = [x + "_down_" + str(y) for x, y in def_downs_df.columns]

    # Join together
    downs_df = (
        downs_df.merge(
            def_downs_df,
            how="left",
            left_index=True,
            right_index=True,
        )
        .reset_index()
        .rename(columns={"offense": "team"})
    )

    return downs_df


def _success_by_play_type(df: pd.DataFrame):
    def _success_play_type_sum(df: pd.DataFrame, side: str):
        pass_rush_df = (
            df[(df["pass"] == 1) | (df["rush"] == 1)]
            .groupby(["game_id", side, "pass"], as_index=False)
            .agg(
                success=("success", "sum"),
                pct_distance=("pct_distance_gained", "sum"),
                pct_goal=("pct_goal_gained", "sum"),
                attempts=("success", "count"),
            )
            .assign(play_type=lambda df: np.where(df["pass"] == 1, "pass", "rush"))
            .drop("pass", axis=1)
            .pivot(
                index=["game_id", side],
                columns="play_type",
                values=["success", "pct_distance", "pct_goal", "attempts"],
            )
        )
        pass_rush_df.columns = [
            x + "_" + y + "_" + side for x, y in pass_rush_df.columns
        ]

        pass_rush_df.index.names = ["game_id", "team"]

        return pass_rush_df

    offense = _success_play_type_sum(df=df, side="offense")
    defense = _success_play_type_sum(df=df, side="defense")

    # Merge together
    offense = offense.merge(
        defense,
        how="left",
        left_index=True,
        right_index=True,
    )

    offense = offense.reset_index()

    return offense


def _sum_field_pos(df):
    """Sum starting field position by drive per team."""
    return (
        df.pipe(lambda df: df[df["play_number"] == 1])  # Each drive starts at 1
        .groupby(["offense", "game_id"])
        .agg(
            starting_pos_sum=("yards_to_goal", "sum"),
            n_drives=("drive_id", "count"),
        )
        .reset_index()
        .rename(columns={"offense": "team"})
    )
