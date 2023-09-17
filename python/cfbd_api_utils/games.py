"""

Ref: https://blog.collegefootballdata.com/talking-tech-building-an-artifical-neural-network-to/

"""
import cfbd
import pandas as pd
import os
from typing import Union, Iterable

import logging

logging.basicConfig()
logger = logging.getLogger(name="preprocess")
logger.setLevel(logging.INFO)

configuration = cfbd.Configuration()
configuration.api_key["Authorization"] = os.getenv("CFBD_API")
configuration.api_key_prefix["Authorization"] = "Bearer"

api_config = cfbd.ApiClient(configuration)
games_api = cfbd.GamesApi(api_config)
betting_api = cfbd.BettingApi(api_config)
metrics_api = cfbd.MetricsApi(api_config)

FBS_CONFS = [
    "FBS Independents",
    "Mountain West",
    "SEC",
    "Big Ten",
    "Big 12",
    "Pac-12",
    "ACC",
    "American",
    "Conference USA",
    "Mid-American",
    "Sun Belt",
]


def get_games(years: Union[int, Iterable], fbs_only: bool = True):
    games = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting games for year {year}")
        response = games_api.get_games(year=year)
        games = [*games, *response]

    games = [
        dict(
            id=g.id,
            year=g.season,
            week=g.week,
            season_type=g.season_type,
            start_date=g.start_date,
            completed=g.completed,
            conference_game=g.conference_game,
            neutral_site=g.neutral_site,
            home_team=g.home_team,
            home_conference=g.home_conference,
            home_points=g.home_points,
            home_elo=g.home_pregame_elo,
            home_post_wp=g.home_post_win_prob,
            away_team=g.away_team,
            away_conference=g.away_conference,
            away_points=g.away_points,
            away_elo=g.away_pregame_elo,
            away_post_wp=g.away_post_win_prob,
        )
        for g in games
    ]

    df = pd.DataFrame.from_records(games)

    df["margin"] = df["away_points"] - df["home_points"]

    # Add lines
    lines_df = get_lines(years=years)

    df = df.merge(
        right=lines_df, how="left", on=["id", "week", "season_type", "start_date"]
    )

    if fbs_only:
        df = df[
            (df["home_conference"].isin(FBS_CONFS))
            | (df["away_conference"].isin(FBS_CONFS))
        ]

    df = df.reset_index(drop=True)

    return df


def get_lines(years: Union[int, Iterable]):
    lines = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting lines for year {year}")
        response = betting_api.get_lines(year=year)
        lines = [*lines, *response]

    df = [
        dict(
            id=game.id,
            season=game.season,
            week=game.week,
            season_type=game.season_type,
            start_date=game.start_date,
            lines=game.lines,
        )
        for game in lines
    ]

    for game in df:
        lines = game["lines"]

        providers = []
        for line in lines:
            providers.append(line.provider)

        if "consensus" in providers:
            line = [line for line in lines if line.provider == "consensus"][0]

        elif "DraftKings" in providers:
            line = [line for line in lines if line.provider == "DraftKings"][0]

        else:
            # TODO: Get avgs of the rest of the fields
            game["home_ml"] = None
            game["away_ml"] = None
            game["over_under"] = None
            game["spread"] = None
            game["spread_open"] = None
            game["over_under_open"] = None
            continue

        game["home_ml"] = line.home_moneyline
        game["away_ml"] = line.away_moneyline
        game["over_under"] = line.over_under
        game["spread"] = line.spread
        game["spread_open"] = line.spread_open
        game["over_under_open"] = line.over_under_open

    for g in df:
        g.pop("lines")

    df = pd.DataFrame.from_records(df)

    # Remove completely NA rows
    keep_rows = (
        df[
            [
                "home_ml",
                "away_ml",
                "over_under",
                "spread",
                "spread_open",
                "over_under_open",
            ]
        ]
        .isna()
        .sum(axis=1)
        < 6
    )

    return df[keep_rows]


def get_pregame_wp(years: Union[int, Iterable]):
    wp = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting pregame wp for year {year}")
        response = metrics_api.get_pregame_win_probabilities(year=year)
        wp = [*wp, *response]

    dict_wp = [
        dict(
            year=x.season,
            week=x.week,
            id=x.game_id,
            home_wp_pregame=x.home_win_prob,
            home_team=x.home_team,
            away_team=x.away_team,
        )
        for x in wp
    ]

    df = pd.DataFrame.from_records(dict_wp)

    return df
