import cfbd
import pandas as pd
import os
from typing import Union
import logging

configuration = cfbd.Configuration()
configuration.api_key["Authorization"] = os.getenv("CFBD_API")
configuration.api_key_prefix["Authorization"] = "Bearer"

api_config = cfbd.ApiClient(configuration)
ratings_api = cfbd.RatingsApi(api_config)
metrics_api = cfbd.MetricsApi(api_config)
rankings_api = cfbd.RankingsApi(api_config)

logging.basicConfig()
logger = logging.getLogger(name="ratings")
logger.setLevel(logging.INFO)


def get_sp(years: Union[int, list]):
    sp = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting sp+ for year {year}")
        response = ratings_api.get_sp_ratings(year=year)
        sp = [*sp, *response]

    dict_sp = [
        dict(
            year=x.year,
            team=x.team,
            sp_total_rating=x.rating,
            sp_total_ranking=x.ranking,
            sp_off_rating=x.offense.rating,
            sp_off_ranking=x.offense.ranking,
            sp_def_rating=x.defense.rating,
            sp_def_ranking=x.defense.ranking,
            sp_st_rating=x.special_teams.rating,
        )
        for x in sp
    ]
    df_sp = pd.DataFrame.from_records(dict_sp)
    return df_sp


def get_srs(years: Union[int, list]):
    srs = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting srs for year {year}")
        response = ratings_api.get_srs_ratings(year=year)
        srs = [*srs, *response]

    dict_srs = [
        dict(
            year=x.year,
            team=x.team,
            srs_rating=x.rating,
            srs_ranking=x.ranking,
        )
        for x in srs
    ]
    df_srs = pd.DataFrame.from_records(dict_srs)
    return df_srs


def get_ppa_season(years: Union[int, list]):
    res_list = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting ppa for year {year}")
        response = metrics_api.get_team_ppa(year=year)
        res_list = [*res_list, *response]

    res_dict = [
        dict(
            year=x.season,
            team=x.team,
            ppa_off_cum=x.offense.cumulative.total,
            ppa_off_pass=x.offense.passing,
            ppa_off_rush=x.offense.rushing,
            ppa_off_third=x.offense.third_down,
            ppa_def_cum=x.defense.cumulative.total,
            ppa_def_pass=x.defense.passing,
            ppa_def_rush=x.defense.rushing,
            ppa_def_third=x.defense.third_down,
        )
        for x in res_list
    ]
    df = pd.DataFrame.from_records(res_dict)
    return df


def get_polls(years: Union[int, list]):
    res_list = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting polls for year {year}")
        response = rankings_api.get_rankings(year=year)
        res_list = [*res_list, *response]

    data = []
    for team in res_list:
        for poll in team.polls:
            dat = [
                dict(
                    team=t.school,
                    poll=poll.poll,
                    rank=t.rank,
                    points=t.points,
                    votes=t.first_place_votes,
                    year=team.season,
                    week=team.week,
                    season_type=team.season_type,
                )
                for t in poll.ranks
            ]

            data = [*data, *dat]
    df = pd.DataFrame.from_records(data)
    return df
