import cfbd
import pandas as pd
import os
from sklearn.decomposition import TruncatedSVD
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import make_pipeline
import logging

configuration = cfbd.Configuration()
configuration.api_key["Authorization"] = os.getenv("CFBD_API")
configuration.api_key_prefix["Authorization"] = "Bearer"

api_config = cfbd.ApiClient(configuration)
player_api = cfbd.PlayersApi(api_config)

logging.basicConfig()
logger = logging.getLogger(name="offseason")
logger.setLevel(logging.INFO)


def get_portal(year: int):
    portal = player_api.get_transfer_portal(year=year)
    portal_list = [
        dict(
            old_team=g.origin,
            year=g.season,
            new_team=g.destination,
            position=g.position,
            rating=g.rating,
            stars=g.stars,
            date=g.transfer_date,
        )
        for g in portal
    ]
    portal_df = pd.DataFrame.from_records(portal_list)
    incoming_talent = pd.DataFrame(
        {"incoming_count": portal_df.groupby("new_team")["old_team"].count()}
    )
    incoming_talent = pd.concat(
        [
            incoming_talent,
            portal_df.groupby("new_team")[["rating", "stars"]].sum(numeric_only=True),
        ],
        axis=1,
    ).reset_index()
    outgoing_talent = pd.DataFrame(
        {"outgoing_count": portal_df.groupby("old_team")["old_team"].count()}
    )
    outgoing_talent = pd.concat(
        [
            outgoing_talent,
            portal_df.groupby("old_team")[["rating", "stars"]].sum(numeric_only=True),
        ],
        axis=1,
    ).reset_index()
    incoming_talent = incoming_talent.fillna(incoming_talent.mean(numeric_only=True))
    outgoing_talent = outgoing_talent.fillna(outgoing_talent.mean(numeric_only=True))
    incoming_talent.columns = [
        "team",
        "incoming_count",
        "incoming_rating",
        "incoming_stars",
    ]
    outgoing_talent.columns = [
        "team",
        "outgoing_count",
        "outgoing_rating",
        "outgoing_stars",
    ]
    talent_df = pd.merge(
        left=incoming_talent, right=outgoing_talent, how="outer", on="team"
    )
    talent_df = talent_df.fillna(0)
    talent_df["season"] = year

    return talent_df


def get_returning(years: int, apply_svd: bool = True):
    res_list = []

    if type(years) is int:
        years = [years]

    for year in years:
        logger.info(f"Getting returning players for year {year}")
        response = player_api.get_returning_production(year=year)
        res_list = [*res_list, *response]

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
        for g in res_list
    ]
    returning_df = pd.DataFrame.from_records(returning_list)
    returning_df = returning_df.fillna(returning_df.mean(numeric_only=True))

    if apply_svd:
        pl = make_pipeline(
            StandardScaler(), TruncatedSVD(n_components=2, random_state=0)
        )
        key_cols = ["team", "year"]
        svd_cols = [c for c in returning_df if c not in key_cols]
        pl_res = pl.fit_transform(returning_df.loc[:, svd_cols])
        svd_df = pd.DataFrame(pl_res, columns=["returning_svd_1", "returning_svd_2"])
        returning_df = returning_df.drop(columns=svd_cols)
        returning_df = pd.concat([returning_df, svd_df], axis=1)

    return returning_df
