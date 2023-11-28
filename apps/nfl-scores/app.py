import streamlit as st
import bs4 as bs
import requests
import pandas as pd
import logging
import os

logger = logging.getLogger()
logger.setLevel(logging.DEBUG)

os.write(1, b"starting up\n")
print("hiya")
print("test")

headers = {
    "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36"
}
scorepage = requests.get("https://www.espn.com/nfl/scoreboard", headers=headers)
content = scorepage.content
soup = bs.BeautifulSoup(content, "html.parser")
games = soup.select("div[class^='ScoreboardScoreCell pa4 nfl']")
game_id = []
team_list = []
score_list = []
for i, game in enumerate(games):
    game_id.append(i)
    game_id.append(i)
    scores = game.select("li[class^=ScoreboardScoreCell]")
    for score in scores:
        team_value = score.select_one("div[class^=ScoreCell__TeamName]").text
        try:
            score_value = score.select_one("div[class^=ScoreCell__Score]").text
        except AttributeError as err:
            logger.debug(err)
            # os.write(1, bytes(str(err), "utf-8"))
            score_value = 0
        team_list.append(team_value)
        score_list.append(score_value)
game_dict = {"id": game_id, "team": team_list, "score": score_list}


# col_a, col_b, col_c, col_d = st.columns(4)

df = pd.DataFrame.from_dict(game_dict)
df

# with col_a:
#     st.text("Hello Kyle")

# with col_d:
#     st.text("hey")
#     st.text("hi")
