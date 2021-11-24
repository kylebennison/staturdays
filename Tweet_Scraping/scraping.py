 
import pandas as pd
import requests
from bs4 import BeautifulSoup

res = requests.get("https://www.racing-reference.info/race/2019_Acura_Grand_Prix_of_Long_Beach/O")
soup = BeautifulSoup(res.content,'html.parser')

table = soup.find_all('table')[5]

table_rows = table.find_all('tr')

l = []
for tr in table_rows:
	td = tr.find_all('td')
	row = [tr.text for tr in td]
	l.append(row)
dt = pd.DataFrame(l)