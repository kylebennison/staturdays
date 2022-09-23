# -*- coding: utf-8 -*-
"""
Created on Thu Mar 17 00:17:20 2022

@author: Kyle
"""

import requests
import pandas as pd


    
df_final = pd.DataFrame()

df_want = pd.DataFrame()

def get_table(url, year):
    
    html = requests.get(url).content
    
    df_list = pd.read_html(html)
    
    df_want = pd.DataFrame()
    
    # get the table you want
    df_want = df_list[0]
    
    df_want['year'] = year
        
    return(df_want)
    
for i in range(2000, 2022):
    
    url = 'https://nflcombineresults.com/nflcombinedata.php?year=' + str(i) + '&pos=&college='
    
    df_want = get_table(url, 2022)
    
    df_final = pd.concat([df_final, df_want])