import re
import requests
import ast
import pandas as pd
import json
import numpy as np
import regex


links = [
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20011001'
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20021001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20031001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20041001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20051001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20061001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20071001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20081001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20091001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20101001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20111001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20121001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20131001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20141001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20151001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20161001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20171001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20181001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=0&id=BTechPlayM&Date=20191001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20011001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20021001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20031001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20041001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20051001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20061001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20071001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20081001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20091001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20101001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20111001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20121001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20131001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20141001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20151001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20161001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20171001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20181001',
    'http://www.fivb.org/Vis/Public/JS/Beach/SeasonRank.aspx?Gender=1&id=BTechPlayM&Date=20191001'
]


df = []
dai = []
sex = []
for link in links:
    gender = link.split('TechPlay')[1][0]
    days = link.split('=')[3]
    year = days[:4]+'-'
    month = days[4:6]+'-'
    date = days[6:]
    day = year + month + date
    r = requests.get(link)
    data = re.findall(r'(\[[^[\]]*])', r.text)
    for player in data:
        details = ast.literal_eval(player)
        df.append(details)
        dai.append(day)
        sex.append(gender)
        df2 = pd.DataFrame(df)
        df3 = pd.DataFrame(dai)
        df4 = pd.DataFrame(sex)
        final_df = pd.concat([df2,df3,df4],axis=1)
        final_df.to_csv('history.csv')
        

