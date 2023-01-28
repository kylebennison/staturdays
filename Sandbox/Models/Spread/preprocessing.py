from __future__ import print_function
import cfbd
import os

key = os.getenv("CFBD_API")

import time
import cfbd
from cfbd.rest import ApiException
from pprint import pprint
import pandas as pd

configuration = cfbd.Configuration()
configuration.api_key['Authorization'] = key
configuration.api_key_prefix['Authorization'] = 'Bearer'

api_instance = cfbd.BettingApi(cfbd.ApiClient(configuration))
year = 2022
week = 15

try:
    # Betting lines
    api_response = api_instance.get_lines(year=year, week=week)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling BettingApi->get_lines: %s\n" % e)

df = pd.DataFrame.from_records(g.to_dict() for g in api_response)

print(df)