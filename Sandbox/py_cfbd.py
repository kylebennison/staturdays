from __future__ import print_function
import cfbd
import pandas as pd
import numpy as np

import time
import cfbd
from cfbd.rest import ApiException
from pprint import pprint

# Configure API key authorization: ApiKeyAuth
# configuration = cfbd.Configuration()
# configuration.api_key['Authorization'] = 'qqJCQGNCYhSDBGJzryFfP8/adPdF/J6ibVH8dqCYa9nYHO5iJ3Ehkr0ZYjnhlQUv'
# configuration.api_key_prefix['Authorization'] = 'Bearer'

# create an instance of the API class
# api_instance = cfbd.BettingApi(cfbd.ApiClient(configuration))
year = 2020 # int | Year/season filter for games (optional)
week = 1 # int | Week filter (optional)
season_type = 'regular' # str | Season type filter (regular or postseason) (optional) (default to regular)

try:
    # Betting lines
    api_response = api_instance.get_lines(year=year, week=week, season_type=season_type)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling BettingApi->get_lines: %s\n" % e)
