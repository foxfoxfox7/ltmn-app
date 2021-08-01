import pandas as pd
import numpy as np
import math

from coord_transform import os_to_ll, ll_to_os, apply_coord_to_df



df = pd.read_csv('./main_data/plot_data.csv')



df = apply_coord_to_df(os_to_ll, df, ['eastings', 'northings'], ['lat', 'long'])
df.to_csv('./main_data/plot_data2.csv', index=False)

