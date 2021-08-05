import pandas as pd
import numpy as np
import math

from convertbng.util import convert_bng, convert_lonlat

from coord_transform import os_to_ll, ll_to_os, apply_coord_to_df

# LOOP BY 50 IN A FUNCTION

# df = pd.read_csv('./main_data/plot_data.csv')


# df = df[0:200]

# print(df.shape)
# print(df.loc[0, 'eastings'])


# def chunks(lst, n):
#     """Yield successive n-sized chunks from lst."""
#     for i in range(0, len(lst), n):
#         yield lst[i:i + n]

# east_all = df[['eastings']].values[0:1000]
# north_all = df[['northings']].values[0:1000]
# east_chunks = list(chunks(east_all, 50))
# north_chunks = list(chunks(north_all, 50))
# n_lists = len(east_chunks)
# lat_tot = []
# lon_tot = []
# for ii in range(0, n_lists):
# 	lat, lon  = convert_lonlat(east_chunks[ii], north_chunks[ii])
# 	lat_tot.append(lat)
# 	lon_tot.append(lon)


print(convert_lonlat([330116], [411577]))


# print(df.head())

#east_ll = df[['eastings']].values[:50]
#east = [item for sublist in east_ll for item in sublist]
#north_ll = df[['northings']].values[:50]
#north = [item for sublist in north_ll for item in sublist]

#east = np.transpose(np.array(df[['eastings']].values[:50], dtype = float)[0])
#north = np.transpose(np.array(df[['northings']].values[:50], dtype = float)[0])



#df = apply_coord_to_df(os_to_ll, df, ['eastings', 'northings'], ['lat', 'long'])
#test1, test2  = convert_lonlat(east_ll, north_ll)
#test1, test2  = convert_lonlat()





#df.to_csv('./main_data/plot_data2.csv', index=False)







# SCRAP


# def chunks(lst, n):
#     """Yield successive n-sized chunks from lst."""
#     for i in range(0, len(lst), n):
#         yield lst[i:i + n]

# east_all = df[['eastings']].values[0:1000]
# north_all = df[['northings']].values[0:1000]
# east_chunks = list(chunks(east_all, 50))
# north_chunks = list(chunks(north_all, 50))
# n_lists = len(east_chunks)
# lat_tot = []
# lon_tot = []
# for ii in range(0, n_lists):
# 	lat, lon  = convert_lonlat(east_chunks[ii], north_chunks[ii])
# 	lat_tot.append(lat)
# 	lon_tot.append(lon)







#df['coord_tuple'] = df.apply(lambda x: convert_lonlat(x[['eastings']].values, x[['northings']].values), axis=1)





# df['lon'] = np.NaN
# df['lat'] = np.NaN
# for index, row in df.iterrows():
	
# 	lon, lat = convert_lonlat(row['eastings'], row['northings'])
	
# 	df.loc[index, 'lat'] = lat
# 	df.loc[index, 'lon'] = lon