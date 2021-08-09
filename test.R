library(shiny)
library(plyr)
library(dplyr)
library(stringr)

library(leaflet)
library(mapview)
library(colorspace)
library(RColorBrewer)
library(tidyverse)
#library(raster)
library(sp)
library(rgdal)

library(rnrfa)

#setwd('C:/Users/kiera/Projects/ltmn-app/')
setwd('C:/Users/Kieran.Fox/Work/ltmn-app')

df_total <- read.csv('./dataframe-creation/main_data/plot_data_sample.csv')# %>%
  #dplyr::select(-c('longitude', 'latitude'))

df_total <- df_total %>%
  dplyr::filter(!is.na(eastings) | !is.na(northings))


#df_lonlat <- EastNorth_to_LongLat(df_total)
df_total <- df_total[!is.na(df_total$bng_grid), ]

coord <- list(lon = NA, lat = NA)
coord <- osg_parse(df_total$bng_grid,
                   coord_system = 'WGS84')

df_total$lon = coord[['lon']]
df_total$lat = coord[['lat']]


map_broke <- c("Braunton Burrows", "Lullington Heath", "Martin Down", 
               "North Solent", "Roudsea Wood and Mosses", 
               "Saltfleetby-Theddlethorpe Dunes", "Woodwalton Fen",
               "Wyre Forest ")

df_total <- df_total[!(df_total$sitecode %in% map_broke), ]
write.csv(df_total, './dataframe-creation/main_data/plot_data_sample2.csv',
          row.names = FALSE)



get_centre_coords <- function(df) {
  # Finding the centre point of the site
  east_min <- min(df[!is.na(df[ ,'lon']),][ ,'lon'])
  east_max <- max(df[!is.na(df[ ,'lon']),][ ,'lon'])
  north_min <- min(df[!is.na(df[ ,'lat']),][ ,'lat'])
  north_max <- max(df[!is.na(df[ ,'lat']),][ ,'lat'])
  
  east_cent = (east_min + east_max) / 2
  north_cent = (north_min + north_max) / 2
  
  return(list(east_cent, north_cent))
}




feature <- 'light'
site <- 'Ainsdale'

df <- df_total[df_total$sitecode == site, ] %>%
  .[!is.na(.[ ,feature]), ]

east_cent <- get_centre_coords(df)[[1]]
north_cent <- get_centre_coords(df)[[2]]



# make palette
domain <- range(df[ ,feature])

pal <- colorNumeric(palette = c('white', 'red'), domain = domain)
# making the reverse pallette for the legend
pal_rev <- colorNumeric(palette = c('white', 'red'), domain = domain, reverse = TRUE)


leaflet(df) %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat,
                   color = ~pal(df[[feature]]),
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addLegend(pal = pal_rev, values = ~df[[feature]],
            title = feature,
            labFormat = labelFormat(transform =
                                      function(x) sort(x, decreasing = TRUE)))









# SCRAP


# df_coords <- df_total %>%
#   dplyr::transmute(  # create new columns and drop all the others
#     eastings = as.numeric(as.character(eastings)), # make this text column numeric
#     northings = as.numeric(as.character(northings))
#   ) %>% 
#   dplyr::rename(longitude = eastings, latitude = northings)  # rename
# 
# df_data <- df_total %>%
#   dplyr::select(-eastings, -northings)  # select all columns except the coords
# 
# schools_spdf <- sp::SpatialPointsDataFrame(  # create a SPDF
#   coords = df_coords,  # the school co-ordinates
#   data = df_data,  # the school data
#   proj4string = CRS("+init=epsg:27700")  # BNG projection system
# ) %>% 
#   sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system



