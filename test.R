library(shiny)
library(plyr)
library(dplyr)
library(stringr)

library(leaflet)
library(mapview)
library(colorspace)
library(RColorBrewer)

#library(raster)
library(sp)
library(rgdal)

#library(rnrfa)

setwd('C:/Users/kiera/Projects/ltmn-app/')

df_total <- read.csv('./dataframe-creation/main_data/plot_data_sample.csv')# %>%
  #dplyr::select(-c('longitude', 'latitude'))

df_total <- df_total %>%
  dplyr::filter(!is.na(eastings) | !is.na(northings))


df_coords <- df_total %>%
  dplyr::transmute(  # create new columns and drop all the others
    eastings = as.numeric(as.character(eastings)), # make this text column numeric
    northings = as.numeric(as.character(northings))
  ) %>% 
  dplyr::rename(longitude = eastings, latitude = northings)  # rename

df_data <- df_total %>%
  dplyr::select(-eastings, -northings)  # select all columns except the coords

schools_spdf <- sp::SpatialPointsDataFrame(  # create a SPDF
  coords = df_coords,  # the school co-ordinates
  data = df_data,  # the school data
  proj4string = CRS("+init=epsg:27700")  # BNG projection system
) %>% 
  sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system











df_lonlat <- EastNorth_to_LongLat(df_total)
print(df_total$bng_grid)

coord <- list(lon = NA, lat = NA)
coord <- osg_parse(df_total$bng_grid[1:5],
                   coord_system = 'WGS84')


osg_parse(grid_refs = c("SN831869","SN829838"),
          coord_system = 'WGS84')

osg_parse(grid_refs = "SN853872", coord_system = "WGS84")
print(df_total$bng_grid[1:5])


test <- osg_parse("SD3011611577", coord_system = 'WGS84')










schools_spdf <- sp::SpatialPointsDataFrame(  # create a SPDF
  coords = schools_coords,  # the school co-ordinates
  data = schools_data,  # the school data
  proj4string = CRS("+init=epsg:27700")  # BNG projection system
) %>% 
  sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system








get_centre_coords <- function(df) {
  # Finding the centre point of the site
  east_min <- min(df[!is.na(df[ ,'longitude']),][ ,'longitude'])
  east_max <- max(df[!is.na(df[ ,'longitude']),][ ,'longitude'])
  north_min <- min(df[!is.na(df[ ,'latitude']),][ ,'latitude'])
  north_max <- max(df[!is.na(df[ ,'latitude']),][ ,'latitude'])
  
  east_cent = (east_min + east_max) / 2
  north_cent = (north_min + north_max) / 2
  
  return(list(east_cent, north_cent))
}

EastNorth_to_LongLat <- function(df) {
  
  coord_cols <- c('plot_id', 'sitecode', 'year', 'eastings', 'northings')
  
  # df_full is not touched and has all the original plots
  df_full <- df %>%
    dplyr::select(contains(coord_cols))
  # need to get rid of NA vals so the new coords can be put back into a df 
  df_plot <- df_full[!is.na(df_full$eastings), ]
  # this will be used to make the calculation. destroys the df gets rid of NA
  # so the coordinate calculatino can work
  df <- df[!is.na(df$eastings), ]
  
  bng_proj <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 
  +x_0=400000 +y_0=-100000 +ellps=airy
  +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs'
  
  coordinates(df) <- c("eastings", "northings")
  proj4string(df) <- projection(bng_proj)
  plotsWGS <- spTransform(df, projection("+proj=longlat +datum=WGS84"))
  
  df_plot$longitude <- plotsWGS@coords[,1]
  df_plot$latitude <- plotsWGS@coords[,2]
  
  df_plot <- full_join(df_full, df_plot,
                       by = c("plot_id", "sitecode", "year", "eastings", "northings"))
  
  return(df_plot)
}


feature <- 'light'
site <- 'B01'

df <- df_total[df_total$sitecode == site, ] %>%
  .[!is.na(.[ ,feature]), ]

df$eastings <- as.integer(df$eastings)
df$northings <- as.integer(df$northings)

#df <- EastNorth_to_LongLat(df)



coord <- osg_parse(paste0(letters, corridor, stairs),
                   coord_system = 'WGS84')



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
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   color = ~pal(df[[feature]]),
                   stroke = FALSE, fillOpacity = 0.5) %>%
  addCircleMarkers(lng = -2.931132,
                   lat = 52.578856) %>%
  addLegend(pal = pal_rev, values = ~df[[feature]],
            title = feature,
            labFormat = labelFormat(transform =
                                      function(x) sort(x, decreasing = TRUE)))


leaflet(df) %>%
  addTiles() %>%
  setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
  addCircleMarkers(lng = -3.0573889,
                   lat = 53.596086) %>%
  addCircleMarkers(lng = -3.056037,
                   lat = 53.59586) %>%
  addCircleMarkers(lng = -3.05738657,
                   lat = 53.59610154)