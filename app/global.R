library(shiny)
library(shinydashboard)
#library(ggiraph)

library(plyr)
library(dplyr)
library(stringr)

library(leaflet)
library(mapview)

library(colorspace)
library(RColorBrewer)

# Importing the transformed per plot data
df_total <- read.csv('./www/Data/plot_data_sample2.csv')

# The list of features that can be mapped
feat_of_int <- c('light', 'wetness', 'acidity', 'fertility', 'competition', 
                 'stress', 'ruderals', 'species_richness', 'species_diversity', 
                 'veg_height', 'veg_height_std', 'litter', 'bare_x')

# Finding the centre point of the site
get_centre_coords <- function(df, lon = 'longitude', lat = 'latitude') {
  east_min <- min(df[!is.na(df[ ,lon]),][ ,lon])
  east_max <- max(df[!is.na(df[ ,lon]),][ ,lon])
  north_min <- min(df[!is.na(df[ ,lat]),][ ,lat])
  north_max <- max(df[!is.na(df[ ,lat]),][ ,lat])
  
  east_cent = (east_min + east_max) / 2
  north_cent = (north_min + north_max) / 2
  
  return(list(east_cent, north_cent))
}
