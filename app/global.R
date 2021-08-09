library(shiny)
library(plyr)
library(dplyr)
library(stringr)

library(leaflet)
library(mapview)
library(colorspace)
library(RColorBrewer)

library(ggiraph)

library(shiny)
library(leaflet)
library(shinydashboard)


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


feat_of_int <- c('light', 'wetness', 'acidity', 'fertility', 'competition', 
                 'stress', 'ruderals', 'species_richness', 'species_diversity', 
                 'veg_height', 'veg_height_std', 'litter', 'bare_x')