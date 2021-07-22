library(tidyverse)
library(readxl)
library(plyr)
library(matrixStats)
library(diverse)
library(openxlsx)
library(rjson)



# change this to your working direstory (the directory with the code in)
setwd('C:/Users/kiera/Projects/ltmn-app/Dataframe-creation/')
source('./dataframe_functions.R')

################################################################################
# Reading in information from disk
################################################################################

# Put the survey files in here
data <- '../Data/Vegetation_old/'
import <- './import_data/'
outputs <- './main_data/'
dir.create(outputs)

list_of_files <- list.files(data)

print(list_of_files)

# reading in some information i have inputted manually in csv files the name 
# swaps for typos in the habitats as well as the full list of proper names
habitat_list_holder <- get_names(paste0(import, 'rename_habitat.csv'))
# reading in our list of typos to change
name_swap <- read.csv(paste0(import, 'species_rename.csv'))


freq_list = list()
pcover_list = list()
plot_data_list = list()
for (ii in 1:length(list_of_files)) {
  
  print(list_of_files[ii])
  file_name <- paste(data, list_of_files[ii], sep='')
  
  ##############################################################################
  # Getting the basic data from the excel file
  ##############################################################################

  base_df <- get_base(file_name)
  wpd_data <- get_whole_plot_data(file_name)
  st <- get_species_template(file_name)
  gf_df <- get_ground_features(file_name, base_df)
  
  ##############################################################################
  # Pivotting the data for Frequency
  ##############################################################################
  
  st <- rename_species(st, name_swap)
  percent_cover <- pivot_on_species(st, 'percent_cover')
  frequency <- pivot_on_species(st, 'frequency')

  ##############################################################################
  # calculating further metrics
  ##############################################################################
  
  div_rich <- get_diversity_richness(frequency)
  mavis <- nvc_divide(wpd_data)
  coords <- EastNorth_to_LongLat(wpd_data)
  
  ##############################################################################
  # making corrections to the dfs
  ##############################################################################
  
  wpd_whole <- wpd_data %>%
    full_join(., mavis, by = c("plot_id", "sitecode", "year", "nvc_result")) %>%
    full_join(., div_rich, by = "plot_id") %>%
    full_join(., coords, by = c("plot_id", "sitecode", "year", "eastings", "northings")) %>%
    full_join(., gf_df, by = c("plot_id", "sitecode", "year"))
    
  wpd_whole <- fix_habitat_names(wpd_whole, habitat_list_holder)

  freq_list[[ii]] <- frequency
  pcover_list[[ii]] <- percent_cover
  plot_data_list[[ii]] <- wpd_whole
}

################################################################################
# Combning it together ready for analysis
################################################################################

# combining it all together
freq_total <- bind_rows(freq_list)
pcover_total <- bind_rows(pcover_list)
plot_data_total <- bind_rows(plot_data_list)

# checking for completely blank rows in the data columns and removing them
blank_check <- c('species_richness', 'litter', 'nvc_habitat', 'veg_height')
blank_len <- length(blank_check)
plot_data_total <- plot_data_total[rowSums(is.na(plot_data_total[,blank_check]))!=blank_len,]

# Choose how you want to write the final dataframe
write.csv(plot_data_total, paste0(outputs, 'plot_data.csv'), row.names = FALSE)
write.csv(freq_total, paste0(outputs, 'species_freq.csv'), row.names = FALSE)
write.csv(pcover_total, paste0(outputs, 'species_pcover.csv'), row.names = FALSE)

hab_sum_bb <- get_hab_sums(df_all, 'broad_hab')
hab_sum_bp <- get_hab_sums(df_all, 'priority_hab')
hab_sum_n <- get_hab_sums(df_all, 'nvc_habitat')
