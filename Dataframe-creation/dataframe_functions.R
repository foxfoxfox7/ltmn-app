library(tidyverse)
library(raster)
library(sp)
library(rgdal)
library(stringdist)

setwd('C:/Users/kiera/Projects/ltmn-app/Dataframe-creation/')

################################################################################
#
#                   Importing data
#
################################################################################

site_names <- list(
  B01	=	"Ainsdale",
  B02	=	"Bure Marshes",
  B03	=	"Burnham Beeches",
  B09	=	"Derbyshire Dales",
  B10	=	"Fenns, Whixall and Bettisfield Mosses",
  B11	=	"Finglandrigg Woods",
  B12	=	"Ingleborough",
  B13	=	"Lindisfarne",
  B14	=	"Lullington Heath",
  B15	=	"Martin Down",
  B16	=	"Monks Wood",
  B18	=	"North Solent",
  B20	=	"Old Winchester Hill",
  B25	=	"Stiperstones",
  B26	=	"Thursley",
  B29	=	"East Dartmoor Woods & Heaths",
  B30	=	"Chippenham Fen",
  B31	=	"Chobham Common",
  B32	=	"Dersingham Bog",
  B33	=	"Downton Gorge",
  B34	=	"Epping Forest",
  B35	=	"Kielderhead",
  B36	=	"Lower Derwent Valley",
  B37	=	"Ludham-Potter Heigham",
  B38	=	"Malham Tarn",
  B39	=	"Saltfleetby-Theddlethorpe Dunes",
  B40	=	"The Lizard",
  B41	=	"Dark Peak",
  B42	=	"North Walney",
  B43	=	"Mottey Meadows",
  B44	=	"Wyre Forest ",
  B45	=	"Woodwalton Fen",
  B46	=	"Braunton Burrows",
  B47	=	"Ennerdale & Scoat Fell",
  B48	=	"May Moss",
  B49	=	"Roudsea Wood and Mosses",
  B50	=	"Cross Fell"
)

get_names <- function(file_name) {
  # Renaming some of the habitat names as they are too far off and wont be found
  name_swap <- read.csv(file_name)
  
  name_swap_bap <- with(name_swap, setNames(right_name, wrong_name))
  # name_swap_bap <- name_swap_bap[!is.na(name_swap_bap)]
  # name_swap_bap <- name_swap_bap[name_swap_bap != ""]
  
  name_swap_nvc <- with(name_swap, setNames(NVC_name, NVC_code))
  name_swap_nvc <- name_swap_nvc[!is.na(name_swap_nvc)]
  name_swap_nvc <- name_swap_nvc[name_swap_nvc != ""]
  
  # Getting the lsit of approved names for BAP_broad and priorty
  bb_list <- name_swap$BAP_broad
  bb_list <- bb_list[!is.na(bb_list)]
  bb_list <- bb_list[bb_list != ""]
  bp_list <- name_swap$BAP_priority
  bp_list <- bp_list[!is.na(bp_list)]
  bp_list <- bp_list[bp_list != ""]
  
  return(list(name_swap_bap, name_swap_nvc, bb_list, bp_list))
}

################################################################################
#
#                   Initial extract from the excel file
#
################################################################################

get_base <- function(file_name){
  
  # These are the columns we want from wpd
  base_cols <- c('PLOT_ID', 'SITECODE', 'YEAR')
  
  # Getting the data out of whole plot data to group the spec data
  base_df <- read_excel(file_name, sheet = "Whole Plot Data", na = c('', "NA", "N/A")) %>%
    dplyr::select(contains(base_cols))
  
  base_df$PLOT_ID <- as.character(base_df$PLOT_ID)
  
  return(base_df)
}

get_whole_plot_data <- function(file_name) {
  
  # These are the columns we want from wpd
  wpd_data_cols <- c('PLOT_ID', 'SITECODE', 'YEAR', 'SDATE', 'EASTINGS' , 
                     'NORTHINGS', 'BNG_GRID_REF', 'LIGHT', 'WETNESS', 'PH', 'FERTILITY',
                     'COMPETITION', 'STRESS', 'RUDERALS',
                     'BAP_BROAD', 'BAP_PRIORITY', 'NVC_FIRST')
  
  # Getting the data out of whole plot data to group the spec data
  wpd_data <- read_excel(file_name, sheet = "Whole Plot Data", na = c('', "NA", "N/A")) %>%
    dplyr::select(contains(wpd_data_cols))
  
  # Always convert to character as some are character and some are num
  wpd_data$PLOT_ID <- as.character(wpd_data$PLOT_ID)
  # Helps with some of the typos involving capital letters
  wpd_data$BAP_BROAD <- str_to_title(wpd_data$BAP_BROAD)
  wpd_data$BAP_PRIORITY <- str_to_title(wpd_data$BAP_PRIORITY)
  
  wpd_data$PLOT_ID <- sub('a$', '', wpd_data$PLOT_ID)
  
  colnames(wpd_data) <-c('plot_id', 'sitecode', 'year', 'date', 'eastings',
                         'northings', 'bng_grid', 'light', 'wetness', 'aciditiy', 
                         'fertility', 'competition', 'stress', 'ruderals', 
                         'broad_hab', 'priority_hab', 'nvc_result')
  
  return(wpd_data)
}

get_species_template <- function(file_name) {
  
  st <- read_excel(file_name, sheet = "Species Template", na = c('', "NA", "N/A"))
  
  # The list of columns I want to keep. only part of the string
  keep_list <- c('PLOT_ID', 'DESC', 'CELL', 'PERCENT', 'FREQ')
  # Reducing the size of the tibble to only the kept columns
  st <- st %>%
    dplyr::select(contains(keep_list))
  
  # There are often many trailing empyty rows
  st <- st[!is.na(st$DESC_LATIN), ]
  # converting all plot_ids to character as some are and some are not
  st$PLOT_ID <- as.character(st$PLOT_ID)
  st$PERCENT_COVER <- as.numeric(st$PERCENT_COVER)
  st$FREQUENCY <- as.numeric(st$FREQUENCY)
  
  st$PLOT_ID <- sub('a$', '', st$PLOT_ID)
  
  # Getting the names of the CELL columns
  cell_names <- names(st)[grep('CELL', names(st))]
  # Changing all missing data in CELL columns to 0
  #st[cell_names][is.na(st[cell_names])] <- 0
  # converting any non numeric to numeric
  st[cell_names] <- lapply(st[cell_names], function(x) as.numeric(as.character(x)))
  
  # FIXING THE FREQUENCY COLUMNS
  # Calculating FREQ from the contents of CELL
  st <- st %>%
    mutate(FREQUENCY = rowSums(.[cell_names], na.rm = TRUE))
  
  st[is.na(st$PERCENT_COVER), 'PERCENT_COVER'] <- st[is.na(st$PERCENT_COVER), 'FREQUENCY'] *1.5
  
  st <- st[ , !(names(st) %in% cell_names)]
  colnames(st) <- c('plot_id', 'species', 'percent_cover', 'frequency')
  
  return(st)
}

get_ground_features <- function(file_name, base_df){
  
  gf <- read_excel(file_name, sheet = "Ground Features", na = c('', "NA", "N/A"))
  # Getting the names of the columns
  col_names_gf <- names(gf)
  
  # Getting the names of the CELL columns
  cell_names <- col_names_gf[grep('CELL', col_names_gf)]
  # converting any non numeric to numeric
  gf[cell_names] <- lapply(gf[cell_names], function(x) as.numeric(as.character(x)))
  # converting all plot_ids to character as some are and some are not
  gf$PLOT_ID <- as.character(gf$PLOT_ID)
  # Inconsistencies with cpitalisation across surveys
  gf$FEATURE <- tolower(gf$FEATURE)
  
  gf$PLOT_ID <- sub('a$', '', gf$PLOT_ID)
  
  ##############################################################################
  # Getting vegetation height
  ##############################################################################
  
  # FIXING THE FREQUENCY COLUMNS
  keep_cols <- c('PLOT_ID', 'YEAR', 'FEATURE')
  # Taking only the vegetaion height rows
  # these are treated separately as the data analysis is different (mean)
  veg_height <- dplyr::select(filter(gf, FEATURE == 'vegetation height'),
                              append(keep_cols, cell_names))
  
  # Creating blank columns so that if there is no veg ehight data, we still
  # have oclumns for future steps
  veg_height$MEAN_HEIGHT <- NA
  veg_height$STD_HEIGHT <- NA
  
  # some surveys dont have veg height data
  try({
    veg_height$MEAN_HEIGHT <- rowMeans(veg_height[cell_names], na.rm=TRUE)
    # rowStds uses matrixStats package and needs data to be in matrix form
    veg_height$STD_HEIGHT <- rowSds(as.matrix(veg_height[cell_names]), na.rm=TRUE)
  })
  
  ##############################################################################
  # Getting bare_x frequency
  ##############################################################################
  
  # Taking all the rest of the rows
  other_rows <- dplyr::select(filter(gf, FEATURE != 'vegetation height'),
                              append(keep_cols, cell_names))
  
  # Changing all missing data in CELL columns to 0
  other_rows[cell_names][is.na(other_rows[cell_names])] <- 0
  
  # Getting a list of the plot IDs
  gf_plot_ids <- unique(other_rows['PLOT_ID'])
  gf_N_plots <- length(gf_plot_ids$PLOT_ID)
  
  bare_tib_list = list()
  for (jj in 1:gf_N_plots) {
    # separating out the data to each plot and only rows with 'bare' in FEATURE
    single_bare <- other_rows %>%
      filter(PLOT_ID == gf_plot_ids$PLOT_ID[jj]) %>%
      .[grep("bare", .$FEATURE), ]
    
    # Create the row for the new bare_x feature
    bare_tib <- tibble(PLOT_ID = gf_plot_ids$PLOT_ID[jj],
                       YEAR = other_rows$YEAR[1],
                       FEATURE = 'bare_x')
    # Function checks for any 1s in the column
    count_bare <- function(col_name) {
      sum_col <- 0
      try(sum_col <- sum(single_bare[ ,col_name]), silent=TRUE)
      if (sum_col == 0) {
        return (0)
      } else {
        return (1)
      }
    }
    # Checking for presence in a bare row and if so counting it as 1
    # then inputting that 1 in the appropriate cell column of the new row
    for (kk in 1:length(cell_names)) {
      bare_tib[ ,cell_names[kk]] <- count_bare(cell_names[kk])
    }
    # Adding each row to a column, ready to be combined and re-added
    bare_tib_list[[jj]] <- bare_tib
  }
  
  bare_total <- bind_rows(bare_tib_list)
  other_rows <- rbind(other_rows, bare_total)
  
  # Calculating FREQ from the contents of CELL
  other_rows <- other_rows %>%
    mutate(FREQ = rowSums(.[cell_names]))
  
  # The list of columns I want to keep. only part of the string
  keep_list_gf <- c('PLOT_ID', 'FEATURE', 'FREQ')
  # Reducing the size of the tibble to only the kept columns
  other_rows <- other_rows %>%
    dplyr::select(contains(keep_list_gf))
  
  # Making a pivot table for each plot and combining
  plot_pivots_gf <- list()
  for (jj in 1:gf_N_plots) {
    single_plot_gf <- other_rows %>%
      # filters for only one plot
      filter(PLOT_ID == gf_plot_ids$PLOT_ID[jj]) %>%
      # There are lots of instances of more than one of the same species per plot
      .[!duplicated(.$FEATURE), ] %>%
      # transforms the data into short and fat
      pivot_wider(names_from = FEATURE, values_from = FREQ)
    
    plot_pivots_gf[[jj]] <- single_plot_gf
  }
  frequency_gf <- rbind.fill(plot_pivots_gf)
  
  
  # repacing NA with 0 in certain columns
  frequency_gf[2:ncol(frequency_gf)][is.na(frequency_gf[2:ncol(frequency_gf)])] <- 0
  
  ##############################################################################
  # Combining it all together
  ##############################################################################
  
  base_df <- base_df %>%
    full_join(.,
              veg_height[ ,c('PLOT_ID', 'YEAR', 'MEAN_HEIGHT', 'STD_HEIGHT')],
              by = c("PLOT_ID", "YEAR")) %>%
    full_join(., frequency_gf, by = "PLOT_ID")
  
  final_keep <- c('PLOT_ID', 'SITECODE', 'YEAR', 'MEAN_HEIGHT', 'STD_HEIGHT',
                  'litter', 'bare_x', 'test')
  
  base_df <- base_df %>%
    dplyr::select(contains(final_keep))
  
  colnames(base_df)[1:5] <- c('plot_id', 'sitecode', 'year', 'veg_height',
                              'veg_height_std')
  
  return(base_df)
}

################################################################################
#
#                  Calculated metrics from the init
#
################################################################################

pivot_on_species <- function(df, pivot_col) {
  
  # Changing all NAs in percent cover or frequency to 999 so they can be
  # changed back later after we have dealth with other NAs
  df[, pivot_col][is.na(df[, pivot_col])] <- 999
  
  # The list of columns I want to keep. only part of the string
  keep_list_freq <- c('plot_id', 'species', pivot_col)
  # Reducing the size of the tibble to only the kept columns
  st_freq <- df %>%
    dplyr::select(contains(keep_list_freq))
  
  # Getting a list of the plot IDs
  plot_ids <- unique(st_freq['plot_id'])
  N_plots <- length(plot_ids$plot_id)
  
  # Making a pivot table for each plot and combining
  plot_pivots <- list()
  for (jj in 1:N_plots) {
    single_plot <- st_freq %>%
      # filters for only one plot
      filter(plot_id == plot_ids$plot_id[jj]) %>%
      # There are lots of instances of more than one of the same species per plot
      .[!duplicated(.$species), ] %>%
      # transforms the data into short and fat
      pivot_wider(names_from = species, 
                  values_from = !!sym(pivot_col),
                  names_repair = 'unique')
    
    plot_pivots[[jj]] <- single_plot
  }
  full_pivot <- rbind.fill(plot_pivots)
  
  # Converting NA to 0
  full_pivot[is.na(full_pivot)] <- 0
  # These ones should be NA, they are missing data from the start
  full_pivot[full_pivot == 999] <- NA
  
  return(full_pivot)
}

get_diversity_richness <- function(df){
  
  # The first column is plot_id
  spec_names <- names(frequency)[-1]
  
  # Getting a list of the plot IDs
  plot_ids <- unique(df['plot_id'])
  N_plots <- length(plot_ids$plot_id)
  
  # Calculating the species diversity from the frequency table
  # Has to be done plot by plot as for some reason it mixes the order up!
  simpson_div_plot_list = list()
  for (jj in 1:N_plots) {
    single_plot_freq <- frequency %>%
      # filters for only one plot
      filter(plot_id == plot_ids$plot_id[jj])
    
    simpson_div_plot_list[[jj]] <- diversity(as.matrix(single_plot_freq[ ,spec_names]), type = "simpson")
  }
  simpson_div_plot_total <- bind_rows(simpson_div_plot_list)
  
  spec_pp <- tibble(plot_id = frequency$plot_id,
                    species_richness = rowSums(frequency[spec_names]!=0),
                    species_diversity = simpson_div_plot_total$simpson.I)
  
  return(spec_pp)
}

nvc_divide <- function(df) {
  
  mavis_col_name <- c('plot_id', 'sitecode', 'year', 'nvc_result')
  mavis <- df %>%
    dplyr::select(contains(mavis_col_name))

  # SEPARATING OUT THE NVC COLUMN data
  mavis$nvc_subgroup <- str_extract(mavis$nvc_result, '^.*?(?=:)')
  mavis$nvc_group <- gsub('(\\d)\\D+$', '\\1', mavis$nvc_subgroup)
  mavis$nvc_habitat <- gsub('[0-9]+', '', mavis$nvc_group)
  
  return(mavis)
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

################################################################################
#
#                  accuracy changes to the dfs
#
################################################################################

rename <- function(input, list_comp, naming_cutoff=0.2) {
  # Makes automatic name changes where the name is close to the names in a list
  # of pre-determined names from a list
  if ( (!input %in% list_comp) & (!is.na(input)) ) {
    list_of_matches <- stringdist(input, list_comp, method='jw')
    if (min(list_of_matches) < naming_cutoff) {
      new_name <- list_comp[which.min(list_of_matches)]
      return(new_name)
    } else {
      return(input)
    }
  } else {
    return(input)
  }
}

fix_habitat_names <- function(df, name_swap_vector) {
  
  df[['broad_hab']] <- mapvalues(df[['broad_hab']],
                                 names(name_swap_vector[[1]]),
                                 unlist(name_swap_vector[[1]]),
                                 warn_missing = FALSE)
  df[['priority_hab']] <- mapvalues(df[['priority_hab']],
                                    names(name_swap_vector[[1]]),
                                    unlist(name_swap_vector[[1]]),
                                    warn_missing = FALSE)
  
  df[['nvc_habitat']] <- mapvalues(df[['nvc_habitat']],
                                         names(name_swap_vector[[2]]),
                                         unlist(name_swap_vector[[2]]),
                                         warn_missing = FALSE)
  
  # Renaming any typos in the bap habitats and changing to NA when they don't
  # match our list of habitats
  df$broad_hab <- unname(sapply(df$broad_hab, rename, list_comp=name_swap_vector[[3]]))
  df$priority_hab <- unname(sapply(df$priority_hab, rename, list_comp=name_swap_vector[[4]]))
  
  return(df)
}

rename_species <- function(df, name_swap){
  
  append_str <- function(input, suffix) {
    # Appends ' sp.' after species names when there is only one word
    # because it is a genus
    if (!grepl(' ', input)) {
      return(paste(input, suffix, sep = ''))
    } else {
      return(input)
    }
  }
  
  append_str2 <- function(input, find, suffix) {
    # usesd for adding a '.' after a 'sp'
    reg <- paste(find, '$', sep='')
    
    if (grepl(reg, input)) {
      return(paste(input, suffix, sep = ''))
    } else {
      return(input)
    }
  }
  
  df$species <- df$species %>%
    map_chr(., append_str, ' sp.') %>%
    map_chr(., append_str2, ' sp', '.') %>%
    # Na get converted to 'NA sp.' so they need to be converted back to blank
    map_chr(function(x) {gsub("NA sp.", NA, x)}) %>%
    mapvalues(., name_swap[['wrong_name']], name_swap[['right_name']], warn_missing = FALSE)
  
  df <- df[(df$species != ''), ]
  
  return(df)
}
  
################################################################################
#
#                 making summary dataframes
#
################################################################################

get_hab_sums <- function(df, habitat) {
  av_cols <- c('species_richness', 'species_diversity', 'light', 'fertility',
               'acidity', 'wetness', 'stress', 'competition', 'ruderals', 
               'veg_height', 'litter', 'bare_x')
  
  df[ ,av_cols][is.na(df[ ,av_cols])] <- 0
  
  all_habs <- unique(df[[habitat]])
  
  hab_summary_list = list()
  for (ii in 1:length(all_habs)) {
    df_hab <- df %>%
      #filter(BAP_broad == all_habs[ii])
      filter(.[[habitat]] == all_habs[ii])
    
    hab_av <- summarize_all(df_hab[, av_cols], mean)
    hab_av$Habitat <- all_habs[ii]
    
    hab_summary_list[[ii]] <- hab_av
  }
  
  hab_summary <- bind_rows(hab_summary_list)
  
  hab_summary <- hab_summary[complete.cases(hab_summary[ , 'Habitat']),]
  #row.names(hab_summary) <- hab_summary$Habitat
  
  return(hab_summary)
}






