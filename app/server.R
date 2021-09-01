setwd('C:/Users/kiera/Projects/ltmn-app/app/')

df_total <- read.csv('./www/Data/plot_data_sample2.csv')


server <- function(input, output) {
  
  #########################
  # Selecting the site and pairing down the datafame
  
  output$site_output <- renderUI({
    selectInput("site_input", 
                "Site",
                sort(unique(df_total$sitecode)),
                selected = 'Lullington Heath')
  })
  
  site_df <- reactive({
    
    if (is.null(input$site_input)) {
      return()
    }
    
    df_total[df_total$sitecode == input$site_input, ]
  })
  
  
  
  #########################
  # Selecting the extra input information depending on the tab
  
  
  # output$feature_output <- renderUI({
  #   
  #   if (input$tab == "1") {
  #     selectInput("feature_input", 
  #                 "Feature",
  #                 feat_of_int,
  #                 feat_of_int[1])
  #   } else {
  #     return(NULL)
  #   }
  #     
  # })
  # 
  # output$habitat_output <- renderUI({
  #   
  #   if (input$tab == "2") {
  #     selectInput("Habitat_input", 
  #                 "Habitat",
  #                 sort(unique(site_df()$broad_hab)),
  #                 multiple = TRUE)
  #   } else {
  #     return(NULL)
  #   }
  #   
  # })
  
  
  output$feature_output <- renderUI({
    
    if (input$tab != "1") {
      return(NULL)
    } 

    selectInput("feature_input", 
                "Feature",
                feat_of_int,
                feat_of_int[1])
  })
  
  output$habitat_output <- renderUI({
    
    if (input$tab != "2") {
      return(NULL)
    }
    
    selectInput("habitat_input", 
                "Habitat",
                sort(unique(site_df()$broad_hab)),
                multiple = TRUE)
    
  })

  


  
  #########################
  # renderring the map

  output$coolplot <- renderLeaflet({
    
    if (is.null(site_df())) {
      return()
    }
    
    if (input$tab == "1") {
      #FIXME
      feature = input$feature_input
      
      df <- site_df()[!is.na(site_df()[ ,input$feature_input]),]

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
                         #label=~year,
                         stroke = FALSE, fillOpacity = 0.5) %>%
        addLegend(pal = pal_rev, values = ~df[[feature]],
                  title = feature,
                  labFormat = labelFormat(transform =
                                            function(x) sort(x, decreasing = TRUE)))
      
    } else if (input$tab == "2") {
      
      
      east_cent <- get_centre_coords(site_df())[[1]]
      north_cent <- get_centre_coords(site_df())[[2]]
      
      df <- site_df()[!is.na(site_df()[ ,input$feature_input]),] %>%
        filter(broad_hab %in% input$habitat_input)
      
      if (dim(df)[0] == 0) {
        leaflet(df) %>%
          addTiles() %>%
          setView(lng=east_cent, lat=north_cent, zoom = 14)
      } else {
        # make palette
        domain <- range(df[ ,feature])
        
        pal <- colorNumeric(palette = c('white', 'blue'), domain = domain)
        # making the reverse pallette for the legend
        pal_rev <- colorNumeric(palette = c('white', 'blue'), domain = domain, reverse = TRUE)
        
        leaflet(df) %>%
          addTiles() %>%
          setView(lng=east_cent, lat=north_cent, zoom = 14) %>%
          addCircleMarkers(lng = ~longitude, lat = ~latitude,
                           color = ~pal(df[[feature]]),
                           #label=~year,
                           stroke = FALSE, fillOpacity = 0.5) %>%
          addLegend(pal = pal_rev, values = ~df[[feature]],
                    title = feature,
                    labFormat = labelFormat(transform =
                                              function(x) sort(x, decreasing = TRUE)))
      }
      
    }
      
      

      
      
    
  })

    


    
  
  
  
  
}





















# get_change_by_year <- function(df) {
#   
#   # these columns give inofrmation about the plot but aren't quantitatively comp
#   track_cols <- c('Plot_ID', 'Longitude', 'Latitude', 'BAP_broad',
#                   'BAP_priority', 'NVC_group', 'NVC_habitat')
#   # these columns wil be compared between years
#   change_cols <- c('Species_richness', 'Species_diversity', 'Light', 'Wetness',
#                    'pH', 'Fertility', 'Competition', 'Stress', 'Ruderals',
#                    'Vegetation_height', 'Litter', 'Bare_ground')
#   
#   df_year_list = list()
#   for (ii in 1:length(unique_years)) {
#     df_year <- df %>%
#       filter(Year == unique_years[ii]) %>%
#       dplyr::select(all_of(track_cols), all_of(change_cols))
#     
#     # Adding the year to the column so we can put different years in the same row
#     year_marker <- unique_years[ii]
#     colnames(df_year)[-1] <- paste(colnames(df_year)[-1], year_marker, sep = '_')
#     df_year$Plot_ID <- gsub('a$', '', df_year$Plot_ID)
#     
#     df_year_list[[ii]] <- df_year
#   }
#   
#   df_change <- df_year_list[[1]]
#   
#   for (ii in 1:(length(df_year_list)-1)) {
#     df_change <- full_join(df_change, df_year_list[[ii+1]], by = 'Plot_ID')
#   }
#   
#   year_change_list = list()
#   for (ii in 1:(length(unique_years)-1)) {
#     
#     east_col <- paste('Longitude', unique_years[ii+1], sep='_')
#     north_col <- paste('Latitude', unique_years[ii+1], sep='_')
#     bap_col <- paste('BAP_broad', unique_years[ii+1], sep='_')
#     nvc_col <- paste('NVC_habitat', unique_years[ii+1], sep='_')
#     
#     # building the basic blocks of the change df
#     yearly_change <- tibble(
#       df_change[ , 'Plot_ID'],
#       Year = unique_years[ii+1],
#       df_change[ ,east_col],
#       df_change[ , north_col],
#       df_change[ ,bap_col],
#       df_change[ ,nvc_col]
#     )
#     
#     # removing the year from the end of the coordinates colums so they join
#     names(yearly_change)[names(yearly_change) == east_col] <-
#       gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == east_col])
#     names(yearly_change)[names(yearly_change) == north_col] <-
#       gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == north_col])
#     names(yearly_change)[names(yearly_change) == bap_col] <-
#       gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == bap_col])
#     names(yearly_change)[names(yearly_change) == nvc_col] <-
#       gsub('.{5}$', '', names(yearly_change)[names(yearly_change) == nvc_col])
#     
#     for (jj in 1:length(change_cols)) {
#       # choosing the feature to look at the difference
#       feature <- change_cols[jj]
#       
#       # getting the two columns to take the difference between
#       col_from <- paste(feature, unique_years[ii], sep='_')
#       col_to <- paste(feature, unique_years[ii+1], sep='_')
#       
#       # Making the new feature column names
#       feature_col_name <- paste(feature, 'diff', sep='_')
#       feature_col_name_norm <- paste(feature, 'diff', 'norm', sep='_')
#       
#       # Saving the difference between the years as a new column
#       # and another column for the normalised difference
#       yearly_change[ ,feature_col_name] <-
#         df_change[ ,col_to] - df_change[ ,col_from]
#       yearly_change[ ,feature_col_name_norm] <-
#         (df_change[ ,col_to] - df_change[ ,col_from]) /
#         ((df_change[ ,col_to] + df_change[ ,col_from]) /2 ) # the average (norm)
#       yearly_change[ ,feature_col_name_norm] <- 
#         yearly_change[ ,feature_col_name_norm] * 100
#     }
#     
#     year_change_list[[ii]] <- yearly_change
#   }
#   
#   total_change <- bind_rows(year_change_list)
#   
#   return(total_change)
# }
# 
# rem_plot <- function(df, plot) {
#   for (ii in 1:length(plot)) {
#     df <- df %>%
#       filter(Plot_ID != plot[ii])
#   }
#   
#   return(df)
# }