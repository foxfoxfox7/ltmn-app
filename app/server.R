setwd('C:/Users/kiera/Projects/ltmn-app/app/')

df_total <- read.csv('./www/Data/plot_data_sample2.csv')


server <- function(input, output) {
  
  #########################
  # Inputs
  
  output$site_output <- renderUI({
    selectInput("site_input", "Site",
                sort(unique(df_total$sitecode)),
                selected = 'Lullington Heath')
  })
  
  output$feature_output <- renderUI({
    selectInput("feature_input", "Feature",
                feat_of_int,
                feat_of_int[1])
  })
  
  

  
  #########################
  # creating the filterred df
  
  site_df <- reactive({
    df_total[df_total$sitecode == input$site_input, ]
    #df_total %>%
    #  filter(sitecode == input$site_input)
  })

  site_feat_df <- reactive({
    site_df()[!is.na(site_df()[ ,input$feature_input]),]
  })





  
  #########################
  # renderring the map

  output$coolplot <- renderLeaflet({
    
    if (is.null(site_feat_df())) {
      return()
    }

    feature = input$feature_input
    df <- site_feat_df()

    east_cent <- get_centre_coords(df)[[1]]
    north_cent <- get_centre_coords(df)[[2]]

    # make palette
    domain <- range(df[ ,feature])

    pal <- colorNumeric(palette = c('white', 'red'), domain = domain)
    # making the reverse pallette for the legend
    pal_rev <- colorNumeric(palette = c('white', 'red'), domain = domain, reverse = TRUE)
    
    print(pal(0.5))

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


  })
  
  # output$coolplot <- renderLeaflet({
  #   
  #   #features = input$feature_input
  #   features = c('species_richness', 'species_diversity')
  #   df <- site_feat_df()
  #   
  #   east_cent <- get_centre_coords(df)[[1]]
  #   north_cent <- get_centre_coords(df)[[2]]
  #   
  #   df$unique_id2 <- as.character(1:length(df$Plot_ID)) %>%
  #     str_pad(., width = 25, side = 'right', pad = 'z')
  #   
  #   list_of_years <- unique(df$Year)
  #   
  #   m <- leaflet(df) %>%
  #     addTiles() %>%
  #     setView(lng=east_cent, lat=north_cent, zoom = 14)
  #   
  #   for (jj in 1:length(features)) {
  #     
  #     domain <- range(df[[features[jj]]])
  #     pal <- colorNumeric(palette = "reds", domain = domain)
  #     
  #     m <- addCircleMarkers(
  #       map = m,
  #       lat=~latitude, 
  #       lng=~longitude, 
  #       color = pal(df[[features[jj]]]),
  #       stroke = FALSE, fillOpacity = 1,
  #       group=~year, 
  #       #label=~BAP_broad, 
  #       layerId = ~paste(unique_id2, features[jj], sep="")) %>%
  #       addLegend(pal = pal,
  #                 values = df[[features[jj]]],
  #                 title = features[jj],
  #                 position = 'bottomleft',
  #                 group = features[jj])
  #     
  #   }
  #   
  #   widget_text <- sprintf("
  #   function(el, x) {
  #     var myMap = this;
  #     var baseLayer = '%s';
  #     myMap.eachLayer(function(layer){
  #       var id = layer.options.layerId;
  #       if (id){
  #         if ('%s' !== id.substring(25,)){
  #           layer.getElement().style.display = 'none';
  #         }
  #       }
  #     })
  #     console.log(myMap.baselayer);
  #     myMap.on('baselayerchange',
  #       function (e) {
  #         baseLayer=e.name;
  #         myMap.eachLayer(function (layer) {
  #             var id = layer.options.layerId;
  #             if (id){
  #               if (e.name !== id.substring(25,)){
  #                 layer.getElement().style.display = 'none';
  #                 layer.closePopup();
  #               }
  #               if (e.name === id.substring(25,)){
  #                 layer.getElement().style.display = 'block';
  #               }
  #             }
  # 
  #         });
  #       })
  #       myMap.on('overlayadd', function(e){
  #         myMap.eachLayer(function(layer){
  #           var id = layer.options.layerId;
  #           if (id){
  #               if (baseLayer !== id.substring(25,)){
  #                 layer.getElement().style.display = 'none';
  #               }
  #           }
  #         })
  #       })
  #   }", features[1], features[1])
  #   
  #   m <- addLayersControl(map = m,
  #                         baseGroups = features,
  #                         overlayGroups = list_of_years,
  #                         options = layersControlOptions(collapsed = F)) %>%
  #     htmlwidgets::onRender(widget_text) %>%
  #     htmlwidgets::onRender("
  #   function(el, x) {
  #     var updateLegend = function () {
  #         var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
  # 
  #         document.querySelectorAll('.legend').forEach(a => a.hidden=true);
  #         document.querySelectorAll('.legend').forEach(l => {
  #           if (l.children[0].children[0].innerText == selectedGroup) l.hidden=false;
  #         });
  #     };
  #     updateLegend();
  #     this.on('baselayerchange', e => updateLegend());
  #   }")
  #   m
  #   
  #   
  # })
  

  
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