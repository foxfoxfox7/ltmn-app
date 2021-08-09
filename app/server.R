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

    # The min and max values in the feature for making the pallette
    domain <- range(df[ ,feature])

    # Creating a palette of colours to represent the feature. white to red
    pal <- colorNumeric(palette = c('white', 'red'), domain = domain)
    # making the reverse palette for the legend
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

  })
}
