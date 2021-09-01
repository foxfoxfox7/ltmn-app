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
