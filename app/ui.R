ui <- fluidPage(
  
  #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "www/bootstrap.css")
  #),
  
  #theme = "bootstrap.css",
  #theme = "bootstrap.css",
  #includeCSS("bootstrap.css"),
  
  tags$style("h1 { color: red;}"),
  h1("Hello World"),
  #headerPanel("Long-Term Monitong Network feature map"),
  

    
    # dashboardHeader(),
    # dashboardSidebar(uiOutput("site_output"),
    #                  uiOutput("feature_output")),
    # dashboardBody(leafletOutput('coolplot'))
    
    
  sidebarLayout(
    sidebarPanel(
      uiOutput("site_output"),
      uiOutput("feature_output")
    ),
    mainPanel(
      leafletOutput("coolplot")
    )
  )
)