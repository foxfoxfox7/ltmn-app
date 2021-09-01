setwd('C:/Users/kiera/Projects/ltmn-app/app/')

ui <- dashboardPage(
  
  #tags$head(
  #  tags$link(rel = "stylesheet", type = "text/css", href = "www/bootstrap.css")
  #),
  
  
  dashboardHeader(title="Long-Term Monitong Network feature map"),
  
  dashboardSidebar(
    
    uiOutput("site_output"),
    
    sidebarMenu(id = "tab", 
                menuItem("Single year feature", tabName = "1"),
                menuItem("Plant species", tabName = "2")
    ),
    
    
    uiOutput("feature_output"),
    uiOutput("habitat_output")),
    
  dashboardBody(
    leafletOutput("coolplot", height = 700)
    )
  
)