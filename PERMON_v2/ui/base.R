source("ui/sideBarUserInfo.R");
source("ui/activitiesDataTable.R")

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "base.css")
  ),
  
  titlePanel("PERMON"),
  
  sidebarLayout(sideBarUserInfo,
    
    mainPanel(
      tabsetPanel(
        activitiesPanel, 
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)
