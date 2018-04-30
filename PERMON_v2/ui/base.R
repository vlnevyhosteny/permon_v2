source("ui/sideBarUserInfo.R");
source("ui/activitiesDataTable.R")

ui <- fluidPage(
  
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
