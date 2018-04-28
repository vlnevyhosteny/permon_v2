source("ui/sideBarUserInfo.R");

ui <- fluidPage(
  
  titlePanel("PERMON"),
  
  sidebarLayout(sideBarUserInfo,
    
    mainPanel(
      tabsetPanel(
        tabPanel("Activities", 
                 busyIndicator(),
                 DT::dataTableOutput("activitiesTable")), 
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)
