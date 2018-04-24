source("ui/sideBarUserInfo.R");

ui <- fluidPage(
  
  titlePanel("PERMON"),
  
  sidebarLayout(sideBarUserInfo,
    
    mainPanel(
      tabsetPanel(
        tabPanel("Activities", DT::dataTableOutput("activitiesTable")), 
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)
