source("ui/sideBarUserInfo.R");

ui <- fluidPage(
  
  titlePanel("PERMON"),
  
  sidebarLayout(sideBarUserInfo,
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")), 
        tabPanel("Summary", verbatimTextOutput("summary")), 
        tabPanel("Table", tableOutput("table"))
      )
    )
  )
)
