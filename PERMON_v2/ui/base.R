source("ui/sideBarUserInfo.R");
source("ui/activitiesDataTable.R")
source("ui/bannisterTabPanel.R")
source("ui/activityStatsPanel.R")

ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "base.css")
  ),
  
  titlePanel("PERMON"),
  busyIndicator(),
  
  sidebarLayout(sideBarUserInfo,
    mainPanel(
      tabsetPanel(id = 'mainTabset',
        activitiesPanel, 
        bannisterPanel,
        activityStatsPanel)
    )
  )
)
