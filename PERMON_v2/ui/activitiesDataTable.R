activitiesPanel <- tabPanel("Activities", 
                               busyIndicator(),
                               DT::dataTableOutput("activitiesTable"),
                               p("Shift+click to select multiple rows"),
                               actionButton("importStream", "Import streams"),
                               actionButton("activityStats", "Show stats")
)