activitiesPanel <- tabPanel("Activities", 
                               DT::dataTableOutput("activitiesTable"),
                               p("Shift+click to select multiple rows"),
                               actionButton("importStream", "Import streams"))