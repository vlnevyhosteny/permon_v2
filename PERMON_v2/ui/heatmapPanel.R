heatmapPanel <- tabPanel("Heatmap", value = 'heatmapPanel', 
                               dateRangeInput('HeatmapDateRange',
                                              label = 'Datum: ',
                                              start = Sys.Date() - 30, end = Sys.Date(),
                                              format = "dd/mm/yy"
                               ),
                               radioButtons("HeatmapTypeRadio", label = h3("Type"),
                                      choices = list("Standard" = 1, "Routes" = 2), 
                                      selected = 1),
                               actionButton("HeatmapUpdateButton", "Update"),
                               plotOutput("Heatmap", height = 800)
)