activityStatsPanel <- tabPanel("Activity stats", value = 'activityStatsPanel', 
                               selectInput('ActivityStatsSelect', 'Choose variable',
                                           c('Alt', 'Heartrate', 'Grade')),
                               actionButton("ActivityStatsUpdateButton", "Update"),
                               plotOutput("ActivityStats", height = 800)
)