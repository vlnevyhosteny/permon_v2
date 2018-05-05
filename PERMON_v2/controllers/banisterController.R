renderBanister <- function(dbPath, token, input) {
    from = format(as.Date(strsplit(as.character(input$banisterDateRange), " ")[[1]]),  "%d/%m/%Y")
    to = format(as.Date(strsplit(as.character(input$banisterDateRange), " ")[[2]]),  "%d/%m/%Y")
    
    banister_data <- PerformanceBanisterModel(dbPath, from, to, token$credentials$athlete$id, Male = input$sex, k1 = input$k1,
                                              k2 = input$k2, r1 = input$r1, r2 = input$r2,
                                              p0 = input$p0)
    
    Plot(banister_data)
}