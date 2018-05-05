renderBanister <- function(dbPath, token, input) {
    tryCatch({
      from = format(as.Date(strsplit(as.character(input$banisterDateRange), " ")[[1]]),  "%d/%m/%Y")
      to = format(as.Date(strsplit(as.character(input$banisterDateRange), " ")[[2]]),  "%d/%m/%Y")
      
      loggit("INFO", paste("Banister model requested for user ", 
                           token$credentials$athlete$id, " from ", from, " to ", to, "."));
      
      banister_data <- PerformanceBanisterModel(dbPath, from, to, token$credentials$athlete$id, Male = input$sex, k1 = input$k1,
                                              k2 = input$k2, r1 = input$r1, r2 = input$r2,
                                              p0 = input$p0);
      
      Plot(banister_data)
    }, error = function(e) {
      loggit("ERROR", paste("Problem with calculating Banister model", activityId = activitiesAll[index,]$Id),
             error = ExceptionToString(e), 
             file = "activities.R");
      #TODO: Message
    })
}