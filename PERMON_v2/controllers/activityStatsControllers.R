renderActivityStats <- function(dbPath, activityId, statType = 'Alt') {
  tryCatch({
    allStatTypes = c('Alt', 'Heartrate', 'Grade');
    if(!(statType %in% allStatTypes)) {
      return();
    }
    
    activity <<- GetActivity(activityId, dbPath);
    
    if(hasStreamData(activity$Stream, statType) == FALSE) {
      return();
    }
    
    variable = switch(statType,
                      "Alt" = round(activity$Stream$Alt),
                      "Heartrate" = activity$Stream$Heartrate,
                      "Grade" = activity$Strean$Grade)
    
    plotData <- data.frame(lat=activity$Stream$Lat, lon=activity$Stream$Lng, variable=variable)
    
    f = 0.1
    bbox <- ggmap::make_bbox(plotData$lon, plotData$lat, f = f)
    
    maptype = 'satellite'
    map <- suppressWarnings(suppressMessages(ggmap::get_map(bbox, maptype = maptype)))
    
    ggmap::ggmap(map) + geom_point(data = plotData, aes(x = lon, y = lat, color = variable, group = variable), lwd=0.3) + scale_color_gradient2(low = 'green', mid = 'yellow', high = 'red',
                              name = statType, midpoint = median(plotData$variable))
  }, error = function(e) {
    loggit("ERROR", paste("Problem with calculating activity stats",
           error = ExceptionToString(e), 
           file = "activityStatsController.R"));
    #TODO: Message
  });
}