renderActivityStats <- function(dbPath, activityId, statType = 'alt') {
  tryCatch({
    allStatTypes = c('alt', 'grade', 'speed');
    if(!(statType %in% allStatTypes)) {
      return();
    }
    
    activity = GetActivity(activityId, dbPath);
    
    plotData <- data.frame(lat=foo$Stream$Lat, lon=foo$Stream$Lng, distance=foo$Stream$Distance, ele=foo$Stream$Alt)
    
    f = 0.2
    bbox <- ggmap::make_bbox(plotData$lon, plotData$lat, f = f)
    
    maptype = 'satellite'
    map <- suppressWarnings(suppressMessages(ggmap::get_map(bbox, maptype = maptype)))
    
    plot_data <- data.frame(x = plotData$lon, value = plotData$lat, value = plotData$ele)
    
    ggmap::ggmap(map) + geom_point(data = data, aes(x = lon, y = lat, color = ele, group = ele), lwd=0.3) + scale_color_gradient2(low = 'green', mid = 'yellow', high = 'red',
                              name = 'Elevation', midpoint = median(data$ele))
  }, error = function(e) {
    loggit("ERROR", paste("Problem with calculating activity stats",
           error = ExceptionToString(e), 
           file = "activityStatsController.R"));
    #TODO: Message
  });
}