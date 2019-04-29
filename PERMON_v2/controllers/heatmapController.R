renderHeatmap <- function(dbPath, userId, input)
  tryCatch({
    from = format(as.Date(strsplit(as.character(input$HeatmapDateRange), " ")[[1]]),  "%d/%m/%Y")
    to = format(as.Date(strsplit(as.character(input$HeatmapDateRange), " ")[[2]]),  "%d/%m/%Y")
    
    lat <- c();
    lon <- c();
    
    activitiesInRange <- GetActivitiesInDateRange(dbPath, from, to, userId);
    for(i in 1:length(activitiesInRange)) {
      if(hasStreamData(activitiesInRange[[i]]$Stream, 'Lat')
         && hasStreamData(activitiesInRange[[i]]$Stream, 'Lng')) {
        lat <- c(lat, activitiesInRange[[i]]$Stream$Lat);
        lon <- c(lon, activitiesInRange[[i]]$Stream$Lng);
      }
    }
    
    plotData <- data.frame(lat=lat, lon=lon);
    
    f = 0.1
    bbox <- ggmap::make_bbox(plotData$lon, plotData$lat, f = f)
    
    maptype = 'terrain'
    map <- suppressWarnings(suppressMessages(ggmap::get_map(bbox, source = 'osm', maptype = maptype)))
    
    if(input$HeatmapTypeRadio == '1') {
      ggmap::ggmap(map) +
        stat_density2d(data = plotData, aes(x = lon, y = lat, fill = ..density..), geom = 'tile', n = 200, contour = F, alpha = .5) +
        scale_fill_viridis(option = 'inferno')
    } else {
      ggmap::ggmap(map) +
        geom_point(aes(x = lon, y = lat), colour = '#CC0000', data = plotData, alpha=0.3, size = 0.5) +
        theme(legend.position="none")
    }
  }, error = function(e) {
    loggit("ERROR", paste("Problem with calculating heatmap", ExceptionToString(e)),
           error = ExceptionToString(e), 
           file = "activities.R");
    #TODO: Message
  })