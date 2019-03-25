getActivitiesAfterStrava <- function(stoken, epoch) {
  url_ <- paste(url_athlete(),"/activities", sep = "")
  url_ <- paste(url_, "?after=", sep="")
  url_ <- paste(url_, epoch, sep="")
  
  dataRaw <- get_pages(url_, stoken, All=TRUE)
}

getActivitiesDataTable <- function(stoken, syncWithDb = FALSE, dbPath = NULL) {
  if(syncWithDb == FALSE) {
    activitiesRaw <- get_activity_list(stoken);
  } else {
    lastestActivity <- GetLatestActivityDateForUser(dbPath, stoken$credentials$athlete$id)
    
    loggit("INFO", "Activities after date requested.", date = lastestActivity, file = "activities.R");
    
    activitiesRaw <- NULL;
    
    tryCatch({
      activitiesRaw <- getActivitiesAfterStrava(stoken, lastestActivity);
      loggit("INFO", "Activities downloaded.", count = length(activitiesRaw), file = "activities.R");
    }, error = function(e) {
      loggit("ERROR", "Problem with downloading data", error = ExceptionToString(e), file = "activities.R", echo = TRUE);
      
      showNotification(
        "Problem with downloading new data.",
        duration = 10, 
        type = "error"
      )
    });
  }
  
  if(is.null(activitiesRaw) == FALSE) {
    tryCatch({
      InsertActivities(dbPath, activitiesRaw);
      loggit("INFO", "Downloaded activities insert to DB.", file = "activities.R");
    }, error = function(e) {
      loggit("ERROR", "Problem with inserting new activities", error = ExceptionToString(e), file = "activities.R");
      
      showNotification(
        "Problem with downloading new data.",
        duration = 10, 
        type = "error"
      )
    })
  }
  
  activities <- data.frame(Id = numeric(), Name = character(), Type = character(), Date = character(), 
                           Distance = double(), HasStream = character(), stringsAsFactors = FALSE);
  
  tryCatch({
    activitiesLocal <- GetActivitiesForUser(dbPath, stoken$credentials$athlete$id);
    loggit("INFO", "Activities selected from DB.", count = length(activitiesLocal), file = "activities.R");
  }, error = function(e) {
    loggit("ERROR", "Problem with selecting activities from DB", error = ExceptionToString(e), file = "activities.R");
    
    showNotification(
      "Problem with getting data.",
      duration = 10, 
      type = "error"
    )
    
    return();
  })
  
  for (act in activitiesLocal) {
    HasStream <- "FALSE";
    if(act$Activity$HasStream) {
      HasStream <- "TRUE";
    }
    
    activities[nrow(activities) + 1,] = list(Id = as.numeric(act$Activity$Id),
                                             Name = act$Activity$Name, 
                                             Type = act$Activity$Type,
                                             Date = convertEpochToDateTime(act$Activity$StartDate), 
                                             Distance = paste(round((act$Activity$Distance / 1000), 2), " km"),
                                             HasStream = HasStream);
    
  }
  
  activities <- activities[order(nrow(activities):1),];
  
  showNotification(
    "All activities are downloaded.",
    duration = 2, 
    type = "message"
  )
  
  return(activities);
}

downloadActivitiesStreams <- function(stoken, selection, activitiesAll, dbPath) {
  streamTypes <- list("latlng","altitude","heartrate","time","grade_smooth")
  
  for(index in selection) {
    tryCatch({
      stream <- get_streams(stoken, activitiesAll[index,]$Id, types = streamTypes);
      stream <- convertStreamRawToDataFrame(stream, activitiesAll[index,]$Id);
      
      InsertStream(dbPath, stream, activitiesAll[index,]$Id);
      
      loggit("INFO", paste("Activity stream has been downloaded and inserted.", activityId = activitiesAll[index,]$Id),
             file = "activities.R");
      
      showNotification(
        paste("Stream of " , activitiesAll[index,]$Name , " downloaded"),
        duration = 2, 
        type = "message"
      )
    }, error = function(e) {

      loggit("ERROR", paste("Problem with downloading activity stream from DB", activityId = activitiesAll[index,]$Id),
             error = ExceptionToString(e), 
             file = "activities.R");
      
      showNotification(
        paste("Problem with downloading stream for ", activitiesAll[index,]$Name),
        duration = 10, 
        type = "error"
      )
    })
  }
}

convertStreamRawToDataFrame <- function(streamRaw, activityId) {
  rows <- streamRaw[[1]]$original_size;
  
  stream <- data.frame(Id = integer(rows), Lat = character(rows), Lng = character(rows), Time = character(rows),
                       Distance = character(rows), Alt = character(rows), Heartrate = character(rows),
                       Grade = character(rows), ActivityId = integer(rows), stringsAsFactors = FALSE);
  
  Id <- rep(NA, rows);
  Lat <- rep(-1, rows);
  Lng <- rep(-1, rows);
  Time <- rep(-1, rows);
  Distance <- rep(-1, rows);
  Alt <- rep(-1, rows);
  Heartrate <- rep(-1, rows);
  Grade <- rep(0, rows);
  ActivityId <- rep(activityId, rows);
  
  for(j in 1:length(streamRaw)) {
    if(streamRaw[[j]]$type == 'latlng') {
      Lat = lapply(streamRaw[[j]]$data, function(x) x[[1]]);
      Lng = lapply(streamRaw[[j]]$data, function(x) x[[2]]);
    } else if(streamRaw[[j]]$type == 'time') {
      Time = streamRaw[[j]]$data;
    } else if(streamRaw[[j]]$type == 'distance') {
      Distance = streamRaw[[j]]$data;
    } else if(streamRaw[[j]]$type == 'altitude') {
      Alt = streamRaw[[j]]$data;
    } else if(streamRaw[[j]]$type == 'grade_smooth') {
      Grade = streamRaw[[j]]$data;
    } else if(streamRaw[[j]]$type == 'heartrate') {
      Heartrate = streamRaw[[j]]$data;
    }
  }
  
  stream$Id <- Id;
  stream$Lat <- Lat;
  stream$Lng <- Lng;
  stream$Time <- Time;
  stream$Distance <- Distance;
  stream$Alt <- Alt;
  stream$Heartrate <- Heartrate;
  stream$Grade <- Grade;
  stream$ActivityId <- ActivityId;
  
  return(stream);
}
