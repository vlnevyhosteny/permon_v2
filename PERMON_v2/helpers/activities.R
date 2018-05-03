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
      loggit("ERROR", "Problem with downloading data", error = ExceptionToString(e), file = "activities.R");
      #TODO: message 
    });
  }
  
  if(is.null(activitiesRaw) == FALSE) {
    tryCatch({
      InsertActivities(dbPath, activitiesRaw);
      loggit("INFO", "Downloaded activities insert to DB.", file = "activities.R");
    }, error = function(e) {
      loggit("ERROR", "Problem with inserting new activities", error = ExceptionToString(e), file = "activities.R");
      #TODO: message 
    })
  }
  
  activities <- data.frame(Id = numeric(), Name = character(), Type = character(), Date = character(), 
                           Distance = double(), stringsAsFactors = FALSE);
  
  tryCatch({
    activitiesLocal <- GetActivitiesForUser(dbPath, stoken$credentials$athlete$id);
    loggit("INFO", "Activities selected from DB.", count = length(activitiesLocal), file = "activities.R");
  }, error = function(e) {
    loggit("ERROR", "Problem with selecting activities from DB", error = ExceptionToString(e), file = "activities.R");
    return();
  })
  
  for (act in activitiesLocal) {
    activities[nrow(activities) + 1,] = list(Id = act$Activity$Id,
                                             Name = act$Activity$Name, 
                                             Type = act$Activity$Type,
                                             Date = convertEpochToDateTime(act$Activity$StartDate), 
                                             Distance = paste(round((act$Activity$Distance / 1000), 2), " km"))
    
  }
  
  activities <- activities[order(nrow(activities):1),];
  
  return(activities);
}

downloadActivitiesStreams <- function(stoken, selection, activitiesAll, dbPath) {
  streamTypes <- list("latlng","altitude","heartrate","time","grade_smooth")
  
  for(index in selection) {
    tryCatch({
      stream <- get_streams(stoken, activitiesAll[index,]$Id, types = streamTypes);
      stream <- convertStreamRawToDataFrame(stream);
      
      InsertStream(dbPath, stream, activitiesAll[index,]$Id);
      
      loggit("INFO", paste("Activity stream has been downloaded and inserted.", activityId = activitiesAll[index,]$Id),
             file = "activities.R");
      
      #TODO: progress
    }, error = function(e) {

      loggit("ERROR", paste("Problem with downloading activity stream from DB", activityId = activitiesAll[index,]$Id),
             error = ExceptionToString(e), 
             file = "activities.R");
      #TODO: Message
    })
  }
}

convertStreamRawToDataFrame <- function(streamRaw) {
  stream <- data.frame(Lat = character(), Lng = character(), Time = character(), Distance = character(),
                       Alt = character(), Heartrate = character(), Grade = character(),
                       stringsAsFactors = FALSE);
  
  for(i in 1:streamRaw[[1]]$original_size) {
    Lat <- "-1"; Lng <- "-1"; Time <- "-1"; Distance <- "-1"; Alt <- "-1"; Heartrate <- "-1"; Grade <- "0";
    
    for(j in 1:length(streamRaw)) {
      if(streamRaw[[j]]$type == 'latlng') {
        Lat = streamRaw[[j]]$data[[i]][[1]];
        Lng = streamRaw[[j]]$data[[i]][[2]];
      } else if(streamRaw[[j]]$type == 'time') {
        Time = streamRaw[[j]]$data[[i]];
      } else if(streamRaw[[j]]$type == 'distance') {
        Distance = streamRaw[[j]]$data[[i]];
      } else if(streamRaw[[j]]$type == 'altitude') {
        Alt = streamRaw[[j]]$data[[i]];
      } else if(streamRaw[[j]]$type == 'grade_smooth') {
        Grade = streamRaw[[j]]$data[[i]];
      } else if(streamRaw[[j]]$type == 'heartrate') {
        Heartrate = streamRaw[[j]]$data[[i]];
      }
    }
    
    stream[nrow(stream) + 1,] = list(Lat = Lat,
                                     Lng = Lng,
                                     Time = Time,
                                     Distance = Distance,
                                     Alt = Alt,
                                     Heartrate = Heartrate,
                                     Grade = Grade);
  }
  
  return(stream);
}
