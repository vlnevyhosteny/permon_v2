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
    
    activitiesRaw <- getActivitiesAfterStrava(stoken, lastestActivity);
  }
  
  InsertActivities(dbPath, activitiesRaw);
  
  activities <- data.frame(Id = numeric(), Name = character(), Type = character(), Date = character(), 
                           Distance = double(), stringsAsFactors = FALSE);
  
  activitiesLocal <- GetActivitiesForUser(dbPath, stoken$credentials$athlete$id);
  
  browser()  

  for (act in activitiesLocal) {
    activities[nrow(activities) + 1,] = list(Id = act$Activity$Id,
                                             Name = act$Activity$Name, 
                                             Type = act$Activity$Type,
                                             Date = convertEpochToDateTime(act$Activity$StartDate), 
                                             Distance = paste(round((act$Activity$Distance / 1000), 2), " km"))
    
  }
  
  return(activities);
}