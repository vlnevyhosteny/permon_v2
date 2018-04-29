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
  
  activities <- data.frame(name = character(), type = character(), date = character(), 
                           distance = double(), stringsAsFactors = FALSE);
  
  browser()
  
  activitiesLocal <- GetActivitiesForUser(dbPath, stoken$credentials$athlete$id);
  
  for (act in activitiesLocal) {
    
    activities[nrow(activities) + 1,] = list(name = act$Activity$Name, 
                                             type = act$Activity$Type,
                                             date = act$Activity$StartDate, 
                                             distance = (act$Activity$Distance / 1000))
    
  }
  
  return(activities);
}