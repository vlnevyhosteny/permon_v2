getActivitiesAfter <- function(stoken, epoch) {
  url_ <- paste(url_athlete(),"/activities", sep = "")
  url_ <- paste(url_, "?after=", sep="")
  url_ <- paste(url_, epoch, sep="")
  
  dataRaw <- get_pages(url_, stoken, All=TRUE)
}

getActivitiesDataTable <- function(stoken, syncWithDb = FALSE, dbPath = NULL) {
  if(syncWithDb == FALSE) {
    activitiesRaw <- get_activity_list(stoken);
  } else {
    lastestActivity <- GetLatestActivityDateForUser(stoken$credentials$athlete$id)
    
    activitiesRaw <- getActivitiesAfter(stoken, lastestActivity);
  }
  
  InsertActivities(dbPath, activitiesRaw);
  
  activities <- data.frame(name = character(), type = character(), date = character(), 
                           distance = double(), stringsAsFactors = FALSE);
  
  #TODO: load activities from DB
  
  for (act in activitiesRaw) {
    
    activities[nrow(activities) + 1,] = list(name = act$name, 
                                             type = act$type,
                                             date = act$start_date_local, 
                                             distance = (act$distance / 1000))
    
  }
  
  return(activities);
}