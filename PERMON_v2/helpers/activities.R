getActivitiesDataTable <- function(stoken) {
  activitiesRaw <- get_activity_list(stoken);
  
  activities <- data.frame(name = character(), type = character(), date = character(), 
                           distance = double(), stringsAsFactors = FALSE);
  
  for (act in activitiesRaw) {
    
    activities[nrow(activities) + 1,] = list(name = act$name, 
                                             type = act$type,
                                             date = act$start_date_local, 
                                             distance = (act$distance / 1000))
    
  }
  
  return(activities);
}