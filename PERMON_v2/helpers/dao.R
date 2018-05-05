CreateNewUserIfNotExist <- function(dbPath, athleteId) {
  library(sqldf)
  
  db <- dbConnect(SQLite(), dbname=dbPath)
  query <- paste("select count(*) from User where Id =", athleteId, ";")
  
  userCount <- as.integer(dbGetQuery(db, query))
  
  if(userCount == 0) {
    query <- paste("insert into User values (", athleteId, ", 180, 40, 1.0, 1.8, 49, 11, 0);")
    dbExecute(db, query)
    
    print("User created")
  } else {
    print("User already exist")
  }
}

GetLatestActivityDateForUser <- function(dbPath, athleteId) {
  db <- dbConnect(SQLite(), dbname=dbPath);
  query <- paste('select max(StartDate) as StartDate from Activity where IdUser = ', athleteId,';');
  result <- dbGetQuery(db, query);
  
  return(as.integer(result$StartDate));
}

InsertActivity <- function(dbPath, activity) {
  db <- dbConnect(SQLite(), dbname=dbPath)
  query <- paste("select * from Activity where Id =", Activity$Id, ";")
  
  DBActivity <- dbGetQuery(db, query)
  
  if(nrow(DBActivity) > 0) {
    #Update activity
    query <- paste("delete from Activity where Id =", Activity$Id)
    DBActivity <- dbExecute(db, query)
  } 
  
  query <- paste('insert into Activity values (', Activity$Id, ',', Activity$AthleteId, ',\"',
                 Activity$Type, '\",\"', Activity$Name, '\",', Activity$Distance, ',',Activity$ElapsedTime ,',', Activity$StartDateEpoch,', 1, NULL);')
  
  dbExecute(db, query)
}

InsertActivities <- function(dbPath, activities) {
  if(length(activities) > 0) {
    db <- dbConnect(SQLite(), dbname=dbPath)
    query <- "";
    
    dbExecute(db, "BEGIN")
    
    for(activity in activities) {
      StartDate <- as.integer(as.POSIXct(strptime(activity$start_date_local, "%Y-%m-%dT%XZ")));
      Name <- gsub("'", "", activity$name)
      
      query <- paste('insert into Activity SELECT ', activity$id, ',', activity$athlete$id, ',\'',
                     activity$type, '\',\'', Name, '\',', activity$distance, ',',activity$elapsed_time 
                   ,',', StartDate,', 1, NULL WHERE NOT EXISTS(SELECT 1 FROM Activity WHERE Id='
                   , activity$id, ');');
      
      dbExecute(db, query)
    }
    
    dbExecute(db, "END")
  }
}

GetActivitiesForUser <- function(dbPath, athleteId, withStreams = FALSE) {
  ## Get list of activities from ./Data.db by Activity Ids.
  db <- dbConnect(SQLite(), dbname=dbPath);
  
  #Activity
  query <- paste("select a.*, case when p.Id is not null then 1 else 0 end as HasStream  from Activity",
                 " a left join ActivityPoint p on p.ActivityId = a.Id where IdUser in (", athleteId, ') group by a.Id;');
  
  result <- dbGetQuery(db, query);
  
  #Streams
  #query <- paste("select * from ActivityPoint where ActivityId in (", IdsInString, ');');
  #Streams <- dbGetQuery(db, query);
  
  Activities <- vector("list", nrow(result));
  for(i in 1:nrow(result)) {
    activity <- result[i,];
    UserId <- result[i,'IdUser'];
    #Stream <- Streams[Streams$ActivityId == result[i, 'Id'],]
    Activity <- list(UserId = UserId, Activity = activity);
    
    Activities[i] <- list(Activity);
  }
  
  return(Activities);
}

InsertStream <- function(dbPath, Stream, ActivityId) {
  db <- dbConnect(SQLite(), dbname=dbPath)
  
  query <- paste("select count(*) from ActivityPoint where ActivityId =", ActivityId, ";")
  
  streamCount <- as.integer(dbGetQuery(db, query))
  
  if(streamCount > 0) {
    query <- paste("delete from ActivityPoint where ActivityId=", ActivityId, ";")
    dbExecute(db, query)
  }

  RSQLite::dbWriteTable(db, "ActivityPoint", playStream, append = TRUE, row.names=FALSE)
}