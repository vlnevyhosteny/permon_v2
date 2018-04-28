CreateNewUserIfNotExist <- function(dbPath, athleteId) {
  library(sqldf)
  
  db <- dbConnect(SQLite(), dbname=dbPath)
  query <- paste("select count(*) from User where Id =", athleteId, ";")
  
  userCount <- as.integer(dbGetQuery(db, query))
  
  if(userCount == 0) {
    query <- paste("insert into User values (", athleteId, ", 180, 40, 1.0, 1.8, 49, 11, 0);")
    dbGetQuery(db, query)
    
    print("User created")
  } else {
    print("User already exist")
  }
}

GetLatestActivityDateForUser <- function(dbPath, athleteId) {
  db <- dbConnect(SQLite(), dbname=dbPath);
  query <- paste('select StartDate from Activity where IdUser = ', athleteId,';');
  result <- dbGetQuery(db, query);
  
  if(!length(result)) {
    return(0);
  }
  
  return(as.integer(result[[1]]));
}

InsertActivity <- function(dbPath, activity) {
  db <- dbConnect(SQLite(), dbname=dbPath)
  query <- paste("select * from Activity where Id =", Activity$Id, ";")
  
  DBActivity <- dbGetQuery(db, query)
  
  if(nrow(DBActivity) > 0) {
    #Update activity
    query <- paste("delete from Activity where Id =", Activity$Id)
    DBActivity <- dbGetQuery(db, query)
  } 
  
  query <- paste('insert into Activity values (', Activity$Id, ',', Activity$AthleteId, ',\"',
                 Activity$Type, '\",\"', Activity$Name, '\",', Activity$Distance, ',',Activity$ElapsedTime ,',', Activity$StartDateEpoch,', 1, NULL);')
  
  dbGetQuery(db, query)
}

InsertActivities <- function(dbPath, activities) {
  if(length(activities) > 0) {
    db <- dbConnect(SQLite(), dbname=dbPath)
    query <- "";
    
    for(activity in activities) {
      query <- paste(query, 'insert into Activity SELECT ', Activity$Id, ',', Activity$AthleteId, ',\"',
                   Activity$Type, '\",\"', Activity$Name, '\",', Activity$Distance, ',',Activity$ElapsedTime 
                   ,',', Activity$StartDateEpoch,', 1, NULL WHERE NOT EXISTS(SELECT 1 FROM Activity WHERE Id='
                   , Activity$Id, ');');
    }
    
    print(query);
    
    dbGetQuery(db, query)
  }
}