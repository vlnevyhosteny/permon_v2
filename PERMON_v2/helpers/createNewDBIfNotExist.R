if(!file.exists(Config$app$dbPath)) {
  system(paste("echo '.exit' | sqlite3 -init resources/CreateDataDB.sql ", Config$app$dbPath, sep = ""))
  print("DB created")
} else {
  print("DB already exist")
}