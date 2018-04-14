library("rjson")

getConfig <- function(pathToConfig) {
  return(fromJSON(file = pathToConfig));
}