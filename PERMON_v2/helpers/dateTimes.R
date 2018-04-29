convertEpochToDateTime <- function(epoch) {
  return(as.character(as.POSIXct(epoch, origin="1970-01-01")));
}