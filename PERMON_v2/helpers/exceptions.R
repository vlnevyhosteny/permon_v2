ExceptionToString <- function(e) {
  result <- "Message: ";
  
  if(is.null(e$message) == FALSE) {
    result <- paste(result, e$message);
  } else {
    result <- paste(result, "no message");
  }
  
  result <- paste(result, "\n", "Call: ");
  if(is.null(e$call) == FALSE) {
    result <- paste(result, e$call);
  }
  
  return(result);
}