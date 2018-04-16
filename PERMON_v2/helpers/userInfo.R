renderUserImageFunc <- function(token) {
  if(is.null(token)) {
    return();
  }
  
  userImageUrl <- getUserImageFromToken(token)
  
  is.atomic(userImageUrl)
  typeof(userImageUrl)
  
  return(renderText(sprintf('<img src="%s"></img>', userImageUrl)))
}
