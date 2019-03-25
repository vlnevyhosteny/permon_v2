has_auth_code <- function(params) {
  return(!is.null(params$code))
}

getUserFromToken <- function(token) {
  if(is.null(token)) {
    return("Not logged");
  } else {
    return(token$credentials$athlete$email);
  }
}

getUserImageFromToken <- function(token) {
  if(is.null(token)) {
    return("");
  } else {
    return(token$credentials$athlete$profile);
  }
}

isTokenSet <- function(session) {
  if(is.null(session) == FALSE) {
    if(is.null(session$userData$stoken) == FALSE) {
      token <- session$userData$stoken;
      
      if(is.null(token$credentials) == FALSE) {
        return(TRUE);
      }
    }
  }
  
  return(FALSE);
}

APP_URL <- if (interactive()) {
  options(shiny.port = 8100)
  "http://localhost:8100/"
} else {
  "http://localhost:8100/"
}

app <- oauth_app(Config$app$name,
                 key = Config$stravaAPI$clientId,
                 secret = Config$stravaAPI$secret,
                 redirect_uri = APP_URL
)

api <- oauth_endpoint(NULL, "authorize", "token", base_url = "https://www.strava.com/oauth")

scope <- Config$stravaAPI$scope
