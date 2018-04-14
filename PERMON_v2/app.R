#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rStrava)
library(shinyjs)
library(httr)

source("helpers/configuration.R");

Config <<- getConfig("resources/configuration/app_config.json");

APP_URL <- if (interactive()) {
  options(shiny.port = 8100)
  "http://localhost:8100/"
} else {
  "https://servername/path-to-app"
}

app <- oauth_app(Config$app$name,
                 key = Config$stravaAPI$clientId,
                 secret = Config$stravaAPI$secret,
                 redirect_uri = APP_URL
)

api <- oauth_endpoint(NULL, "authorize", "token", base_url = "https://www.strava.com/oauth")

scope <- Config$stravaAPI$scope;

has_auth_code <- function(params) {
  return(!is.null(params$code))
}

getUserNameFromToken <- function(token) {
  if(is.null(token)) {
    return("Not logged");
  } else {
    return(token$credentials$athlete$username);
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

ui <- fluidPage(
  verbatimTextOutput("code"),
  
  shiny::actionButton(inputId='ab1', label="Loogin", 
                      icon = icon("th"), 
                      onclick = sprintf("location.replace('%s', '_blank')", oauth2.0_authorize_url(api, app, scope = scope))),
  
  actionButton("logout", "Odhlasit")
)

uiFunc <- function(req) {
    ui
}

server <- function(input, output, session) {

  token <- NULL;
  params <- parseQueryString(isolate(session$clientData$url_search))
  
  if(isTokenSet(session) == FALSE || has_auth_code(params)) {
    if (!has_auth_code(params)) {
      return()
    }
    
    # Manually create a token
    token <- oauth2.0_token(
      app = app,
      endpoint = api,
      credentials = oauth2.0_access_token(api, app, params$code),
      cache = FALSE
    )
    
    session$userData$stoken <- token;
    
    
  } else {
    token <- session$userData$stoken;
  }
  
  username = getUserNameFromToken(token);
  
  output$code <- renderText(username)
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)