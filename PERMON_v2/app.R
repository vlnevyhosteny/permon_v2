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
library(DT)
library(shinysky)
library(sqldf)

source("helpers/configuration.R");
Config <<- getConfig("resources/configuration/app_config.json");

source("helpers/authentication.R");

source("ui/base.R");
source("helpers/userInfo.R");
source("helpers/activities.R");
source("helpers/createNewDBIfNotExist.R");
source("helpers/dao.R");

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
    
    CreateNewUserIfNotExist(Config$app$dbPath, token$credentials$athlete$id)
  } else {
    token <- session$userData$stoken;
  }
  
  output$User <- renderText(getUserFromToken(token))
  output$UserImage <- renderUserImageFunc(token);
  
  output$activitiesTable = DT::renderDataTable({
    getActivitiesDataTable(token, Config$app$dbPath, syncWithDb = TRUE)
  })
}

shinyApp(uiFunc, server)