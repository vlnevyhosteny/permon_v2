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
library(loggit)
library(RSQLite)
library(ggplot2)
library(htmlwidgets)

source("helpers/configuration.R");
Config <<- getConfig("resources/configuration/app_config.json");

source("helpers/authentication.R");
source("helpers/dateTimes.R");

source("ui/base.R");

source("helpers/userInfo.R");
source("helpers/activities.R");
source("helpers/createNewDBIfNotExist.R");
source("helpers/dao.R");
source("helpers/exceptions.R");

source("calc/trainingImpulsePerformanceCalc.R")

source("controllers/banisterController.R")
source("controllers/activityStatsControllers.R")

setLogFile(Config$app$logFilePath);
loggit("INFO", "PERMON has started", file = "app.R")

uiFunc <- function(req) {
    ui
}

server <- function(input, output, session) {

  # ---- AUTH ----
  
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
    
    loggit("INFO", "User logged in", User = token$credentials$athlete$id, file = "app.R");
  } else {
    token <- session$userData$stoken;
  }
  
  # --------------
  
  output$User <- renderText(getUserFromToken(token))
  output$UserImage <- renderUserImageFunc(token);
  
  activities <- getActivitiesDataTable(token, Config$app$dbPath, syncWithDb = TRUE)
  selectedActivities <<- NULL;
  
  output$activitiesTable = DT::renderDataTable({
    activities
  })
  
  observeEvent(input$importStream, {
    if(is.null(token) == FALSE) {
      downloadActivitiesStreams(token, input$activitiesTable_rows_selected, activities, Config$app$dbPath)
    }
  });
  
  observeEvent(input$activityStats, {
    if(is.null(token) == FALSE) {
      if(activities[input$activitiesTable_rows_selected[[1]], 'HasStream']) {
        selectedActivities <<- activities[input$activitiesTable_rows_selected[[1]], ];
        
        updateTabsetPanel(session, 'mainTabset', 
                          selected = 'activityStatsPanel');
        
        # add to output
        output$ActivityStats <- renderPlot({
          renderActivityStats(Config$app$dbPath, activities[input$activitiesTable_rows_selected[[1]], 'Id'])
        });
      }
    } 
  });
  
  observeEvent(input$ActivityStatsUpdateButton, {
    if(is.null(selectedActivities) == FALSE && nrow(selectedActivities) > 0) {
      selectedStreamType <- input$ActivityStatsSelect;
      
      output$ActivityStats <- renderPlot({
        renderActivityStats(Config$app$dbPath, activities[input$activitiesTable_rows_selected[[1]], 'Id'], selectedStreamType)
      });
    }
  });
  
  output$BanisterPlot <- renderPlot({
    renderBanister(Config$app$dbPath, token, input)
  });
}

shinyApp(uiFunc, server)