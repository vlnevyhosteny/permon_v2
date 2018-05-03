sideBarUserInfo <- sidebarPanel(
  htmlOutput("UserImage"),
  verbatimTextOutput("User"),
  
  shiny::actionButton(inputId='logout', label="Login", 
                      icon = icon("sign-in-alt"), 
                      onclick = sprintf("location.replace('%s', '_blank')", oauth2.0_authorize_url(api, app, scope = scope))),
  
  shiny::actionButton(inputId='login', label="Logout", 
                      icon = icon("sign-out-alt"), 
                      onclick = sprintf("location.replace('%s', '_blank')", oauth2.0_authorize_url(api, app, scope = scope))),
  busyIndicator()
)