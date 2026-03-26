source("R/utils/auth.r")

loginUI <- function(id) {
  ns <- NS(id)
  dashboardPage(
    header = dashboardHeader(disable = TRUE),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      tags$head(tags$style(HTML(".main-header { display: none !important; }
                             .content-wrapper, .main-sidebar { padding-top: 0 !important; } body { overflow: hidden; }"))),
    tagList(
      useShinyFeedback(),
      div(
        class = "d-flex justify-content-center align-items-center vh-100",
        bs4Card(
          width = 4, 
          collapsible = FALSE,
          title = "RIDS",
          textInput(ns("username"), 'Username'),
          passwordInput(ns("password"), "Password"),
          actionButton(ns("login"), "Login")
        ),
      )
     )
    )
  )
}

loginServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # auth_state state flags
      auth_state <- reactiveValues(
        logged_in = FALSE,
        user_id   = NULL,
        username  = NULL,
        role      = NULL
      )
      
      observeEvent(input$login, {
        # Check user exists ONLY when button is clicked
        user_name <- input$username
        
        tryCatch({
          user_row <- DBI::dbGetQuery(CON, "SELECT * from users WHERE username = ?", 
                                      params = list(user_name))
          print(user_row)
          
          if (nrow(user_row) == 0) {
            feedbackDanger("username", TRUE, "User not found")
            return()
          } 
          
          if(verify_password(input$password, user_row$password_hash)) {
            auth_state$logged_in = TRUE
            auth_state$user_id <- user_row$id
            auth_state$username <- user_row$username
            auth_state$role <- user_row$role
    
            showNotification("Login Successful!", type = "message", duration = 5)
          } else {
            auth_state$logged_in = FALSE
            
            feedbackDanger(inputId = 'username', session = session, 
                           show = !auth_state$logged_in, text = '')
            feedbackDanger(inputId = 'password', session = session, 
                           show = !auth_state$logged_in, text = '')
            
            showNotification("Login Unsuccessful!", type = "warning", duration = 5)
          }
          
        }, error = function(e) {
          message("Login Error: ", e$message) 
          showNotification(paste("Database Error:"), type = "error")
        }) 
      }) 
      
      return(auth_state) 
    }
  ) 
}

