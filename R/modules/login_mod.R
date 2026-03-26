loginUI <- function(id) {
  ns <- NS(id)
  div(
    class = "login-screen",
    div(
      class = "login-card-shell",
      div(
        class = "card login-card",
        div(
          class = "card-body",
          h1(class = "login-title", "RIDS"),
          div(
            class = "login-form",
            textInput(ns("username"), "Username"),
            passwordInput(ns("password"), "Password")
          ),
          actionButton(ns("login"), "Sign in", class = "btn-primary login-button"),
          p(class = "login-note", "Authorised users only.")
        )
      )
    )
  )
}

loginServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    auth_state <- reactiveValues(
      logged_in = FALSE,
      user_id   = NULL,
      username  = NULL,
      role      = NULL
    )
    
    observeEvent(input$login, {
      tryCatch({
        user_row <- DBI::dbGetQuery(CON, "SELECT * FROM users WHERE username = ?",
                                    params = list(input$username))
        
        if (nrow(user_row) == 0) {
          feedbackDanger("username", show = TRUE, text = "User not found")
          return()
        }
        
        if (verify_password(input$password, user_row$password_hash)) {
          auth_state$logged_in <- TRUE
          auth_state$user_id   <- user_row$id
          auth_state$username  <- user_row$username
          auth_state$role      <- user_row$role
          showNotification("Login Successful!", type = "message", duration = 5)
        } else {
          feedbackDanger("username", show = TRUE, text = "Invalid credentials")
          feedbackDanger("password", show = TRUE, text = "")
          showNotification("Login Unsuccessful!", type = "warning", duration = 5)
        }
        
      }, error = function(e) {
        message("Login Error: ", e$message)
        showNotification("Database Error", type = "error")
      })
    })
    
    return(auth_state)
  })
}
