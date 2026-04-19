loginUI <- function(id) {
  ns <- NS(id)
  div(
    id = "login-overlay",
    class = "login-screen",
    div(
      class = "login-card-shell",
      div(
        class = "card login-card",
        div(
          class = "card-body",
          h1(class = "login-title", "RIDS"),
          
          # ── Login state ──────────────────────────────────────────────────
          div(
            id = ns("login_view"),
            div(
              class = "login-form",
              textInput(ns("username"), "Username"),
              passwordInput(ns("password"), "Password")
            ),
            actionButton(ns("login"), "Sign in", class = "btn-primary login-button"),
            p(class = "login-note",
              "First time?",
              actionLink(ns("show_setup"), "Set your password")
            )
          ),

# -------------------------------------------------------------------------

          
          # ── Setup state ──────────────────────────────────────────────────
          shinyjs::hidden(
            div(
              id = ns("setup_view"),
              div(
                class = "login-form",
                textInput(ns("token"), "Setup Token"),
                passwordInput(ns("new_password"), "New Password"),
                passwordInput(ns("confirm_password"), "Confirm Password")
              ),
              actionButton(ns("set_password"), "Set Password", class = "btn-primary login-button"),
              p(class = "login-note",
                actionLink(ns("show_login"), "Back to login")
              )
            )
          )
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
    
    observe({
      message("ns login_view: ", session$ns("login_view"))
      message("ns setup_view: ", session$ns("setup_view"))
    })
    
    # ── Toggle views ─────────────────────────────────────────────────────────
    observeEvent(input$show_setup, {
      shinyjs::hide("login_view")
      shinyjs::show("setup_view")
    })
    
    observeEvent(input$show_login, {
      shinyjs::hide("setup_view")
      shinyjs::show("login_view")
    })
    
    # ── Login ─────────────────────────────────────────────────────────────────
    observeEvent(input$login, {
      tryCatch({
        user_row <- DBI::dbGetQuery(CON,
                                    "SELECT * FROM users WHERE username = ?",
                                    params = list(input$username)
        )
        
        if (nrow(user_row) == 0) {
          feedbackDanger("username", show = TRUE, text = "User not found")
          return()
        }
        
        if (is.na(user_row$password_hash) || is.null(user_row$password_hash)) {
          feedbackDanger("username", show = TRUE, text = "Account not activated — use setup token")
          return()
        }
        
        if (verify_password(input$password, user_row$password_hash)) {
          dbExecute(CON,
                    "UPDATE users SET last_login = CURRENT_TIMESTAMP WHERE id = ?",
                    params = list(user_row$id)
          )
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
    
    # ── Set password ──────────────────────────────────────────────────────────
    observeEvent(input$set_password, {
      
      # Validate inputs
      if (input$token == "") {
        feedbackDanger("token", show = TRUE, text = "Required")
        return()
      }
      if (nchar(input$new_password) < 8) {
        feedbackDanger("new_password", show = TRUE, text = "Minimum 8 characters")
        return()
      }
      if (input$new_password != input$confirm_password) {
        feedbackDanger("confirm_password", show = TRUE, text = "Passwords do not match")
        return()
      }
      
      # Consume token
      token_row <- consume_token(input$token)
      
      if (is.null(token_row)) {
        feedbackDanger("token", show = TRUE, text = "Invalid or expired token")
        return()
      }
      
      # Set password
      tryCatch({
        dbExecute(CON,
                  "UPDATE users SET password_hash = ? WHERE id = ?",
                  params = list(hash_password(input$new_password), token_row$user_id)
        )
        
        showNotification("Password set — please log in", type = "message", duration = 5)
        shinyjs::hide("setup_view")
        shinyjs::show("login_view")
        
      }, error = function(e) {
        message("Set password error: ", e$message)
        showNotification("Failed to set password", type = "error")
      })
    })
    
    return(auth_state)
  })
}