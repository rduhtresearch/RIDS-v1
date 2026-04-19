adminUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Card(
        title  = "Create User",
        width  = 4,
        status = "primary",
        textInput(ns("new_username"), "Username"),
        selectInput(ns("new_role"), "Role", choices = c("user", "admin")),
        actionButton(ns("create_user"), "Create User", class = "btn-primary"),
        hr(),
        verbatimTextOutput(ns("token_display"))
      ),
      bs4Card(
        title  = "Users",
        width  = 8,
        status = "primary",
        reactableOutput(ns("users_table"))
      )
    )
  )
}

adminServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
 
    observe({
      if (!isTRUE(auth_state$role == "admin")) {
        shinyjs::runjs('$("[data-value=\'tab_dashboard\']").tab("show")')
        return()
      }
    })
    
    # Refresh trigger 
    refresh <- reactiveVal(0)
    
    # Users table 
    output$users_table <- renderReactable({
      refresh()
      
      users <- dbGetQuery(CON,
                          "SELECT id, username, role, created_at, last_login FROM users ORDER BY created_at DESC"
      )
      
      # Render table
      users$actions <- users$id
      
      reactable(
        users,
        striped   = TRUE,
        highlight = TRUE,
        compact   = TRUE,
        columns = list(
          id         = colDef(show = FALSE),
          username   = colDef(name = "Username"),
          role       = colDef(name = "Role"),
          created_at = colDef(name = "Created"),
          last_login = colDef(name = "Last Login"),
          actions    = colDef(
            name = "Actions",
            cell = function(value) {
              tags$button(
                "Delete",
                class = "btn btn-danger btn-sm",
                onclick = sprintf("Shiny.setInputValue('admin-delete_user', %d, {priority: 'event'})", value)
              )
            }
          )
        )
      )
    })
    
    
    # Create user
    observeEvent(input$create_user, {
      req(input$new_username != "")
      
      token <- create_user(input$new_username, input$new_role)
      
      if (is.null(token)) {
        showNotification("Failed to create user", type = "error")
        return()
      }
      
      refresh(refresh() + 1)
      updateTextInput(session, "new_username", value = "")
      showNotification("User created", type = "message")
    })
    
    # Delete User
    observeEvent(input$delete_user, {
      req(input$delete_user)
      
      user_id <- input$delete_user
      
      # Prevent self-deletion
      if (user_id == auth_state$user_id) {
        showNotification("You cannot delete your own account", type = "warning")
        return()
      }
      
      tryCatch({
        dbExecute(CON, "DELETE FROM tokens WHERE user_id = ?", params = list(user_id))
        dbExecute(CON, "DELETE FROM users WHERE id = ?", params = list(user_id))
        
        refresh(refresh() + 1)
        showNotification("User deleted", type = "message", duration = 5)
        
      }, error = function(e) {
        message("Delete user error: ", e$message)
        showNotification("Failed to delete user", type = "error")
      })
    })
    
    # Display token 
    output$token_display <- renderText({
      req(input$create_user)
      isolate({
        req(input$new_username != "")
        token <- dbGetQuery(CON,
                            "SELECT t.token FROM tokens t
           JOIN users u ON t.user_id = u.id
           WHERE u.username = ?
           ORDER BY t.created_at DESC LIMIT 1",
                            params = list(input$new_username)
        )
        if (nrow(token) == 0) return("No token found")
        paste0("Token:\n", token$token)
      })
    })
    
  })
}