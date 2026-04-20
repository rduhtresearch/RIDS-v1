sidebarUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("menu"))
}

sidebarServer <- function(id, auth_state, parent_session, current_step) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    observeEvent(input$new_ict, {
      current_step("step1")
    })
    
    observeEvent(input$new_ict, {
      current_step("step1")
      shinyjs::runjs('$("[data-value=\'tab_step1\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
    # output$menu <- renderUI({
    #   req(auth_state$logged_in)
    #   sidebarMenu(
    #     id = "sidebar",
    #     actionButton(ns('new_ict'), 'Process ICT'),
    #     menuItem("Reporting", tabName = "tab_reporting",  icon = ionicon(name = "analytics")),
    #     menuItem("Study Library",  tabName = "tab_library",  icon = ionicon(name = "book")),
    #     menuItem("Settings",  tabName = "tab_settings",  icon = ionicon(name = "settings")),
    #     menuItem("Support",  tabName = "tab_support",  icon = ionicon(name = "help-buoy")),
    #     if (auth_state$role == "admin") {
    #       menuItem("Admin", tabName = "tab_admin", icon = icon("users-cog"))
    #     },
    #     
    #     # step / workflow menu items (Hidden)
    #     tags$div(
    #       style = "display:none",
    #       menuItem("ICT", tabName = "tab_step1", icon = icon("file")),
    #       menuItem("ICT Step 2", tabName = "tab_step2", icon = icon("file")),
    #       menuItem("ICT Step 3", tabName = "tab_step3", icon = icon("file")),
    #     ),
    #     
    #     actionButton(ns('logout'), 'Logout')
    #   )
    # })
    
    output$menu <- renderUI({
      req(auth_state$logged_in)
      
      tagList(
        # ── Process ICT button ───────────────────────────────────────────────────
        div(
          style = "padding: 0.75rem; display: flex; justify-content: center;",
          actionButton(
            ns('new_ict'),
            label = tagList(icon("upload"), " Process ICT"),
            class = "btn-primary",
            style = "width: 210px; font-weight: 600;"
          )
        ),
        
        # ── Nav items ─────────────────────────────────────────────────────────────
        sidebarMenu(
          id = "sidebar",
          menuItem("Reporting",     tabName = "tab_reporting", icon = ionicon(name = "analytics")),
          menuItem("Study Library", tabName = "tab_library",   icon = ionicon(name = "book")),
          menuItem("Settings",      tabName = "tab_settings",  icon = ionicon(name = "settings")),
          menuItem("Support",       tabName = "tab_support",   icon = ionicon(name = "help-buoy")),
          if (isTRUE(auth_state$role == "admin")) {
            menuItem("Admin", tabName = "tab_admin", icon = icon("users-cog"))
          },
          tags$div(
            style = "display:none",
            menuItem("ICT",        tabName = "tab_step1", icon = icon("file")),
            menuItem("ICT Step 2", tabName = "tab_step2", icon = icon("file")),
            menuItem("ICT Step 3", tabName = "tab_step3", icon = icon("file")),
            menuItem("ICT Step 4", tabName = "tab_step4", icon = icon("file"))
          )
        ),
        
        # ── Logout button ─────────────────────────────────────────────────────────
        div(
          style = "padding: 0.75rem; position: absolute; bottom: 0; width: 100%; display: flex; justify-content: center;",
          actionButton(
            ns('logout'),
            label = tagList(icon("sign-out-alt"), " Logout"),
            class = "btn-outline-danger",
            style = "width: 210px; font-weight: 600;"
          )
        )
      )
    })
    
    observeEvent(input$logout, {
      print('working')
      auth_state$logged_in = FALSE
    })
    
    observeEvent(input$new_ict, {
      shinyjs::runjs('$("[data-value=\'tab_step1\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}