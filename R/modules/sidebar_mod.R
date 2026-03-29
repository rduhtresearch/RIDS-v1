sidebarUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("menu"))
}

sidebarServer <- function(id, auth_state, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    output$menu <- renderUI({
      req(auth_state$logged_in)
      sidebarMenu(
        id = "sidebar",
        actionButton(ns('new_ict'), 'Process ICT'),
        menuItem("Reporting", tabName = "tab_reporting",  icon = ionicon(name = "analytics")),
        menuItem("Study Library",  tabName = "tab_library",  icon = ionicon(name = "book")),
        menuItem("Settings",  tabName = "tab_settings",  icon = ionicon(name = "settings")),
        menuItem("Support",  tabName = "tab_support",  icon = ionicon(name = "help-buoy")),
        
        # step / workflow menu items (Hidden)
        tags$div(
          style = "display:none",
          menuItem("ICT", tabName = "tab_step1", icon = icon("file")),
          menuItem("ICT Step 2", tabName = "tab_step2", icon = icon("file")),
          menuItem("ICT Step 3", tabName = "tab_step3", icon = icon("file")),
        ),
        
        actionButton(ns('logout'), 'Logout')
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