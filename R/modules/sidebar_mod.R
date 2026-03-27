sidebarUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("menu"))
}

sidebarServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    output$menu <- renderUI({
      req(auth_state$logged_in)
      sidebarMenu(
        id = "sidebar",
        actionButton(ns('new'), 'Process ICT'),
        menuItem("Reporting", tabName = "tab_reporting",  icon = ionicon(name = "analytics")),
        menuItem("Study Library",  tabName = "tab_library",  icon = ionicon(name = "book")),
        menuItem("Settings",  tabName = "tab_settings",  icon = ionicon(name = "settings")),
        menuItem("Support",  tabName = "tab_support",  icon = ionicon(name = "help-buoy")),
        actionButton(ns('logout'), 'Logout')
      )
    })
    
    observeEvent(input$logout, {
      print('working')
      auth_state$logged_in = FALSE
    })
    
  })
}