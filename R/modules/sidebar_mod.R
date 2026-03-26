sidebarUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("menu"))
}

sidebarServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    output$menu <- renderUI({
      req(auth_state$logged_in)
      sidebarMenu(
        id = "sidebar",
        menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("gauge")),
        menuItem("Settings",  tabName = "tab_settings",  icon = icon("gears"))
      )
    })
  })
}