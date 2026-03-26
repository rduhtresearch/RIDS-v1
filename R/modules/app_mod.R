appUI <- function(id) {
  tabItems(
    tabItem("tab_dashboard", ""),
    tabItem("tab_settings",  "")
  )
}

appServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    # child modules go here
  })
}