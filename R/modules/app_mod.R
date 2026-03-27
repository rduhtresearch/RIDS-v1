appUI <- function(id) {
  tabItems(
    tabItem("tab_dashboard", ""),
    tabItem("tab_settings",  ""),
    tabItem("tab_step1", step1_UI())
  )
}

appServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    
    shared_state <- reactiveValues(
      scenario_id  = NULL,
      upload_meta  = NULL,
      raw_ict      = NULL,
      processed_ict = NULL,
      edge_templates = NULL
    )
    
    
  })
}