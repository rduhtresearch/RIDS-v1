appUI <- function(id) {
  tabItems(
    tabItem("tab_dashboard", ""),
    tabItem("tab_settings",  ""),
    tabItem("tab_step1", step1_UI(NS(id, "step1"))),
    tabItem("tab_step2", step2_UI(NS(id, "step2"))),
    tabItem("tab_step2", step2_UI(NS(id, "step3")))
  )
}

appServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    
    step <- reactiveVal(0) 
    
    shared_state <- reactiveValues(
      scenario_id     = NULL,
      edge_id         = NULL,
      filename        = NULL,
      upload_meta     = NULL,
      raw_ict         = NULL,
      processed_ict   = NULL,
      edge_templates  = NULL,
      timestamp       = NULL
    )
    
    step1_Server("step1", auth_state, shared_state)
    step2_Server("step2", auth_state, shared_state)
    step2_Server("step3", auth_state, shared_state)
    
    
  })
}