appUI <- function(id) {
  tagList(
    progressUI(NS(id, "progress")),
    tabItems(
      tabItem("tab_dashboard", ""),
      tabItem("tab_settings",  ""),
      tabItem("tab_library",   libraryUI(NS(id, "library"))),
      tabItem("tab_step1", step1_UI(NS(id, "step1"))),
      tabItem("tab_step2", step2_UI(NS(id, "step2"))),
      tabItem("tab_step3", step3_UI(NS(id, "step3"))),
      tabItem("tab_step4", step4_UI(NS(id, "step4"))),
      tabItem("tab_admin", adminUI("admin"))
    )
  )
}

appServer <- function(id, auth_state, current_step) {
  moduleServer(id, function(input, output, session) {
    
    step <- reactiveVal(0) 
    
    shared_state <- reactiveValues(
      scenario_id     = NULL,
      edge_id         = NULL,
      cpms_id         = NULL,
      filename        = NULL,
      upload_meta     = NULL,
      raw_ict         = NULL,
      posting_plan    = NULL,
      processed_ict   = NULL,
      edge_templates  = NULL,
      current_step    = NULL,
      timestamp       = NULL
    )
    
    step1_Server("step1", auth_state, shared_state, current_step)
    step2_Server("step2", auth_state, shared_state, current_step)
    step3_Server("step3", auth_state, shared_state, current_step)
    step4_Server("step4", auth_state, shared_state, current_step)
    progressServer("progress", current_step)
    libraryServer("library", auth_state)
    
    observe({
      shared_state$current_step <- current_step()
    })
    
    
  })
}
