appUI <- function(id) {
  tagList(
    progressUI(NS(id, "progress")),
    tabItems(
      tabItem("tab_dashboard", ""),
      tabItem("tab_settings",  ""),
      tabItem("tab_step1", step1_UI(NS(id, "step1"))),
      tabItem("tab_step2", step2_UI(NS(id, "step2"))),
      tabItem("tab_step3", step3_UI(NS(id, "step3"))),
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
    progressServer("progress", current_step)
    
    observe({
      current <- input$sidebar
      if (!is.null(current) && current %in% c("tab_step1", "tab_step2", "tab_step3")) {
        shared_state$current_step <- gsub("tab_", "", current)
      } else {
        shared_state$current_step <- NULL
      }
    })
    
    
  })
}