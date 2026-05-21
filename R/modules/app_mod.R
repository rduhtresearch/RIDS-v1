appUI <- function(id) {
  tagList(
    progressUI(NS(id, "progress")),
    tabItems(
      tabItem("tab_dashboard", ""),
      tabItem("tab_reporting", reportingUI(NS(id, "reporting"))),
      tabItem("tab_settings",  settingsUI(NS(id, "settings"))),
      tabItem("tab_library",   libraryUI(NS(id, "library"))),
      tabItem("tab_study",     studyWorkspaceUI(NS(id, "study_workspace"))),
      tabItem("tab_support",   supportUI(NS(id, "support"))),
      tabItem("tab_step1", step1_UI(NS(id, "step1"))),
      tabItem("tab_step2", step2_UI(NS(id, "step2"))),
      tabItem("tab_step3", step3_UI(NS(id, "step3"))),
      tabItem("tab_step4", step4_UI(NS(id, "step4"))),
      tabItem("tab_admin", uiOutput(NS(id, "admin_tab")))
    )
  )
}

appServer <- function(id, auth_state, current_step) {
  moduleServer(id, function(input, output, session) {
    shared_state <- reactiveValues(
      scenario_id     = NULL,
      study_site      = NULL,
      edge_id         = NULL,
      cpms_id         = NULL,
      study_name      = NULL,
      upload_id       = NULL,
      filename        = NULL,
      upload_meta     = NULL,
      raw_ict         = NULL,
      posting_plan    = NULL,
      processed_ict   = NULL,
      edge_templates  = NULL,
      current_step    = NULL,
      timestamp       = NULL,
      current_study   = NULL
    )

    session$userData$reset_app_state <- function() {
      shared_state$scenario_id <- NULL
      shared_state$study_site <- NULL
      shared_state$edge_id <- NULL
      shared_state$cpms_id <- NULL
      shared_state$study_name <- NULL
      shared_state$upload_id <- NULL
      shared_state$filename <- NULL
      shared_state$upload_meta <- NULL
      shared_state$raw_ict <- NULL
      shared_state$posting_plan <- NULL
      shared_state$processed_ict <- NULL
      shared_state$edge_templates <- NULL
      shared_state$current_step <- NULL
      shared_state$timestamp <- NULL
      shared_state$current_study <- NULL
      current_step(NULL)
    }
    
    step1_Server("step1", auth_state, shared_state, current_step)
    step2_Server("step2", auth_state, shared_state, current_step)
    step3_Server("step3", auth_state, shared_state, current_step)
    step4_Server("step4", auth_state, shared_state, current_step)
    progressServer("progress", current_step)
    reportingServer("reporting", auth_state)
    settingsServer("settings", auth_state)
    libraryServer("library", auth_state, shared_state)
    supportServer("support", auth_state)
    studyWorkspaceServer("study_workspace", shared_state)

    output$admin_tab <- renderUI({
      if (!isTRUE(is_admin(auth_state$role))) {
        return(
          div(
            style = "padding: 1.5rem; color: #697786;",
            "Admin access is required for this area."
          )
        )
      }

      adminUI("admin")
    })
    
    observe({
      shared_state$current_step <- current_step()
    })
    
    
  })
}
