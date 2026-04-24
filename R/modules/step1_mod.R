step1_UI <- function(id) {
  ns <- NS(id)
  bs4Card(
    title  = "Upload ICT Workbook",
    width  = 6,
    status = "primary",
    solidHeader = FALSE,
    selectInput(ns('scenario'), 'Select Scenario', choices = c("A", "B")),
    textInput(ns('edge_id'), 'EDGE ID'),
    textInput(ns('study_name'), 'Study Name'),
    fileInput(ns("upload"), "Choose Excel File",
              multiple = FALSE,
              accept = c(".xlsx")),
    textAreaInput(ns("notes"), "Add upload notes"),
    actionButton(ns('next_step'), 'Next: Review Costs', class = "btn-primary"),
    helpUI(ns("help"))
  )
}

step1_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    
    # ── Help ─────────────────────────────────────────────────────────────────
    helpServer("help", content = list(
      title = "Upload Help",
      sections = list(
        list(
          heading = "What is this step?",
          body    = "This step allows you to upload an ICT costing workbook and begin the RIDS pipeline."
        ),
        list(
          heading = "What file should I upload?",
          body    = "Upload the Excel (.xlsx) ICT workbook provided by your study team."
        ),
        list(
          heading = "What is a Scenario?",
          body    = "The scenario determines how costs are distributed across posting lines. Select the scenario that matches your study's commercial arrangement."
        ),
        list(
          heading = "FAQ",
          body    = "If you are unsure which scenario to select, contact your R&D finance lead."
        )
      )
    ))
    
    # ── Next step ─────────────────────────────────────────────────────────────
    observeEvent(input$next_step, {
      
      # ── Validation ───────────────────────────────────────────────────────
      feedbackDanger("edge_id", show = input$edge_id == "", text = "Required")
      feedbackDanger("upload",  show = is.null(input$upload), text = "Required")
      
      req(
        input$edge_id != "",
        input$scenario,
        !is.null(input$upload)
      )
      
      # ── Process ──────────────────────────────────────────────────────────
      timestamp     <- format(Sys.time(), "%Y%m%d_%H%M%S")
      original_name <- input$upload$name
      saved_name    <- paste0(timestamp, "_", original_name)
      saved_path    <- file.path(ICT_UPLOAD_DIR, saved_name)
      
      file.copy(input$upload$datapath, saved_path)
      
      extracted_cpms <- tryCatch({
        extract_cpms_id(saved_path)
      }, error = function(e) {
        showNotification("Failed to extract CPMS ID", type = "error")
        print(e)
        return(NULL)
      })
      
      req(extracted_cpms)
      
      DBI::dbExecute(CON,
                     "INSERT INTO meta_data 
   (cpms_id, scenario_id, edge_id, study_name, notes, uploaded_by, original_filename, saved_file_path)
   VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
                     params = list(
                       as.character(extracted_cpms),
                       input$scenario,
                       input$edge_id,
                       input$study_name,
                       input$notes,
                       auth_state$user_id,
                       original_name,
                       saved_path
                     )
      )
      
      shared_state$processed_ict <- tryCatch({
        process_workbook(input_path = saved_path, db_path = DB_DIR)
      }, error = function(e) {
        showNotification("Failed to process workbook", type = "error")
        print(e)
        return(NULL)
      })
      
      req(shared_state$processed_ict)
      
      shared_state$cpms_id      <- extracted_cpms
      shared_state$scenario_id  <- input$scenario
      shared_state$upload_meta  <- list(
        scenario_id = input$scenario,
        edge_id     = input$edge_id,
        filename    = original_name,
        raw_ict     = saved_path,
        timestamp   = timestamp
      )
      
      # ── Navigate ─────────────────────────────────────────────────────────
      current_step("step2")
      shinyjs::runjs('$("[data-value=\'tab_step2\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}