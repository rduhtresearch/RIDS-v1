step1_UI <- function(id) {
  ns <- NS(id)
  div(
    selectInput(ns('scenario'), 'Select Scenario', choices = c("A", "B")),
    textInput(ns('edge_id'), 'EDGE ID'),
    textInput(ns('study_name'), 'Study Name'),
    fileInput(ns("upload"), "Choose CSV File",
              multiple = FALSE,
              accept = c(".xlsx")),
    textAreaInput( 
      ns("notes"), 
      "Add upload notes", 
    ),
    actionButton(ns('next_step'), 'Step 2'),
    
  )
}

step1_Server <- function(id, auth_state, shared_state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ICT_UPLOAD_DIR <- "/Users/tategraham/Documents/NHS/ict_dir"
      
      observeEvent(input$next_step, {
        shinyjs::runjs('$("[data-value=\'tab_step2\']").tab("show")')
        shinyjs::runjs("$('body').addClass('sidebar-collapse')")
      })
      
      observeEvent(input$'next_step', {
        feedbackDanger("edge_id", show = is.null(input$edge_id) || input$edge_id == "", text = "Required")
        feedbackDanger("scenario", show = is.null(input$scenario), text = "Required")
        if (is.null(input$upload)) {
          showNotification("Please upload a file", type = "warning")
          return()
        }
        
        req(input$edge_id != "", input$upload, input$scenario)
        
        # Build timestamped filename
        timestamp     <- format(Sys.time(), "%Y%m%d_%H%M%S")
        original_name <- input$upload$name
        saved_name    <- paste0(timestamp, "_", original_name)
        saved_path    <- file.path(ICT_UPLOAD_DIR, saved_name)
        
        # Copy file to audit directory
        file.copy(input$upload$datapath, saved_path)
        
        # Write metadata to DB
        DBI::dbExecute(CON,
                       "INSERT INTO meta_data 
      (scenario_id, edge_id, study_name, notes, uploaded_by, original_filename, saved_file_path)
      VALUES (?, ?, ?, ?, ?, ?, ?)",
                         params = list(
                         input$scenario,
                         input$edge_id,
                         input$study_name,
                         input$notes,
                         auth_state$user_id,
                         original_name,
                         saved_path
                       )
        )
        
        shared_state$scenario_id  <- input$scenario
        shared_state$upload_meta  <- list(
          scenario_id = input$scenario,
          edge_id   = input$edge_id,
          filename  = original_name,
          raw_ict   = saved_path,
          timestamp = timestamp
        )
        
        
      })
      
    }
  )
}