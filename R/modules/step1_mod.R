step1_UI <- function(id) {
  ns <- NS(id)
    bs4Card(
      title  = "Upload ICT Workbook",
      width  = 6,
      status = "primary",
      solidHeader = FALSE,
      selectInput(ns('scenario'), 'Select Scenario', choices = c("A", "B")),
      selectInput(
        ns("study_site"),
        "Study Site",
        choices = c("Select site..." = "", "RDUHT", "NDDHT"),
        selected = ""
      ),
      selectInput(ns('speciality_id'), 'Clinical speciality', choices = NULL),
      textInput(ns('edge_id'), 'EDGE ID'),
      textInput(ns('study_name'), 'Study Name'),
    fileInput(ns("upload"), "Choose Excel File",
              multiple = FALSE,
              accept = c(".xlsx")),
    textAreaInput(ns("notes"), "Add upload notes"),
    actionButton(ns('next_step'), 'Next: Review Costs', class = "pipeline-next-btn"),
    helpUI(ns("help"))
  )
}

step1_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    
    # ── Specialities lookup (loaded once at module init) ─────────────────────
    specialities <- reactive({
      dbGetQuery(CON, "
        SELECT id, name
        FROM specialities
        WHERE archived_at IS NULL
        ORDER BY name
      ")
    })
    
    observe({
      sp <- specialities()
      req(nrow(sp) > 0)
      
      choices        <- sp$id
      names(choices) <- sp$name
      
      updateSelectInput(
        session,
        "speciality_id",
        choices  = c("Select speciality..." = "", choices),
        selected = ""
      )
    })
    
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
          heading = "What is Clinical Speciality?",
          body    = "The clinical area the study sits within. This is required and used for grouping studies in reporting."
        ),
        list(
          heading = "What is Study Site?",
          body    = "Select the study site that this upload belongs to. This is used to keep studies with the same CPMS ID distinct."
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
      feedbackDanger("edge_id",       show = input$edge_id == "", text = "Required")
      feedbackDanger("upload",        show = is.null(input$upload), text = "Required")
      feedbackDanger("study_site",    show = is.null(input$study_site) || input$study_site == "",
                     text = "Required")
      feedbackDanger("speciality_id", show = is.null(input$speciality_id) || input$speciality_id == "",
                     text = "Required")
      
      req(
        input$edge_id != "",
        input$scenario,
        !is.null(input$study_site),
        input$study_site != "",
        !is.null(input$upload),
        !is.null(input$speciality_id),
        input$speciality_id != ""
      )

      log_event(
        level = "INFO",
        area = "upload",
        message = "Upload started",
        user_id = auth_state$user_id,
        username = auth_state$username,
        session_id = auth_state$session_id,
        details = list(
          scenario_id = input$scenario,
          study_site = input$study_site,
          edge_id = input$edge_id,
          original_filename = input$upload$name,
          speciality_id = as.integer(input$speciality_id)
        )
      )
      app_log_info("step1", "Upload started", list(
        scenario_id = input$scenario,
        study_site = input$study_site,
        edge_id = input$edge_id,
        filename = input$upload$name
      ))
      
      ###
      # ── Validation ────────────────────────────────────────────────────────
      validation <- tryCatch(
        validate_ict_workbook(input$upload$datapath),
        error = function(e) {
          list(valid = FALSE, findings = paste("Validation error:", conditionMessage(e)))
        }
      )
      
      if (!isTRUE(validation$valid)) {
        log_event(
          level = "WARN",
          area = "upload",
          message = "Upload validation failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            study_site = input$study_site,
            edge_id = input$edge_id,
            original_filename = input$upload$name,
            validation_findings = head(as.character(validation$findings %||% character()), 10)
          )
        )
        app_log_warn("step1", "Upload validation failed", list(
          edge_id = input$edge_id,
          filename = input$upload$name
        ))

        showModal(modalDialog(
          title = "ICT workbook validation failed",
          size  = "m",
          easyClose = FALSE,
          footer = modalButton("Close"),
          div(
            style = "padding: 0.5rem 0;",
            p(
              style = "color: #697786; margin-bottom: 1rem;",
              "The uploaded workbook contains data outside the expected structure. ",
              "Please remove the highlighted content and re-upload."
            ),
            tags$ul(
              style = "padding-left: 1.25rem; color: #1d2a36;",
              lapply(validation$findings, tags$li)
            )
          )
        ))
        return()  # block — do not proceed to file.copy / DB insert
      }
      ###
      # ── Process ──────────────────────────────────────────────────────────
      timestamp     <- format(Sys.time(), "%Y%m%d_%H%M%S")
      original_name <- input$upload$name
      saved_name    <- paste0(timestamp, "_", original_name)
      saved_path    <- file.path(ICT_UPLOAD_DIR, saved_name)

      log_event(
        level = "INFO",
        area = "upload",
        message = "Upload processing started",
        user_id = auth_state$user_id,
        username = auth_state$username,
        session_id = auth_state$session_id,
        details = list(
          scenario_id = input$scenario,
          study_site = input$study_site,
          edge_id = input$edge_id,
          original_filename = original_name
        )
      )
      app_log_info("step1", "Upload processing started", list(
        edge_id = input$edge_id,
        filename = original_name
      ))

      copy_ok <- tryCatch({
        file.copy(input$upload$datapath, saved_path, overwrite = TRUE)
      }, error = function(e) {
        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "file_copy"
          )
        )
        app_log_exception("step1", "Workbook file copy failed", e, list(
          edge_id = input$edge_id,
          filename = original_name
        ))
        FALSE
      })

      if (!isTRUE(copy_ok)) {
        showNotification("Failed to save uploaded workbook", type = "error")
        return()
      }
      
      extracted_cpms <- tryCatch({
        extract_cpms_id(saved_path)
      }, error = function(e) {
        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "cpms_extract"
          )
        )
        app_log_exception("step1", "CPMS extraction failed", e, list(
          edge_id = input$edge_id,
          filename = original_name
        ))
        showNotification("Failed to extract CPMS ID", type = "error")
        return(NULL)
      })
      
      req(extracted_cpms)

      duplicate_exists <- tryCatch({
        existing <- DBI::dbGetQuery(
          CON,
          paste(
            "SELECT 1",
            "FROM meta_data",
            "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?",
            "LIMIT 1"
          ),
          params = list(
            sanitize_text_value(as.character(extracted_cpms)),
            sanitize_text_value(input$study_site),
            sanitize_text_value(input$scenario)
          )
        )
        nrow(existing) > 0
      }, error = function(e) {
        app_log_exception("step1", "Duplicate study check failed", e, list(
          cpms_id = extracted_cpms,
          study_site = input$study_site,
          scenario_id = input$scenario
        ))
        showNotification("Failed to validate whether this study already exists", type = "error")
        return(NA)
      })

      req(!is.na(duplicate_exists))

      if (isTRUE(duplicate_exists)) {
        showNotification(
          paste(
            "This study already exists for CPMS",
            extracted_cpms,
            ", site",
            input$study_site,
            "and scenario",
            input$scenario
          ),
          type = "warning",
          duration = 8
        )
        return()
      }
      
      meta_saved <- tryCatch({
        DBI::dbExecute(CON,
                     "INSERT INTO meta_data 
   (cpms_id, study_site, scenario_id, edge_id, study_name, notes, uploaded_by, 
    original_filename, saved_file_path, speciality_id)
   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                     params = list(
                       sanitize_text_value(as.character(extracted_cpms)),
                       sanitize_text_value(input$study_site),
                       sanitize_text_value(input$scenario),
                       sanitize_text_value(input$edge_id),
                       sanitize_text_value(input$study_name),
                       sanitize_text_value(input$notes),
                       sanitize_text_value(auth_state$username %||% auth_state$name %||% ""),
                       sanitize_text_value(original_name),
                       sanitize_text_value(saved_path),
                       as.integer(input$speciality_id)
                     )
        )
        TRUE
      }, error = function(e) {
        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = extracted_cpms,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            study_site = input$study_site,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "meta_data_insert"
          )
        )
        app_log_exception("step1", "Upload metadata save failed", e, list(
          cpms_id = extracted_cpms,
          edge_id = input$edge_id
        ))
        showNotification("Failed to save upload metadata", type = "error")
        FALSE
      })

      if (!isTRUE(meta_saved)) {
        return()
      }

      upload_id <- tryCatch({
        DBI::dbGetQuery(CON, "SELECT currval('upload_id_seq') AS upload_id")$upload_id[[1]]
      }, error = function(e) {
        NA_integer_
      })
      
      shared_state$processed_ict <- tryCatch({
        process_workbook(input_path = saved_path, db_path = DB_DIR)
      }, error = function(e) {
        log_event(
          level = "ERROR",
          area = "upload",
          message = "Upload processing failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = extracted_cpms,
          upload_id = upload_id,
          session_id = auth_state$session_id,
          details = list(
            scenario_id = input$scenario,
            edge_id = input$edge_id,
            original_filename = original_name,
            error = conditionMessage(e),
            stage = "workbook_process"
          )
        )
        app_log_exception("step1", "Workbook processing failed", e, list(
          cpms_id = extracted_cpms,
          upload_id = upload_id
        ))
        showNotification("Failed to process workbook", type = "error")
        return(NULL)
      })
      
      req(shared_state$processed_ict)
      
      # ── Resolve speciality name for shared_state ─────────────────────────
      sp_id   <- as.integer(input$speciality_id)
      sp_name <- specialities()$name[specialities()$id == sp_id]
      
      shared_state$cpms_id          <- extracted_cpms
      shared_state$upload_id        <- upload_id
      shared_state$scenario_id      <- input$scenario
      shared_state$study_site       <- input$study_site
      shared_state$speciality_id    <- sp_id
      shared_state$speciality_name  <- sp_name
      shared_state$study_name       <- input$study_name
      shared_state$upload_meta      <- list(
        scenario_id     = input$scenario,
        study_site      = input$study_site,
        edge_id         = input$edge_id,
        study_name      = input$study_name,
        filename        = original_name,
        raw_ict         = saved_path,
        timestamp       = timestamp,
        upload_id       = upload_id,
        speciality_id   = sp_id,
        speciality_name = sp_name
      )

      log_event(
        level = "INFO",
        area = "upload",
        message = "Upload completed",
        user_id = auth_state$user_id,
        username = auth_state$username,
        cpms_id = extracted_cpms,
        upload_id = upload_id,
        session_id = auth_state$session_id,
        details = list(
          scenario_id = input$scenario,
          study_site = input$study_site,
          edge_id = input$edge_id,
          original_filename = original_name,
          speciality_id = sp_id
        )
      )
      app_log_info("step1", "Upload completed", list(
        cpms_id = extracted_cpms,
        upload_id = upload_id
      ))
      
      # ── Navigate ─────────────────────────────────────────────────────────
      current_step("step2")
      shinyjs::runjs('$("[data-value=\'tab_step2\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}
