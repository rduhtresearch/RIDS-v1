step2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      title      = "Review Contract Costs",
      width      = 12,
      status     = "primary",
      solidHeader = FALSE,
      footer = tagList(
        div(
          class = "step2-rounding-wrap",
          span(class = "step2-rounding-title", "Contract cost mode"),
          div(
            class = "step2-rounding-toggle",
            span(
              id = ns("round_left_label"),
              class = "step2-rounding-label is-active",
              "Rounded"
            ),
            tags$label(
              class = "step2-switch",
              tags$input(
                id = ns("round_to_pound_switch"),
                type = "checkbox",
                checked = "checked",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', !this.checked, {priority: 'event'})",
                  ns("use_unrounded_cost")
                )
              ),
              tags$span(
                class = "step2-switch-track",
                tags$span(class = "step2-switch-knob")
              )
            ),
            span(
              id = ns("round_right_label"),
              class = "step2-rounding-label",
              "Unrounded"
            )
          ),
          tags$script(HTML(sprintf("
            (function() {
              var cb = document.getElementById('%s');
              var leftLbl = document.getElementById('%s');
              var rightLbl = document.getElementById('%s');
              function refresh() {
                var knob = cb.parentNode.querySelector('.step2-switch-knob');
                if (cb.checked) {
                  knob.style.transform = 'translateX(0)';
                  leftLbl.classList.add('is-active');
                  rightLbl.classList.remove('is-active');
                } else {
                  knob.style.transform = 'translateX(24px)';
                  leftLbl.classList.remove('is-active');
                  rightLbl.classList.add('is-active');
                }
              }
              cb.addEventListener('change', refresh);
              refresh();
            })();
          ",
            ns("round_to_pound_switch"),
            ns("round_left_label"),
            ns("round_right_label")
          )))
        ),
        actionButton(ns("save"), "Save to database", class = "btn-success"),
        actionButton(ns("next_step"), "Next: Apply Tags", class = "pipeline-next-btn")
      ),
      reactableOutput(ns("table"))
    )
  )
}

step2_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
          
    working_data <- reactiveValues(df = NULL)
    is_saved <- reactiveVal(FALSE)
    
    observe({
      shinyjs::toggleState("next_step", condition = isTRUE(is_saved()))
    })
    
    # ── Load data ─────────────────────────────────────────────────────────────
    observe({
      req(shared_state$cpms_id)
      
      df <- DBI::dbGetQuery(
        CON,
        "SELECT CPMS_ID, study_site, scenario_id, Study, Visit_Number, Study_Arm,
         Visit_Label, Activity_Name, ICT_Cost, Contract_Cost,
         activity_occurrence_id, staff_group
         FROM ict_costing_tbl
         WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?",
         params = list(
           as.character(shared_state$cpms_id),
           as.character(shared_state$study_site),
           as.character(shared_state$scenario_id)
         )
      )
      
      working_data$df <- df
      is_saved(FALSE)
    })
    
    apply_contract_cost_mode <- function(use_unrounded_cost) {
      req(working_data$df)
      working_data$df$Contract_Cost <- if (isTRUE(use_unrounded_cost)) {
        working_data$df$ICT_Cost
      } else {
        round(working_data$df$ICT_Cost)
      }
      updateReactable("table", data = working_data$df)
    }

    # ── Toggle rounding mode ──────────────────────────────────────────────────
    observeEvent(input$use_unrounded_cost, {
      apply_contract_cost_mode(input$use_unrounded_cost)
      is_saved(FALSE)
    })
    
    # ── Row select → modal ────────────────────────────────────────────────────
    observeEvent(getReactableState("table", "selected"), {
      req(getReactableState("table", "selected"))
      
      selected_row <- getReactableState("table", "selected")
      row <- working_data$df[selected_row, ]
      
      showModal(modalDialog(
        title = "Set Custom Value",
        p(strong("Visit: "), row$Visit_Label),
        p(strong("Activity: "), row$Activity_Name),
        p(strong("ICT Cost: "), paste0("£", round(row$ICT_Cost, 2))),
        hr(),
        numericInput(
          session$ns("contract_value"),
          label = "Contract Cost (£)",
          value = NULL,
          min   = 0
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_edit"), "Confirm", class = "btn-primary")
        )
      ))
    })
    
    # ── Confirm edit ──────────────────────────────────────────────────────────
    observeEvent(input$confirm_edit, {
      req(input$contract_value)
      
      selected_row <- getReactableState("table", "selected")
      working_data$df[selected_row, "Contract_Cost"] <- input$contract_value
      
      updateReactable("table", data = working_data$df)
      is_saved(FALSE)
      removeModal()
    })
    
    # ── Save to DB ────────────────────────────────────────────────────────────
    observeEvent(input$save, {
      req(working_data$df)

      log_event(
        level = "INFO",
        area = "step2",
        message = "Step 2 save started",
        user_id = auth_state$user_id,
        username = auth_state$username,
        cpms_id = shared_state$cpms_id,
        upload_id = shared_state$upload_id,
        session_id = auth_state$session_id,
        details = list(rows = nrow(working_data$df))
      )
      app_log_info("step2", "Save started")
      
      tryCatch({
        dbExecute(CON,
                  paste(
                    "DELETE FROM ict_costing_tbl",
                    "WHERE CPMS_ID = ? AND study_site = ? AND scenario_id = ?"
                  ),
                  params = list(
                    as.character(shared_state$cpms_id),
                    as.character(shared_state$study_site),
                    as.character(shared_state$scenario_id)
                  )
        )
        dbAppendTable(CON, "ict_costing_tbl", working_data$df)

        log_event(
          level = "INFO",
          area = "step2",
          message = "Step 2 save completed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(rows = nrow(working_data$df))
        )
        app_log_info("step2", "Save completed")

        is_saved(TRUE)
        showNotification("Saved successfully", type = "message", duration = 5)
      }, error = function(e) {
        if (handle_fatal_db_error(session, e, "step2", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(working_data$df),
          stage = "save"
        ))) {
          is_saved(FALSE)
          return(NULL)
        }

        app_log_exception("step2", "Step 2 save failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(working_data$df)
        ))
        log_event(
          level = "ERROR",
          area = "step2",
          message = "Step 2 save failed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(
            rows = nrow(working_data$df),
            error = conditionMessage(e)
          )
        )
        is_saved(FALSE)
        showNotification("Save failed", type = "error", duration = 5)
      })
    })
    
    # ── Render table ──────────────────────────────────────────────────────────
    output$table <- renderReactable({
      req(working_data$df)
      reactable(
        working_data$df,
        selection = "single",
        onClick   = "select",
        rownames  = FALSE,
        columns = list(
          Contract_Cost = colDef(
            name = "Contract Cost",
            headerStyle = list(
              background = "#eef5fa",
              borderLeft = "1px solid #d6e4ef",
              borderRight = "1px solid #d6e4ef"
            ),
            style = list(
              background = "#f8fbfd",
              borderLeft = "1px solid #e2edf5",
              borderRight = "1px solid #e2edf5"
            )
          )
        )
      )
    })
    
    # ── Next step ─────────────────────────────────────────────────────────────
    observeEvent(input$next_step, {
      current_step("step3")
      shared_state$current_step <- "step3"
      shinyjs::runjs('$("[data-value=\'tab_step3\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}
