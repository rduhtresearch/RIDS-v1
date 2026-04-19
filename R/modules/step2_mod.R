step2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      title      = "Review Contract Costs",
      width      = 12,
      status     = "primary",
      solidHeader = FALSE,
      footer = tagList(
        actionButton(ns("round_all"), "Round all to nearest £"),
        actionButton(ns("save"), "Save to database", class = "btn-success"),
        actionButton(ns("next_step"), "Next: Apply Tags", class = "btn-primary")
      ),
      reactableOutput(ns("table"))
    )
  )
}

step2_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    
    working_data <- reactiveValues(df = NULL)
    
    # ── Load data ─────────────────────────────────────────────────────────────
    observe({
      req(shared_state$cpms_id)
      
      df <- DBI::dbGetQuery(
        CON,
        "SELECT Study, Visit_Number, Study_Arm,
         Visit_Label, Activity_Name, ICT_Cost, Contract_Cost
         FROM ict_costing_tbl WHERE CPMS_ID = ?",
        params = list(as.character(shared_state$cpms_id))
      )
      
      working_data$df <- df
    })
    
    # ── Round all ─────────────────────────────────────────────────────────────
    observeEvent(input$round_all, {
      req(working_data$df)
      working_data$df$Contract_Cost <- round(working_data$df$ICT_Cost)
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
      removeModal()
    })
    
    # ── Save to DB ────────────────────────────────────────────────────────────
    observeEvent(input$save, {
      req(working_data$df)
      
      tryCatch({
        dbExecute(CON,
                  "DELETE FROM ict_costing_tbl WHERE CPMS_ID = ?",
                  params = list(as.character(shared_state$cpms_id))
        )
        dbAppendTable(CON, "ict_costing_tbl", working_data$df)
        showNotification("Saved successfully", type = "message", duration = 5)
      }, error = function(e) {
        message("Save error: ", e$message)
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
        rownames  = FALSE
      )
    })
    
    # ── Next step ─────────────────────────────────────────────────────────────
    observeEvent(input$next_step, {
      current_step("step3")
      shinyjs::runjs('$("[data-value=\'tab_step3\']").tab("show")')
      shinyjs::runjs("$('body').addClass('sidebar-collapse')")
    })
    
  })
}