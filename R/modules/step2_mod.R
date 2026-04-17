step2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("round_all"), "Round all to nearest £"),
    actionButton(ns("save"), "Save to database"),
    reactableOutput(ns("table")),
    actionButton(ns('next_step'), 'Step 3'),
  )
}

step2_Server <- function(id, auth_state, shared_state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      working_data <- reactiveValues(df = NULL)
      
      # load / query data
      observe({
        req(shared_state$cpms_id)
      
        df <- DBI::dbGetQuery(
          CON,
          "SELECT 
           Study, Visit_Number, Study_Arm,
           Visit_Label, Activity_Name, ICT_Cost, Contract_Cost
           FROM ict_costing_tbl WHERE CPMS_ID = ?",
          params = list(as.character(shared_state$cpms_id))
        )
        
        working_data$df <- df
      })
      
      # track user changes
      observeEvent(input$table_cell_edit, {
        info <- input$table_cell_edit
        
        working_data$df[info$row, info$col + 1] <- as.numeric(info$value)
        
      })
      
      # round all costs to 'contract value'
      observeEvent(input$round_all, {
        req(working_data$df)
        
        working_data$df$Contract_Cost <- round(working_data$df$ICT_Cost)
        
      })
      
      # modal
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
      
      # write edit
      observeEvent(input$confirm_edit, {
        req(input$contract_value)
        
        selected_row <- getReactableState("table", "selected")
        
        working_data$df[selected_row, "Contract_Cost"] <- input$contract_value
        
        updateReactable("table", data = working_data$df)
        removeModal()
      })
      
      # save costs to database
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
      
      observe({
        req(shared_state$cpms_id)
        
        count <- DBI::dbGetQuery(
          CON,
          "SELECT COUNT(*) AS n FROM ict_costing_tbl WHERE CPMS_ID = ?",
          params = list(as.character(shared_state$cpms_id))
        )
        
        message("Rows in DB for cpms_id ", shared_state$cpms_id, ": ", count$n)
      })
      
      # render data
      output$table <- renderReactable({
        req(working_data$df)

        reactable(
          working_data$df,
          selection  = "single",
          onClick    = "select",
          rownames   = FALSE
        )
      })
      
      # Switch step view
      observeEvent(input$next_step, {
        shared_state$current_step <- "step 3"
        shinyjs::runjs('$("[data-value=\'tab_step3\']").tab("show")')
        shinyjs::runjs("$('body').addClass('sidebar-collapse')")
      })
    
      
    }
  )
}