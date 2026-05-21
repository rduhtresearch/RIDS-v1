step3_UI <- function(id) {
  ns <- NS(id)
  bs4Card(
    title  = "Apply Tags",
    width  = 12,
    status = "primary",
    solidHeader = FALSE,
    footer = tagList(
      selectInput(ns("tag_select"), label = NULL, choices = c("TRAINING_FEE"), width = "200px"),
      actionButton(ns("apply_tag"), "Apply Tag", class = "btn-primary"),
      actionButton(ns("save"), "Save", class = "btn-success"),
      actionButton(ns("next_step"), "Next: Generate Templates", class = "pipeline-next-btn")
    ),
    reactableOutput(ns("table"))
  )
}

step3_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # loading animation
      w <- Waiter$new(
        html = tagList(
          div(
            style = "display: flex; flex-direction: column; align-items: center; gap: 1.5rem;",
            tags$style("
        @keyframes spin-ring {
          0%   { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        .green-ring {
          width: 64px;
          height: 64px;
          border-radius: 50%;
          border: 5px solid rgba(40, 167, 69, 0.2);
          border-top-color: #28a745;
          animation: spin-ring 0.9s linear infinite;
        }
      "),
            div(class = "green-ring"),
            div(
              style = "color: #ffffff; font-size: 1rem; font-weight: 600; letter-spacing: 0.03em;",
              "Running cost adjustment engine"
            ),
            div(
              style = "color: rgba(255,255,255,0.5); font-size: 0.8rem;",
              "This may take a moment..."
            )
          )
        ),
        color = "rgba(18, 34, 48, 0.92)"
      )
      
      # declare reactive val for data
      working_data = reactiveValues(df = NULL)
      
      # run pipeline step on load
      observe({
        req(shared_state$current_step == "step3")
        req(shared_state$processed_ict)
        
        df <- tryCatch({
          prepare_posting_input(
            ict           = shared_state$processed_ict,
            ict_db_path   = DB_DIR,
            scenario_id   = shared_state$scenario_id,
          )
    
        }, error = function(e){
          app_log_exception("step3", "Posting input preparation failed", e, list(
            cpms_id = shared_state$cpms_id,
            scenario_id = shared_state$scenario_id
          ))
          showNotification("Failed to generate posting plan", type = "error")
          return(NULL)
        })
        
        # assign processed data to sys shared state
        req(df)
        df$study_site <- shared_state$study_site
        working_data$df <- df
        shared_state$posting_plan <- df
      })
      
      # render table
      output$table <- renderReactable({
        req(working_data$df)
        
        reactable(
          working_data$df,
          selection  = "multiple",
          onClick    = "select",
          rownames   = FALSE,
          striped    = TRUE,
          highlight  = TRUE,
          compact    = TRUE,
          rowStyle   = JS("function(rowInfo) {
    if (rowInfo.row['calc_tag'] !== null && rowInfo.row['calc_tag'] !== '') {
      return { background: '#e8f4fd' }
    }
  }"),
          columns = list(
            .selection               = colDef(name = "Select", sortable = FALSE, filterable = FALSE, width = 50, align = "center", headerStyle = list(fontWeight = "bold"), header = JS("function() { return '' }")),
            Visit                    = colDef(show = TRUE),
            Activity                 = colDef(show = TRUE),
            Activity.Type            = colDef(name = "Type", show = TRUE),
            Department               = colDef(show = TRUE),
            calc_tag                 = colDef(name = "Tag", show = TRUE),
            Activity.Code            = colDef(show = FALSE),
            Staff.Role               = colDef(show = FALSE),
            Time.Required            = colDef(show = FALSE),
            Activity.Cost            = colDef(show = FALSE),
            Total.Activity.Cost      = colDef(show = FALSE),
            Indirect.Costs           = colDef(show = FALSE),
            Capacity.Building        = colDef(show = FALSE),
            MFF                      = colDef(show = FALSE),
            Total                    = colDef(show = FALSE),
            study_name               = colDef(show = FALSE),
            cpms_id                  = colDef(show = FALSE),
            Flag                     = colDef(show = FALSE),
            SheetName                = colDef(show = FALSE),
            staff_group              = colDef(show = FALSE),
            Study_Arm                = colDef(show = FALSE),
            activity_occurrence_id.x = colDef(show = FALSE),
            sheet_name               = colDef(show = FALSE),
            row_id                   = colDef(show = FALSE),
            provider_org             = colDef(show = FALSE),
            pi_org                   = colDef(show = FALSE),
            Visit_Label              = colDef(show = FALSE),
            activity_type_norm       = colDef(show = FALSE),
            staff_role_norm          = colDef(show = FALSE),
            row_category_auto        = colDef(show = FALSE),
            row_category             = colDef(show = FALSE),
            is_medic                 = colDef(show = FALSE),
            scenario_id              = colDef(show = FALSE),
            ruleset_id               = colDef(show = FALSE),
            activity_occurrence_id.y = colDef(show = FALSE),
            contract_cost            = colDef(show = FALSE)
          )
        )
      })
      
      # Apply custom tag
      observeEvent(input$apply_tag, {
        selected_rows <- getReactableState("table", "selected")
        req(selected_rows)
        
        working_data$df[selected_rows, "calc_tag"] <- input$tag_select

        log_event(
          level = "INFO",
          area = "step3",
          message = "Scenario applied",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(
            tag = input$tag_select,
            row_count = length(selected_rows)
          )
        )
        app_log_info("step3", "Scenario tag applied", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          tag = input$tag_select,
          rows = length(selected_rows)
        ))
        
        updateReactable("table", data = working_data$df)
      })
      
      # save updates
      observeEvent(input$save, {
        req(working_data$df)
        
        w$show()
        
        shared_state$posting_plan <- working_data$df

        log_event(
          level = "INFO",
          area = "step3",
          message = "Posting evaluation started",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(rows = nrow(working_data$df))
        )
        app_log_info("step3", "Posting evaluation started", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(working_data$df)
        ))
        
        evaluated <- tryCatch({
          evaluate_posting_plan(
            prepared_df = working_data$df,
            rules_db_path = DB_DIR,
            scenario_id = "A"
          )
        }, error = function(e) {
          app_log_exception("step3", "Posting evaluation failed", e, list(
            cpms_id = shared_state$cpms_id,
            upload_id = shared_state$upload_id,
            rows = nrow(working_data$df)
          ))
          log_event(
            level = "ERROR",
            area = "step3",
            message = "Evaluation failed",
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
          showNotification("Failed to evaluate posting plan", type = "error")
          w$hide()
          return(NULL)
        })
        
        w$hide()
        req(evaluated)
        evaluated$study_site <- shared_state$study_site
        shared_state$evaluated_plan <- evaluated

        log_event(
          level = "INFO",
          area = "step3",
          message = "Posting evaluation completed",
          user_id = auth_state$user_id,
          username = auth_state$username,
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          session_id = auth_state$session_id,
          details = list(rows = nrow(evaluated))
        )
        app_log_info("step3", "Posting evaluation completed", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(evaluated)
        ))
        
        showNotification("Tags saved", type = "message", duration = 5)
      })
      
      # next step
      observeEvent(input$next_step, {
        req(shared_state$evaluated_plan)
        shared_state$current_step <- "step4"
        current_step("step4")
        shinyjs::runjs('$("[data-value=\'tab_step4\']").tab("show")')
        shinyjs::runjs("$('body').addClass('sidebar-collapse')")
      })
      
    }
  )
}
