step3_UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
      selectInput(
        ns("tag_select"),
        label = NULL,
        choices = c("TRAINING_FEE"),
        width = "200px"
      ),
      actionButton(ns("apply_tag"), "Apply Tag", class = "btn-primary"),
      actionButton(ns("save"), "Save", class = "btn-success")
    ),
    reactableOutput(ns("table"))
  )
}

step3_Server <- function(id, auth_state, shared_state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # declare reactive val for data
      working_data = reactiveValues(df = NULL)
      
      # run pipeline step on load
      observe({
        req(shared_state$current_step == "step 3")
        req(shared_state$processed_ict)
        
        df <- tryCatch({
  
          generate_posting_plan(
            ict           = shared_state$processed_ict,
            rules_db_path = DB_DIR,
            scenario_id   = shared_state$scenario_id,
            ict_db_path   = DB_DIR
          )
        }, error = function(e){
          message("generate_posting_plan error: ", e$message)
          print(e)
          showNotification("Failed to generate posting plan", type = "error")
          return(NULL)
        })
        
        # assign processed data to sys shared state
        req(df)
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
          compact    = TRUE
        )
      })
      
      
      observe({
        req(working_data$df)
        message("class: ", class(working_data$df))
        if (is.list(working_data$df)) {
          message("names: ", paste(names(working_data$df), collapse = ", "))
        }
      })
    }
  )
}