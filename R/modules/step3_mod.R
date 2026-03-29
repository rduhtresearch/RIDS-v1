step2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h1('step2'),
    selectInput(ns("df_select"), "Select sheet", choices = NULL),
    reactableOutput(ns("table"))
  )
}

step2_Server <- function(id, auth_state, shared_state) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ict_data <- reactiveVal(NULL)
      
      # Get data / run pipeline
      observeEvent(shared_state$upload_meta, {
        req(shared_state$upload_meta)
        ict_data(process_workbook(
          input_path = shared_state$upload_meta$raw_ict,
          db_path = DB_DIR))
        
      })
      
      # Populate select input
      observeEvent(ict_data(), {
        req(ict_data())
        updateSelectInput(session, "df_select", choices = names(ict_data()))
      })
      
      
      output$table <- renderReactable({
        req(ict_data(), input$df_select)
        df <- ict_data()[[input$df_select]]
        req(is.data.frame(df))
        reactable(df,
                  pagination = FALSE,
                  height = 500,
                  striped = TRUE,
                  wrap = TRUE,
                  defaultColDef = colDef(minWidth = 120, maxWidth = 200)
        )
      })
      
      
    }
  )
}