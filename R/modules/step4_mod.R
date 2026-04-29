step4_UI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      title       = "Generate EDGE Templates",
      width       = 12,
      status      = "primary",
      solidHeader = FALSE,
      footer = tagList(
        downloadButton(ns("download_zip"), "Download ZIP", class = "btn-success")
      ),
      div(
        style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
        selectInput(ns("arm_select"), label = "Study Arm", choices = NULL, width = "200px"),
        uiOutput(ns("save_status"))
      ),
      reactableOutput(ns("preview_table")),
      
      hr(),
      h4("Template builder (preview)"),
      edgeBuilderUI(ns("edge_builder"))
    )
  )
}

# step4_Server <- function(id, auth_state, shared_state, current_step) {
#   moduleServer(id, function(input, output, session) {
#     
#     templates <- reactiveVal(NULL)
#     zip_path  <- reactiveVal(NULL)
#     
#     edited_templates <- edgeBuilderServer(
#       id             = "edge_builder",
#       edge_templates = reactive(shared_state$edge_templates)
#     )
#     
#     w <- Waiter$new(
#       html = tagList(
#         div(
#           style = "display: flex; flex-direction: column; align-items: center; gap: 1.5rem;",
#           div(class = "green-ring"),
#           div(
#             style = "color: #ffffff; font-size: 1rem; font-weight: 600;",
#             "Generating EDGE templates"
#           ),
#           div(
#             style = "color: rgba(255,255,255,0.5); font-size: 0.8rem;",
#             "This may take a moment..."
#           )
#         )
#       ),
#       color = "rgba(31, 95, 139, 0.55)"
#     )
#     
#     # ── Generate templates on load ────────────────────────────────────────────
#     observe({
#       req(shared_state$current_step == "step4")
#       req(shared_state$evaluated_plan)
#       
#       w$show()
#       
#       # Step 1 — adjust posting lines
#       adjusted <- tryCatch({
#         adjust_posting_lines(shared_state$evaluated_plan)
#       }, error = function(e) {
#         message("adjust_posting_lines error: ", e$message)
#         showNotification("Failed to adjust posting lines", type = "error")
#         w$hide()
#         return(NULL)
#       })
#       
#       req(adjusted)
#       
#       # Note: Fix this - needs to be renamed earlier in the pipeline. This is not clean.
#       adjusted <- adjusted %>% 
#         rename(Staff_Role = Staff.Role)
#       
#       # Step 2 — save posting lines to DB
#       tryCatch({
#         dbExecute(CON,
#                   "DELETE FROM posting_lines WHERE cpms_id = ?",
#                   params = list(as.character(shared_state$cpms_id))
#         )
#         dbAppendTable(CON, "posting_lines", adjusted)
#         message("Posting lines saved to DB: ", nrow(adjusted), " rows")
#       }, error = function(e) {
#         message("Posting lines DB error: ", e$message)
#         showNotification("Failed to save posting lines", type = "error")
#       })
#       
#       # Step 3 — build templates
#       tmpl <- tryCatch({
#         build_all_edge_templates(adjusted)
#       }, error = function(e) {
#         message("build_all_edge_templates error: ", e$message)
#         showNotification("Failed to build templates", type = "error")
#         w$hide()
#         return(NULL)
#       })
#       
#       req(tmpl)
#       templates(tmpl)
#       shared_state$edge_templates <- tmpl
#       
#       # Step 4 — write CSVs and zip
#       tryCatch({
#         tmp_dir <- tempdir()
#         
#         for (arm in names(tmpl)) {
#           csv_path <- file.path(tmp_dir, paste0(arm, ".csv"))
#           write.csv(tmpl[[arm]], csv_path, row.names = FALSE)
#         }
#         
#         timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
#         zip_name  <- paste0(shared_state$cpms_id, "_", timestamp, ".zip")
#         zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)
#         
#         if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
#         
#         zip(zp, files = file.path(tmp_dir, paste0(names(tmpl), ".csv")), flags = "-j")
#         zip_path(zp)
#         
#       }, error = function(e) {
#         message("Zip error: ", e$message)
#         showNotification("Failed to save ZIP", type = "error")
#       })
#       
#       updateSelectInput(session, "arm_select", choices = names(tmpl))
#       
#       w$hide()
#       showNotification("Templates generated successfully", type = "message", duration = 5)
#     })
#     
#     # ── Preview selected arm ──────────────────────────────────────────────────
#     output$preview_table <- renderReactable({
#       req(templates())
#       req(input$arm_select)
#       req(input$arm_select %in% names(templates()))
#       
#       df <- templates()[[input$arm_select]]
#       
#       reactable(
#         df,
#         columns = list(
#           Department = colDef(show = FALSE)
#         ),
#         striped       = TRUE,
#         highlight     = TRUE,
#         compact       = TRUE,
#         rownames      = FALSE,
#         pagination    = FALSE,
#         height        = 500,
#         resizable     = TRUE,
#         wrap          = FALSE,
#         defaultColDef = colDef(minWidth = 120)
#       )
#     })
#     
#     # ── Save status ───────────────────────────────────────────────────────────
#     output$save_status <- renderUI({
#       req(zip_path())
#       div(
#         style = "display: flex; align-items: center; gap: 0.5rem;",
#         span(style = "color: #28a745; font-size: 1.2rem;", "✓"),
#         span(
#           style = "font-size: 0.85rem; color: #697786;",
#           paste0("Saved to: ", zip_path())
#         )
#       )
#     })
#     
#     # ── Download ZIP ──────────────────────────────────────────────────────────
#     output$download_zip <- downloadHandler(
#       filename = function() {
#         paste0(shared_state$cpms_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
#       },
#       content = function(file) {
#         req(zip_path())
#         file.copy(zip_path(), file)
#       }
#     )
#     
#   })
# }

step4_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    
    templates <- reactiveVal(NULL)
    zip_path  <- reactiveVal(NULL)
    
    # ── Edge template builder module ─────────────────────────────────────────
    edited_templates <- edgeBuilderServer(
      id             = "edge_builder",
      edge_templates = reactive(shared_state$edge_templates)
    )
    
    w <- Waiter$new(
      html = tagList(
        div(
          style = "display: flex; flex-direction: column; align-items: center; gap: 1.5rem;",
          div(class = "green-ring"),
          div(
            style = "color: #ffffff; font-size: 1rem; font-weight: 600;",
            "Generating EDGE templates"
          ),
          div(
            style = "color: rgba(255,255,255,0.5); font-size: 0.8rem;",
            "This may take a moment..."
          )
        )
      ),
      color = "rgba(31, 95, 139, 0.55)"
    )
    
    # ── Helpers ──────────────────────────────────────────────────────────────
    prepare_for_export <- function(tpls) {
      tpls <- Filter(function(d) !is.null(d) && nrow(d) > 0, tpls)
      lapply(tpls, function(d) dplyr::select(d, -dplyr::any_of("Department")))
    }
    
    write_zip <- function(tpls, zp) {
      tpls <- prepare_for_export(tpls)
      
      if (length(tpls) == 0) {
        stop("No templates with rows to export.")
      }
      
      tmp_dir <- tempfile("edge_export_")
      dir.create(tmp_dir, recursive = TRUE)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      
      csv_files <- character(length(tpls))
      for (i in seq_along(tpls)) {
        nm  <- names(tpls)[i]
        csv <- file.path(tmp_dir, paste0(nm, ".csv"))
        write.csv(tpls[[i]], csv, row.names = FALSE)
        csv_files[i] <- csv
      }
      
      if (file.exists(zp)) file.remove(zp)
      zip(zp, files = csv_files, flags = "-j")
      
      invisible(zp)
    }
    
    # ── Generate templates on load ────────────────────────────────────────────
    observe({
      req(shared_state$current_step == "step4")
      req(shared_state$evaluated_plan)
      
      w$show()
      
      adjusted <- tryCatch({
        adjust_posting_lines(shared_state$evaluated_plan)
      }, error = function(e) {
        message("adjust_posting_lines error: ", e$message)
        showNotification("Failed to adjust posting lines", type = "error")
        w$hide()
        return(NULL)
      })
      
      req(adjusted)
      
      adjusted <- adjusted %>% rename(Staff_Role = Staff.Role)
      
      tryCatch({
        dbExecute(CON,
                  "DELETE FROM posting_lines WHERE cpms_id = ?",
                  params = list(as.character(shared_state$cpms_id))
        )
        dbAppendTable(CON, "posting_lines", adjusted)
        message("Posting lines saved to DB: ", nrow(adjusted), " rows")
      }, error = function(e) {
        message("Posting lines DB error: ", e$message)
        showNotification("Failed to save posting lines", type = "error")
      })
      
      tmpl <- tryCatch({
        build_all_edge_templates(adjusted)
      }, error = function(e) {
        message("build_all_edge_templates error: ", e$message)
        showNotification("Failed to build templates", type = "error")
        w$hide()
        return(NULL)
      })
      
      req(tmpl)
      templates(tmpl)
      shared_state$edge_templates <- tmpl
      
      # Initial ZIP write — uses original templates (user hasn't touched yet).
      # The download handler regenerates from edited_templates() on click.
      tryCatch({
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        zip_name  <- paste0(shared_state$cpms_id, "_", timestamp, ".zip")
        zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)
        
        if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
        
        write_zip(tmpl, zp)
        zip_path(zp)
        
      }, error = function(e) {
        message("Zip error: ", e$message)
        showNotification("Failed to save ZIP", type = "error")
      })
      
      updateSelectInput(session, "arm_select", choices = names(tmpl))
      
      w$hide()
      showNotification("Templates generated successfully", type = "message", duration = 5)
    })
    
    # ── Preview selected arm ──────────────────────────────────────────────────
    output$preview_table <- renderReactable({
      req(templates())
      req(input$arm_select)
      req(input$arm_select %in% names(templates()))
      
      df <- templates()[[input$arm_select]]
      
      reactable(
        df,
        columns = list(
          Department = colDef(show = FALSE)
        ),
        striped       = TRUE,
        highlight     = TRUE,
        compact       = TRUE,
        rownames      = FALSE,
        pagination    = FALSE,
        height        = 500,
        resizable     = TRUE,
        wrap          = FALSE,
        defaultColDef = colDef(minWidth = 120)
      )
    })
    
    # ── Save status ───────────────────────────────────────────────────────────
    output$save_status <- renderUI({
      req(zip_path())
      div(
        style = "display: flex; align-items: center; gap: 0.5rem;",
        span(style = "color: #28a745; font-size: 1.2rem;", "✓"),
        span(
          style = "font-size: 0.85rem; color: #697786;",
          paste0("Saved to: ", zip_path())
        )
      )
    })
    
    # ── Download ZIP (rebuilds from edited templates on click) ───────────────
    output$download_zip <- downloadHandler(
      filename = function() {
        paste0(shared_state$cpms_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        tpls <- edited_templates()
        if (is.null(tpls) || length(tpls) == 0) {
          tpls <- templates()
        }
        
        req(tpls)
        write_zip(tpls, file)
      }
    )
    
  })
}