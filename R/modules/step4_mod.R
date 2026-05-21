# step4_UI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     bs4Card(
#       title       = "Generate EDGE Templates",
#       width       = 12,
#       status      = "primary",
#       solidHeader = FALSE,
#       footer = tagList(
#         downloadButton(ns("download_zip"), "Download ZIP", class = "btn-success"),
#         actionButton(ns("complete"), "Complete and return to library", class = "btn-primary")
#       ),
#       div(
#         style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
#         selectInput(ns("arm_select"), label = "Study Arm", choices = NULL, width = "200px"),
#         uiOutput(ns("save_status"))
#       ),
#       reactableOutput(ns("preview_table")),
#       
#       hr(),
#       h4("Template builder (preview)"),
#       edgeBuilderUI(ns("edge_builder"))
#     )
#   )
# }
# 
# # step4_Server <- function(id, auth_state, shared_state, current_step) {
# #   moduleServer(id, function(input, output, session) {
# #     
# #     templates <- reactiveVal(NULL)
# #     zip_path  <- reactiveVal(NULL)
# #     
# #     edited_templates <- edgeBuilderServer(
# #       id             = "edge_builder",
# #       edge_templates = reactive(shared_state$edge_templates)
# #     )
# #     
# #     w <- Waiter$new(
# #       html = tagList(
# #         div(
# #           style = "display: flex; flex-direction: column; align-items: center; gap: 1.5rem;",
# #           div(class = "green-ring"),
# #           div(
# #             style = "color: #ffffff; font-size: 1rem; font-weight: 600;",
# #             "Generating EDGE templates"
# #           ),
# #           div(
# #             style = "color: rgba(255,255,255,0.5); font-size: 0.8rem;",
# #             "This may take a moment..."
# #           )
# #         )
# #       ),
# #       color = "rgba(31, 95, 139, 0.55)"
# #     )
# #     
# #     # ── Generate templates on load ────────────────────────────────────────────
# #     observe({
# #       req(shared_state$current_step == "step4")
# #       req(shared_state$evaluated_plan)
# #       
# #       w$show()
# #       
# #       # Step 1 — adjust posting lines
# #       adjusted <- tryCatch({
# #         adjust_posting_lines(shared_state$evaluated_plan)
# #       }, error = function(e) {
# #         message("adjust_posting_lines error: ", e$message)
# #         showNotification("Failed to adjust posting lines", type = "error")
# #         w$hide()
# #         return(NULL)
# #       })
# #       
# #       req(adjusted)
# #       
# #       # Note: Fix this - needs to be renamed earlier in the pipeline. This is not clean.
# #       adjusted <- adjusted %>% 
# #         rename(Staff_Role = Staff.Role)
# #       
# #       # Step 2 — save posting lines to DB
# #       tryCatch({
# #         dbExecute(CON,
# #                   "DELETE FROM posting_lines WHERE cpms_id = ?",
# #                   params = list(as.character(shared_state$cpms_id))
# #         )
# #         dbAppendTable(CON, "posting_lines", adjusted)
# #         message("Posting lines saved to DB: ", nrow(adjusted), " rows")
# #       }, error = function(e) {
# #         message("Posting lines DB error: ", e$message)
# #         showNotification("Failed to save posting lines", type = "error")
# #       })
# #       
# #       # Step 3 — build templates
# #       tmpl <- tryCatch({
# #         build_all_edge_templates(adjusted)
# #       }, error = function(e) {
# #         message("build_all_edge_templates error: ", e$message)
# #         showNotification("Failed to build templates", type = "error")
# #         w$hide()
# #         return(NULL)
# #       })
# #       
# #       req(tmpl)
# #       templates(tmpl)
# #       shared_state$edge_templates <- tmpl
# #       
# #       # Step 4 — write CSVs and zip
# #       tryCatch({
# #         tmp_dir <- tempdir()
# #         
# #         for (arm in names(tmpl)) {
# #           csv_path <- file.path(tmp_dir, paste0(arm, ".csv"))
# #           write.csv(tmpl[[arm]], csv_path, row.names = FALSE)
# #         }
# #         
# #         timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
# #         zip_name  <- paste0(shared_state$cpms_id, "_", timestamp, ".zip")
# #         zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)
# #         
# #         if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
# #         
# #         zip(zp, files = file.path(tmp_dir, paste0(names(tmpl), ".csv")), flags = "-j")
# #         zip_path(zp)
# #         
# #       }, error = function(e) {
# #         message("Zip error: ", e$message)
# #         showNotification("Failed to save ZIP", type = "error")
# #       })
# #       
# #       updateSelectInput(session, "arm_select", choices = names(tmpl))
# #       
# #       w$hide()
# #       showNotification("Templates generated successfully", type = "message", duration = 5)
# #     })
# #     
# #     # ── Preview selected arm ──────────────────────────────────────────────────
# #     output$preview_table <- renderReactable({
# #       req(templates())
# #       req(input$arm_select)
# #       req(input$arm_select %in% names(templates()))
# #       
# #       df <- templates()[[input$arm_select]]
# #       
# #       reactable(
# #         df,
# #         columns = list(
# #           Department = colDef(show = FALSE)
# #         ),
# #         striped       = TRUE,
# #         highlight     = TRUE,
# #         compact       = TRUE,
# #         rownames      = FALSE,
# #         pagination    = FALSE,
# #         height        = 500,
# #         resizable     = TRUE,
# #         wrap          = FALSE,
# #         defaultColDef = colDef(minWidth = 120)
# #       )
# #     })
# #     
# #     # ── Save status ───────────────────────────────────────────────────────────
# #     output$save_status <- renderUI({
# #       req(zip_path())
# #       div(
# #         style = "display: flex; align-items: center; gap: 0.5rem;",
# #         span(style = "color: #28a745; font-size: 1.2rem;", "✓"),
# #         span(
# #           style = "font-size: 0.85rem; color: #697786;",
# #           paste0("Saved to: ", zip_path())
# #         )
# #       )
# #     })
# #     
# #     # ── Download ZIP ──────────────────────────────────────────────────────────
# #     output$download_zip <- downloadHandler(
# #       filename = function() {
# #         paste0(shared_state$cpms_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
# #       },
# #       content = function(file) {
# #         req(zip_path())
# #         file.copy(zip_path(), file)
# #       }
# #     )
# #     
# #   })
# # }
# 
# step4_Server <- function(id, auth_state, shared_state, current_step) {
#   moduleServer(id, function(input, output, session) {
#     
#     templates <- reactiveVal(NULL)
#     zip_path  <- reactiveVal(NULL)
#     
#     # ── Reset helper for shared_state ──────────────────────────────────────────
#     reset_shared_state <- function() {
#       shared_state$scenario_id      <- NULL
#       shared_state$edge_id          <- NULL
#       shared_state$cpms_id          <- NULL
#       shared_state$filename         <- NULL
#       shared_state$upload_meta      <- NULL
#       shared_state$raw_ict          <- NULL
#       shared_state$posting_plan     <- NULL
#       shared_state$processed_ict    <- NULL
#       shared_state$evaluated_plan   <- NULL
#       shared_state$edge_templates   <- NULL
#       shared_state$speciality_id    <- NULL
#       shared_state$speciality_name  <- NULL
#       shared_state$current_step     <- NULL
#       shared_state$timestamp        <- NULL
#     }
#     
#     templates <- reactiveVal(NULL)
#     zip_path  <- reactiveVal(NULL)
#     
#     # ── Edge template builder module ─────────────────────────────────────────
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
#     # ── Helpers ──────────────────────────────────────────────────────────────
#     
#     prepare_for_export <- function(tpls) {
#       Filter(function(d) !is.null(d) && nrow(d) > 0, tpls)
#     }
#     
#     # Department is internal-only — drives builder read-only logic.
#     # EDGE expects it blank on import, and the top preview represents the export.
#     blank_department <- function(tpls) {
#       lapply(tpls, function(d) {
#         if ("Department" %in% names(d)) d$Department <- NA
#         d
#       })
#     }
#     
#     # prepare_for_export <- function(tpls) {
#     #   Filter(function(d) !is.null(d) && nrow(d) > 0, tpls)
#     # }
#     
#     write_zip <- function(tpls, zp) {
#       
#       tpls <- prepare_for_export(tpls)
#       if (length(tpls) == 0) {
#         stop("No templates with rows to export.")
#       }
#       tpls <- blank_department(tpls)
#       
#       
#       if (length(tpls) == 0) {
#         stop("No templates with rows to export.")
#       }
#       # Write CSVs locally first
#       tmp_dir <- tempfile("edge_export_")
#       dir.create(tmp_dir, recursive = TRUE)
#       on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
#       csv_files <- character(length(tpls))
#       for (i in seq_along(tpls)) {
#         nm <- names(tpls)[i]
#         # Make filename safe
#         safe_nm <- gsub("[^A-Za-z0-9_-]", "_", nm)
#         csv <- file.path(tmp_dir, paste0(safe_nm, ".csv"))
#         write.csv(
#           tpls[[i]],
#           file = csv,
#           row.names = FALSE,
#           na = ""
#         )
#         csv_files[i] <- csv
#       }
#       if (!all(file.exists(csv_files))) {
#         stop("One or more CSV files were not created before zipping.")
#       }
#       # Create ZIP locally first
#       local_zip <- tempfile("edge_zip_", fileext = ".zip")
#       zip::zipr(
#         zipfile = local_zip,
#         files   = csv_files,
#         root    = tmp_dir
#       )
#       if (!file.exists(local_zip) || file.info(local_zip)$size == 0) {
#         stop("ZIP archive was not created locally.")
#       }
#       # Copy final ZIP to network/shared output path
#       out_dir <- dirname(zp)
#       if (!dir.exists(out_dir)) {
#         dir.create(out_dir, recursive = TRUE)
#       }
#       ok <- file.copy(local_zip, zp, overwrite = TRUE)
#       if (!ok || !file.exists(zp) || file.info(zp)$size == 0) {
#         stop("ZIP was created locally but could not be copied to: ", zp)
#       }
#       invisible(zp)
#     }
#     
#     # ── Generate templates on load ────────────────────────────────────────────
#     observe({
#       req(shared_state$current_step == "step4")
#       req(shared_state$evaluated_plan)
#       
#       w$show()
#       
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
#       adjusted <- adjusted %>% rename(Staff_Role = Staff.Role)
#       
#       # ── Attach cost centres ──────────────────────────────────────────────────
#       adjusted <- tryCatch({
#         add_cost_centres(adjusted, isolate(shared_state$speciality_name))
#       }, error = function(e) {
#         message("add_cost_centres error: ", conditionMessage(e))
#         showNotification(
#           paste("Failed to assign cost centres:", conditionMessage(e)),
#           type = "error",
#           duration = 10
#         )
#         return(adjusted)
#       })
#       
#       adjusted <- tryCatch({
#         assign_edge_keys(adjusted)
#       }, error = function(e) {
#         message("assign_edge_keys error: ", conditionMessage(e))
#         showNotification(
#           paste("Failed to assign EDGE keys:", conditionMessage(e)),
#           type = "error",
#           duration = 10
#         )
#         return(adjusted)
#       })
#       
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
#       tmpl <- tryCatch({
#       
#         visit_lookup <- dbGetQuery(CON, "
#                         SELECT DISTINCT Study, Study_Arm, Visit_Label, Visit_Number
#                         FROM ict_costing_tbl
#                         WHERE Visit_Label IS NOT NULL
#                       ")
#         
#         templates <- build_all_edge_templates(adjusted, visit_lookup, shared_state$upload_meta$edge_id)
#         
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
#       # Initial ZIP write — uses original templates (user hasn't touched yet).
#       # The download handler regenerates from edited_templates() on click.
#       tryCatch({
#         timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
#         zip_name  <- paste0(shared_state$cpms_id, "_", timestamp, ".zip")
#         zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)
#         
#         if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
#         
#         write_zip(tmpl, zp)
#         zip_path(zp)
#         
#         # Persist the ZIP path to meta_data for this study
#         dbExecute(CON,
#                   "UPDATE meta_data SET edge_zip_path = ? WHERE cpms_id = ?",
#                   params = list(zp, as.character(shared_state$cpms_id))
#         )
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
#     # output$preview_table <- renderReactable({
#     #   req(templates())
#     #   req(input$arm_select)
#     #   req(input$arm_select %in% names(templates()))
#     #   
#     #   df <- templates()[[input$arm_select]]
#     #   
#     #   reactable(
#     #     df,
#     #     columns = list(
#     #       Department = colDef(show = FALSE)
#     #     ),
#     #     striped       = TRUE,
#     #     highlight     = TRUE,
#     #     compact       = TRUE,
#     #     rownames      = FALSE,
#     #     pagination    = FALSE,
#     #     height        = 500,
#     #     resizable     = TRUE,
#     #     wrap          = FALSE,
#     #     defaultColDef = colDef(minWidth = 120)
#     #   )
#     # })
#     output$preview_table <- renderReactable({
#       req(input$arm_select)
#       
#       tpls <- edited_templates()
#       if (is.null(tpls) || length(tpls) == 0) tpls <- templates()
#       
#       req(tpls, input$arm_select %in% names(tpls))
#       
#       df <- blank_department(tpls)[[input$arm_select]]
#       
#       reactable(
#         df,
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
#     # ── Download ZIP (rebuilds from edited templates on click) ───────────────
#     output$download_zip <- downloadHandler(
#       filename = function() {
#         paste0(shared_state$cpms_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
#       },
#       contentType = "application/zip",
#       content = function(file) {
#         tpls <- edited_templates()
#         if (is.null(tpls) || length(tpls) == 0) {
#           tpls <- templates()
#         }
#         
#         req(tpls)
#         
#         tmp_zip <- tempfile("edge_download_", fileext = ".zip")
#         on.exit(unlink(tmp_zip), add = TRUE)
#         
#         write_zip(tpls, tmp_zip)
#         
#         ok <- file.copy(tmp_zip, file, overwrite = TRUE)
#         if (!ok || !file.exists(file)) {
#           stop("Failed to copy ZIP to the download target.")
#         }
#       }
#     )
#     
#     # ── Complete: success modal + navigate + reset ──────────────────────────
#     observeEvent(input$complete, {
#       
#       current_session <- session
#       
#       showModal(modalDialog(
#         title     = NULL,
#         footer    = NULL,
#         easyClose = FALSE,
#         size      = "s",
#         div(
#           style = "text-align: center; padding: 1.5rem 1rem;",
#           div(
#             style = paste(
#               "width: 64px;",
#               "height: 64px;",
#               "border-radius: 50%;",
#               "background: #e6f4ea;",
#               "display: flex;",
#               "align-items: center;",
#               "justify-content: center;",
#               "margin: 0 auto 1rem auto;"
#             ),
#             tags$span(
#               style = "color: #28a745; font-size: 2rem; font-weight: 700;",
#               HTML("&check;")
#             )
#           ),
#           h4(
#             style = "margin-bottom: 0.5rem; color: #1d2a36;",
#             "Study processed successfully"
#           ),
#           p(
#             style = "color: #697786; margin-bottom: 0;",
#             "Opening the study library..."
#           )
#         )
#       ))
#       
#       later::later(function() {
#         shiny::withReactiveDomain(current_session, {
#           removeModal()
#           templates(NULL)
#           zip_path(NULL)
#           reset_shared_state()
#           current_step(NULL)
#           shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
#           shinyjs::runjs("$('body').addClass('sidebar-collapse')")
#         })
#       }, delay = 2)
#     })
#     
#     # ── Disable Complete until templates exist ──────────────────────────────
#     observe({
#       shinyjs::toggleState("complete", condition = !is.null(templates()))
#     })
#     
#   })
# }

step4_UI <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Card(
      title       = "Generate EDGE Templates",
      width       = 12,
      status      = "primary",
      solidHeader = FALSE,
      footer = tagList(
        downloadButton(ns("download_zip"), "Download ZIP", class = "btn-success"),
        actionButton(ns("complete"), "Complete and return to library", class = "btn-primary")
      ),
      div(
        style = "display: flex; align-items: center; gap: 1rem; margin-bottom: 1rem;",
        selectInput(ns("arm_select"), label = "Study Arm", choices = NULL, width = "200px"),
        uiOutput(ns("save_status"))
      ),
      reactableOutput(ns("preview_table")),
      
      hr(),
      h4("Template builder (preview)"),
      edgeBuilderUI(ns("edge_builder")),
      
      # ── ADDON ── custom activities panel ──────────────────────────────────
      hr(),
      customActivityUI(ns("custom_activities"))
      # ──────────────────────────────────────────────────────────────────────
    )
  )
}

step4_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    study_identity_params <- function() {
      list(
        as.character(shared_state$cpms_id),
        as.character(shared_state$study_site),
        as.character(shared_state$scenario_id)
      )
    }
    
    templates <- reactiveVal(NULL)
    zip_path  <- reactiveVal(NULL)
    
    # ── Reset helper for shared_state ──────────────────────────────────────────
    reset_shared_state <- function() {
      shared_state$scenario_id      <- NULL
      shared_state$study_site       <- NULL
      shared_state$edge_id          <- NULL
      shared_state$cpms_id          <- NULL
      shared_state$study_name       <- NULL
      shared_state$filename         <- NULL
      shared_state$upload_meta      <- NULL
      shared_state$raw_ict          <- NULL
      shared_state$posting_plan     <- NULL
      shared_state$processed_ict    <- NULL
      shared_state$evaluated_plan   <- NULL
      shared_state$edge_templates   <- NULL
      shared_state$speciality_id    <- NULL
      shared_state$speciality_name  <- NULL
      shared_state$current_step     <- NULL
      shared_state$timestamp        <- NULL
    }
    
    templates <- reactiveVal(NULL)
    zip_path  <- reactiveVal(NULL)
    
    # ── Edge template builder module ─────────────────────────────────────────
    edited_templates <- edgeBuilderServer(
      id             = "edge_builder",
      edge_templates = reactive(shared_state$edge_templates)
    )
    
    # ── ADDON ── custom activities module ─────────────────────────────────
    custom_activity_handles <- customActivityServer(
      id                = "custom_activities",
      auth_state        = auth_state,
      shared_state      = shared_state,
      study_arm_choices = reactive({
        tpl <- templates()
        if (is.null(tpl)) character(0) else names(tpl)
      })
    )
    # ──────────────────────────────────────────────────────────────────────
    
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
      Filter(function(d) !is.null(d) && nrow(d) > 0, tpls)
    }
    
    # Department is internal-only — drives builder read-only logic.
    # EDGE expects it blank on import, and the top preview represents the export.
    blank_department <- function(tpls) {
      lapply(tpls, function(d) {
        if ("Department" %in% names(d)) d$Department <- NA
        d
      })
    }
    
    write_zip <- function(tpls, zp) {
      
      tpls <- prepare_for_export(tpls)
      if (length(tpls) == 0) {
        stop("No templates with rows to export.")
      }
      tpls <- blank_department(tpls)
      
      
      if (length(tpls) == 0) {
        stop("No templates with rows to export.")
      }
      # Write CSVs locally first
      tmp_dir <- tempfile("edge_export_")
      dir.create(tmp_dir, recursive = TRUE)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      csv_files <- character(length(tpls))
      for (i in seq_along(tpls)) {
        nm <- names(tpls)[i]
        # Make filename safe
        safe_nm <- gsub("[^A-Za-z0-9_-]", "_", nm)
        csv <- file.path(tmp_dir, paste0(safe_nm, ".csv"))
        write.csv(
          tpls[[i]],
          file = csv,
          row.names = FALSE,
          na = ""
        )
        csv_files[i] <- csv
      }
      if (!all(file.exists(csv_files))) {
        stop("One or more CSV files were not created before zipping.")
      }
      # Create ZIP locally first
      local_zip <- tempfile("edge_zip_", fileext = ".zip")
      zip::zipr(
        zipfile = local_zip,
        files   = csv_files,
        root    = tmp_dir
      )
      if (!file.exists(local_zip) || file.info(local_zip)$size == 0) {
        stop("ZIP archive was not created locally.")
      }
      # Copy final ZIP to network/shared output path
      out_dir <- dirname(zp)
      if (!dir.exists(out_dir)) {
        dir.create(out_dir, recursive = TRUE)
      }
      ok <- file.copy(local_zip, zp, overwrite = TRUE)
      if (!ok || !file.exists(zp) || file.info(zp)$size == 0) {
        stop("ZIP was created locally but could not be copied to: ", zp)
      }
      invisible(zp)
    }

    log_step4_event <- function(level, message, details = list()) {
      log_event(
        level = level,
        area = "step4",
        message = message,
        user_id = auth_state$user_id,
        username = auth_state$username,
        cpms_id = shared_state$cpms_id,
        upload_id = shared_state$upload_id,
        session_id = auth_state$session_id,
        details = details
      )
    }
    
    # ── Generate templates on load ────────────────────────────────────────────
    observe({
      req(shared_state$current_step == "step4")
      req(shared_state$evaluated_plan)
      
      # Re-trigger when customs change so templates rebuild with/without them.
      # First entry: signal is 0; addon wipes; signal bumps to 1 → this observer
      # runs once more with customs cleared (no-op effectively).
      custom_activity_handles$invalidation_signal()
      
      w$show()

      log_step4_event(
        level = "INFO",
        message = "Posting line generation started",
        details = list(scenario_id = shared_state$scenario_id)
      )
      
      adjusted <- tryCatch({
        adjust_posting_lines(shared_state$evaluated_plan)
      }, error = function(e) {
        app_log_exception("step4", "Adjust posting lines failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id
        ))
        log_step4_event(
          level = "ERROR",
          message = "Posting line generation failed",
          details = list(
            stage = "adjust_posting_lines",
            error = conditionMessage(e)
          )
        )
        showNotification("Failed to adjust posting lines", type = "error")
        w$hide()
        return(NULL)
      })
      
      req(adjusted)
      adjusted$study_site <- shared_state$study_site
      
      adjusted <- adjusted %>% rename(Staff_Role = Staff.Role)
      
      # ── Attach cost centres ──────────────────────────────────────────────────
      adjusted <- tryCatch({
        add_cost_centres(adjusted, isolate(shared_state$speciality_name))
      }, error = function(e) {
        app_log_exception("step4", "Cost centre assignment failed", e, list(
          cpms_id = shared_state$cpms_id,
          speciality = isolate(shared_state$speciality_name)
        ))
        showNotification(
          paste("Failed to assign cost centres:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        return(adjusted)
      })
      
      adjusted <- tryCatch({
        assign_edge_keys(adjusted)
      }, error = function(e) {
        app_log_exception("step4", "EDGE key assignment failed", e, list(
          cpms_id = shared_state$cpms_id
        ))
        showNotification(
          paste("Failed to assign EDGE keys:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        return(adjusted)
      })
      
      # ── ADDON ── merge custom activities before persist + template build ──
      adjusted <- tryCatch({
        apply_custom_activities(adjusted, shared_state)
      }, error = function(e) {
        app_log_exception("step4", "Custom activity merge failed", e, list(
          cpms_id = shared_state$cpms_id
        ))
        showNotification(
          paste("Failed to merge custom activities:", conditionMessage(e)),
          type = "error",
          duration = 10
        )
        return(adjusted)   # fall back to pipeline-only output
      })
      # ──────────────────────────────────────────────────────────────────────
      
      tryCatch({
        dbExecute(CON,
                  paste(
                    "DELETE FROM posting_lines",
                    "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
                  ),
                  params = study_identity_params()
        )
        dbAppendTable(CON, "posting_lines", adjusted)
        log_step4_event(
          level = "INFO",
          message = "Posting line generation completed",
          details = list(rows = nrow(adjusted))
        )
        app_log_info("step4", "Posting lines saved", list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(adjusted)
        ))
      }, error = function(e) {
        app_log_exception("step4", "Posting lines persistence failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(adjusted)
        ))
        log_step4_event(
          level = "ERROR",
          message = "Persistence failed",
          details = list(
            stage = "posting_lines_persist",
            rows = nrow(adjusted),
            error = conditionMessage(e)
          )
        )
        showNotification("Failed to save posting lines", type = "error")
      })
      
      tmpl <- tryCatch({
        
        visit_lookup <- dbGetQuery(CON, "
                        SELECT DISTINCT Study, Study_Arm, Visit_Label, Visit_Number
                        FROM ict_costing_tbl
                        WHERE Visit_Label IS NOT NULL
                      ")
        
        templates <- build_all_edge_templates(adjusted, visit_lookup, shared_state$upload_meta$edge_id)
        
      }, error = function(e) {
        app_log_exception("step4", "EDGE template build failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          rows = nrow(adjusted)
        ))
        log_step4_event(
          level = "ERROR",
          message = "Posting line generation failed",
          details = list(
            stage = "template_build",
            rows = nrow(adjusted),
            error = conditionMessage(e)
          )
        )
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
        zip_name  <- paste0(
          shared_state$cpms_id,
          "_",
          shared_state$study_site,
          "_",
          shared_state$scenario_id,
          "_",
          timestamp,
          ".zip"
        )
        zp        <- file.path(EDGE_OUTPUT_DIR, zip_name)

        log_step4_event(
          level = "INFO",
          message = "ZIP generation started",
          details = list(
            template_count = length(tmpl),
            zip_name = zip_name
          )
        )
        
        if (!dir.exists(EDGE_OUTPUT_DIR)) dir.create(EDGE_OUTPUT_DIR, recursive = TRUE)
        
        write_zip(tmpl, zp)
        zip_path(zp)
        
        # Persist the ZIP path to meta_data for this study
        dbExecute(CON,
                  paste(
                    "UPDATE meta_data SET edge_zip_path = ?",
                    "WHERE cpms_id = ? AND study_site = ? AND scenario_id = ?"
                  ),
                  params = c(list(zp), study_identity_params())
        )

        log_step4_event(
          level = "INFO",
          message = "ZIP generation completed",
          details = list(
            template_count = length(tmpl),
            zip_name = zip_name
          )
        )
        
      }, error = function(e) {
        app_log_exception("step4", "ZIP generation failed", e, list(
          cpms_id = shared_state$cpms_id,
          upload_id = shared_state$upload_id,
          template_count = length(tmpl)
        ))
        log_step4_event(
          level = "ERROR",
          message = "Persistence failed",
          details = list(
            stage = "zip_generation",
            template_count = length(tmpl),
            error = conditionMessage(e)
          )
        )
        showNotification("Failed to save ZIP", type = "error")
      })
      
      updateSelectInput(session, "arm_select", choices = names(tmpl))
      
      w$hide()
      showNotification("Templates generated successfully", type = "message", duration = 5)
    })
    
    # ── Preview selected arm ──────────────────────────────────────────────────
    output$preview_table <- renderReactable({
      req(input$arm_select)
      
      tpls <- edited_templates()
      if (is.null(tpls) || length(tpls) == 0) tpls <- templates()
      
      req(tpls, input$arm_select %in% names(tpls))
      
      df <- blank_department(tpls)[[input$arm_select]]
      
      reactable(
        df,
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
      contentType = "application/zip",
      content = function(file) {
        tpls <- edited_templates()
        if (is.null(tpls) || length(tpls) == 0) {
          tpls <- templates()
        }
        
        req(tpls)
        
        tmp_zip <- tempfile("edge_download_", fileext = ".zip")
        on.exit(unlink(tmp_zip), add = TRUE)
        
        write_zip(tpls, tmp_zip)
        
        ok <- file.copy(tmp_zip, file, overwrite = TRUE)
        if (!ok || !file.exists(file)) {
          stop("Failed to copy ZIP to the download target.")
        }
      }
    )
    
    # ── Complete: success modal + navigate + reset ──────────────────────────
    observeEvent(input$complete, {
      
      current_session <- session
      
      showModal(modalDialog(
        title     = NULL,
        footer    = NULL,
        easyClose = FALSE,
        size      = "s",
        div(
          style = "text-align: center; padding: 1.5rem 1rem;",
          div(
            style = paste(
              "width: 64px;",
              "height: 64px;",
              "border-radius: 50%;",
              "background: #e6f4ea;",
              "display: flex;",
              "align-items: center;",
              "justify-content: center;",
              "margin: 0 auto 1rem auto;"
            ),
            tags$span(
              style = "color: #28a745; font-size: 2rem; font-weight: 700;",
              HTML("&check;")
            )
          ),
          h4(
            style = "margin-bottom: 0.5rem; color: #1d2a36;",
            "Study processed successfully"
          ),
          p(
            style = "color: #697786; margin-bottom: 0;",
            "Opening the study library..."
          )
        )
      ))
      
      later::later(function() {
        shiny::withReactiveDomain(current_session, {
          removeModal()
          templates(NULL)
          zip_path(NULL)
          reset_shared_state()
          current_step(NULL)
          shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
          shinyjs::runjs("$('body').addClass('sidebar-collapse')")
        })
      }, delay = 2)
    })
    
    # ── Disable Complete until templates exist ──────────────────────────────
    observe({
      shinyjs::toggleState("complete", condition = !is.null(templates()))
    })
    
  })
}
