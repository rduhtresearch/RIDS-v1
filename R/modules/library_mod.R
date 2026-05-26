# libraryUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("study_cards"))
#   )
# }
# 
# libraryServer <- function(id, auth_state) {
#   moduleServer(id, function(input, output, session) {
#     
#     # ── Load studies ──────────────────────────────────────────────────────────
#     studies <- reactive({
#       DBI::dbGetQuery(CON,
#                       "SELECT cpms_id, study_name, scenario_id, edge_id, uploaded_by, upload_timestamp
#          FROM meta_data
#          ORDER BY upload_timestamp DESC"
#       )
#     })
#     
#     # ── Track selected study ──────────────────────────────────────────────────
#     selected_study <- reactiveVal(NULL)
#     
#     observe({
#       req(nrow(studies()) > 0)
#       lapply(seq_len(nrow(studies())), function(i) {
#         observeEvent(input[[paste0("view_study_", i)]], {
#           selected_study(studies()[i, ])
#         }, ignoreInit = TRUE)
#       })
#     })
#     
#     # ── Show modal ────────────────────────────────────────────────────────────
#     observeEvent(selected_study(), {
#       req(selected_study())
#       row <- selected_study()
#       
#       posting_data <- DBI::dbGetQuery(CON,
#                                       "SELECT * FROM posting_lines WHERE cpms_id = ?",
#                                       params = list(as.character(row$cpms_id))
#       )
#       
#       showModal(modalDialog(
#         title     = row$study_name,
#         size      = "xl",
#         easyClose = TRUE,
#         footer    = actionButton(session$ns("close_modal"), "Close", class = "btn-default"),
#         if (nrow(posting_data) == 0) {
#           p(style = "color: #697786;", "No posting lines found for this study.")
#         } else {
#           reactable(
#             posting_data,
#             striped       = TRUE,
#             highlight     = TRUE,
#             compact       = TRUE,
#             rownames      = FALSE,
#             pagination    = TRUE,
#             resizable     = TRUE,
#             wrap          = FALSE,
#             defaultColDef = colDef(minWidth = 120)
#           )
#         }
#       ))
#     })
#     
#     observeEvent(input$close_modal, {
#       removeModal()
#       selected_study(NULL)
#     })
#     
#     # ── Render cards ──────────────────────────────────────────────────────────
#     output$study_cards <- renderUI({
#       req(nrow(studies()) > 0)
#       
#       div(
#         style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1.5rem; padding: 1rem;",
#         lapply(seq_len(nrow(studies())), function(i) {
#           row <- studies()[i, ]
#           
#           div(
#             class = "card",
#             style = "border: 1px solid #f0f4f8; border-radius: 0.75rem; box-shadow: 0 2px 12px rgba(18,34,48,0.06);",
#             div(
#               class = "card-body",
#               style = "padding: 1.25rem;",
#               div(
#                 style = "font-size: 1rem; font-weight: 700; color: #1d2a36; margin-bottom: 0.75rem;",
#                 row$study_name
#               ),
#               div(
#                 style = "display: flex; flex-wrap: wrap; gap: 0.4rem; margin-bottom: 1rem;",
#                 span(
#                   style = "background: #e8f4fd; color: #1f5f8b; padding: 0.2rem 0.65rem; border-radius: 1rem; font-size: 0.75rem; font-weight: 600;",
#                   paste0("Scenario ", row$scenario_id)
#                 ),
#                 span(
#                   style = "background: #f0f4f8; color: #697786; padding: 0.2rem 0.65rem; border-radius: 1rem; font-size: 0.75rem; font-weight: 600;",
#                   paste0("EDGE: ", row$edge_id)
#                 )
#               ),
#               div(
#                 style = "display: flex; flex-direction: column; gap: 0.35rem; margin-bottom: 1rem;",
#                 div(
#                   style = "display: flex; align-items: center; gap: 0.5rem;",
#                   span(style = "font-size: 0.75rem; color: #697786;", icon("user")),
#                   span(style = "font-size: 0.82rem; color: #1d2a36;", row$uploaded_by)
#                 ),
#                 div(
#                   style = "display: flex; align-items: center; gap: 0.5rem;",
#                   span(style = "font-size: 0.75rem; color: #697786;", icon("clock")),
#                   span(style = "font-size: 0.82rem; color: #1d2a36;",
#                        format(as.POSIXct(row$upload_timestamp), "%d %b %Y %H:%M"))
#                 )
#               ),
#               div(
#                 style = "border-top: 1px solid #f0f4f8; padding-top: 0.75rem;",
#                 actionButton(
#                   inputId = session$ns(paste0("view_study_", i)),
#                   label   = tagList(icon("eye"), " View"),
#                   class   = "btn btn-sm btn-outline-primary",
#                   style   = "font-size: 0.8rem; font-weight: 600;"
#                 )
#               )
#             )
#           )
#         })
#       )
#     })
#     
#   })
# } 

libraryUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("study_cards"))
  )
}

libraryServer <- function(id, auth_state, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    build_study_ref <- function(row) {
      list(
        cpms_id = as.character(row$cpms_id),
        study_site = as.character(row$study_site),
        scenario_id = as.character(row$scenario_id)
      )
    }
    
    # ── Track selected study ──────────────────────────────────────────────────
    selected_study <- reactiveVal(NULL)
    
    # ── Wire card click handlers ──────────────────────────────────────────────
    observe({
      studies <- DBI::dbGetQuery(CON,
                                 "SELECT
            REPLACE(cpms_id, chr(0), '') AS cpms_id,
            REPLACE(study_site, chr(0), '') AS study_site,
            REPLACE(study_name, chr(0), '') AS study_name,
            REPLACE(scenario_id, chr(0), '') AS scenario_id,
            REPLACE(edge_id, chr(0), '') AS edge_id,
            REPLACE(uploaded_by, chr(0), '') AS uploaded_by,
            upload_timestamp
         FROM meta_data
         ORDER BY upload_timestamp DESC"
      )
      req(nrow(studies) > 0)
      
      lapply(seq_len(nrow(studies)), function(i) {
        observeEvent(input[[paste0("view_study_", i)]], {
          selected_study(studies[i, ])
        }, ignoreInit = TRUE)
      })
    })
    
    # ── Open the selected study in the workspace ─────────────────────────────
    observeEvent(selected_study(), {
      req(selected_study())
      row <- selected_study()
      
      shared_state$current_study <- build_study_ref(row)
      shinyjs::runjs('$("a[data-value=\'tab_study\']").trigger("click")')
      
      selected_study(NULL)
    }, ignoreNULL = TRUE)
    
    # ── Render cards ──────────────────────────────────────────────────────────
    output$study_cards <- renderUI({
      studies <- DBI::dbGetQuery(CON,
                                 "SELECT
            REPLACE(cpms_id, chr(0), '') AS cpms_id,
            REPLACE(study_site, chr(0), '') AS study_site,
            REPLACE(study_name, chr(0), '') AS study_name,
            REPLACE(scenario_id, chr(0), '') AS scenario_id,
            REPLACE(edge_id, chr(0), '') AS edge_id,
            REPLACE(uploaded_by, chr(0), '') AS uploaded_by,
            upload_timestamp
         FROM meta_data
         ORDER BY upload_timestamp DESC"
      )
      req(nrow(studies) > 0)
      
      div(
        style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 1.5rem; padding: 1rem;",
        lapply(seq_len(nrow(studies)), function(i) {
          row <- studies[i, ]
          
          div(
            class = "card",
            style = "border: 1px solid #f0f4f8; border-radius: 0.75rem; box-shadow: 0 2px 12px rgba(18,34,48,0.06);",
            div(
              class = "card-body",
              style = "padding: 1.25rem;",
              div(
                style = "font-size: 1rem; font-weight: 700; color: #1d2a36; margin-bottom: 0.75rem;",
                row$study_name
              ),
              div(
                style = "display: flex; flex-wrap: wrap; gap: 0.4rem; margin-bottom: 1rem;",
                span(
                  style = "background: #e8f4fd; color: #1f5f8b; padding: 0.2rem 0.65rem; border-radius: 1rem; font-size: 0.75rem; font-weight: 600;",
                  paste0("Scenario ", row$scenario_id)
                ),
                span(
                  style = "background: #edf7ed; color: #1f6f43; padding: 0.2rem 0.65rem; border-radius: 1rem; font-size: 0.75rem; font-weight: 600;",
                  row$study_site %||% "Site unknown"
                ),
                span(
                  style = "background: #f0f4f8; color: #697786; padding: 0.2rem 0.65rem; border-radius: 1rem; font-size: 0.75rem; font-weight: 600;",
                  paste0("EDGE: ", row$edge_id)
                )
              ),
              div(
                style = "display: flex; flex-direction: column; gap: 0.35rem; margin-bottom: 1rem;",
                div(
                  style = "display: flex; align-items: center; gap: 0.5rem;",
                  span(style = "font-size: 0.75rem; color: #697786;", icon("user")),
                  span(style = "font-size: 0.82rem; color: #1d2a36;", row$uploaded_by)
                ),
                div(
                  style = "display: flex; align-items: center; gap: 0.5rem;",
                  span(style = "font-size: 0.75rem; color: #697786;", icon("clock")),
                  span(style = "font-size: 0.82rem; color: #1d2a36;",
                       format(as.POSIXct(row$upload_timestamp), "%d %b %Y %H:%M"))
                )
              ),
              div(
                style = "border-top: 1px solid #f0f4f8; padding-top: 0.75rem;",
                actionButton(
                  inputId = ns(paste0("view_study_", i)),
                  label   = tagList(icon("folder-open"), " Open"),
                  class   = "btn btn-sm btn-outline-primary",
                  style   = "font-size: 0.8rem; font-weight: 600;"
                )
              )
            )
          )
        })
      )
    })
    
  })
}

`%||%` <- function(a, b) {
  if (is.null(a) || is.na(a) || !nzchar(as.character(a))) b else a
}
