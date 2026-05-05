studyWorkspaceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = paste(
        "display: flex;",
        "justify-content: space-between;",
        "align-items: center;",
        "padding: 0.75rem 1rem;",
        "border-bottom: 1px solid #eee;",
        "margin-bottom: 1rem;"
      ),
      actionLink(
        inputId = ns("back_to_library"),
        label   = tagList(icon("arrow-left"), " Back to library"),
        style   = "font-size: 0.9rem; color: #1f5f8b; text-decoration: none;"
      ),
      div(
        style = "font-size: 1.05rem; font-weight: 600; color: #1d2a36;",
        textOutput(ns("workspace_title"), inline = TRUE)
      )
    ),
    
    uiOutput(ns("body"))
  )
}

studyWorkspaceServer <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Helper: render a value, falling back to em-dash if NA/empty ──────────
    fmt_value <- function(x) {
      if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) {
        return(tags$span(style = "color: #aaa;", "—"))
      }
      as.character(x)
    }
    
    # ── Look up the active study's metadata ──────────────────────────────────
    study_meta <- reactive({
      req(shared_state$current_study_id)
      
      df <- DBI::dbGetQuery(CON,
                            "SELECT m.cpms_id, m.study_name, m.scenario_id, m.edge_id,
                m.uploaded_by, m.upload_timestamp, m.original_filename, m.notes,
                m.speciality_id, s.name AS speciality_name
         FROM meta_data m
         LEFT JOIN specialities s ON m.speciality_id = s.id
         WHERE m.cpms_id = ?",
                            params = list(as.character(shared_state$current_study_id))
      )
      
      if (nrow(df) == 0) return(NULL)
      df[1, ]
    })
    
    # ── Posting lines row count for active study ─────────────────────────────
    posting_count <- reactive({
      req(shared_state$current_study_id)
      
      df <- DBI::dbGetQuery(CON,
                            "SELECT COUNT(*) AS n FROM posting_lines WHERE cpms_id = ?",
                            params = list(as.character(shared_state$current_study_id))
      )
      
      df$n[1]
    })
    
    # ── Header title ─────────────────────────────────────────────────────────
    output$workspace_title <- renderText({
      if (is.null(shared_state$current_study_id)) return("No study selected")
      
      meta <- study_meta()
      if (is.null(meta)) {
        return(paste0("CPMS ", shared_state$current_study_id))
      }
      
      if (is.na(meta$study_name) || !nzchar(meta$study_name)) {
        paste0("CPMS ", meta$cpms_id)
      } else {
        paste0(meta$study_name, " · CPMS ", meta$cpms_id)
      }
    })
    
    # ── Body: tabs ───────────────────────────────────────────────────────────
    output$body <- renderUI({
      if (is.null(shared_state$current_study_id)) {
        return(p(
          style = "color: #697786; padding: 1rem;",
          "No study selected. Open one from the library."
        ))
      }
      
      meta <- study_meta()
      if (is.null(meta)) {
        return(p(
          style = "color: #c0392b; padding: 1rem;",
          paste0("Study not found: CPMS ", shared_state$current_study_id)
        ))
      }
      
      # ── Overview tab content ─────────────────────────────────────────────
      uploaded_at <- tryCatch(
        format(as.POSIXct(meta$upload_timestamp), "%d %b %Y %H:%M"),
        error = function(e) "—"
      )
      uploader_blob <- if (is.na(meta$uploaded_by) || !nzchar(meta$uploaded_by)) {
        uploaded_at
      } else {
        paste0(meta$uploaded_by, " · ", uploaded_at)
      }
      
      kv <- function(label, value) {
        div(
          style = "display: grid; grid-template-columns: 180px 1fr; gap: 1rem; padding: 0.45rem 0;",
          div(
            style = "font-size: 0.82rem; color: #697786; font-weight: 500;",
            label
          ),
          div(
            style = "font-size: 0.92rem; color: #1d2a36;",
            value
          )
        )
      }
      
      overview_panel <- div(
        style = "padding: 1rem;",
        kv("CPMS ID",        fmt_value(meta$cpms_id)),
        kv("EDGE ID",        fmt_value(meta$edge_id)),
        kv("Scenario",       fmt_value(meta$scenario_id)),
        kv("Speciality",     fmt_value(meta$speciality_name)),
        kv("Original file",  fmt_value(meta$original_filename)),
        kv("Uploaded by",    uploader_blob),
        
        hr(style = "margin: 1rem 0;"),
        
        div(
          style = "font-size: 0.82rem; color: #697786; font-weight: 500; margin-bottom: 0.4rem;",
          "Notes"
        ),
        div(
          style = "font-size: 0.9rem; color: #1d2a36; line-height: 1.5;",
          if (is.na(meta$notes) || !nzchar(meta$notes)) {
            tags$span(style = "color: #aaa; font-style: italic;", "No notes")
          } else {
            meta$notes
          }
        )
      )
      
      # ── Posting lines tab content ────────────────────────────────────────
      n_posting <- posting_count()
      
      posting_panel <- div(
        style = "padding: 1rem;",
        if (n_posting == 0) {
          p(
            style = "color: #aaa; font-style: italic;",
            "No posting lines yet for this study."
          )
        } else {
          tagList(
            p(
              style = "color: #697786; margin-bottom: 1rem;",
              "Download the posting lines for this study as a CSV file."
            ),
            div(
              style = "font-size: 0.82rem; color: #697786; margin-bottom: 1rem;",
              format(n_posting, big.mark = ","), " posting lines"
            ),
            downloadButton(
              ns("download_posting_lines"),
              label = "Download posting lines (CSV)",
              class = "btn-primary"
            )
          )
        }
      )
      
      # ── Tab structure ────────────────────────────────────────────────────
      bs4Card(
        title       = NULL,
        width       = 12,
        status      = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        
        tabsetPanel(
          id = ns("workspace_tabs"),
          tabPanel("Overview",       overview_panel),
          tabPanel("Posting lines",  posting_panel)
        )
      )
    })
    
    # ── Download handler ─────────────────────────────────────────────────────
    output$download_posting_lines <- downloadHandler(
      filename = function() {
        req(shared_state$current_study_id)
        meta <- study_meta()
        req(meta)
        
        cpms <- meta$cpms_id
        name <- if (!is.na(meta$study_name) && nzchar(meta$study_name)) {
          gsub("[^A-Za-z0-9_-]+", "_", meta$study_name)
        } else {
          "study"
        }
        ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
        paste0(cpms, "_", name, "_posting_lines_", ts, ".csv")
      },
      content = function(file) {
        req(shared_state$current_study_id)
        
        df <- DBI::dbGetQuery(CON,
                              "SELECT * FROM posting_lines WHERE cpms_id = ?",
                              params = list(as.character(shared_state$current_study_id))
        )
        
        if (nrow(df) == 0) {
          showNotification("No posting lines found for this study", type = "warning")
        }
        
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # ── Back button ──────────────────────────────────────────────────────────
    observeEvent(input$back_to_library, {
      shared_state$current_study_id <- NULL
      shinyjs::runjs('$("a[data-value=\'tab_library\']").trigger("click")')
    })
    
  })
}