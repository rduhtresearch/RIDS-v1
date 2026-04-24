# ─────────────────────────────────────────────────────────────────────────────
# library_mod.R — Study library browser
#
# Notes:
#   * Uses a single JS-delegated click handler instead of one observeEvent per
#     card. The previous pattern leaked observers on every studies() refresh.
#   * Card summary stats come from a LEFT JOIN aggregate, so the grid is one
#     query regardless of study count. Adjust the `line_total` column in the
#     subquery to whatever your posting_lines cost column is actually called
#     (fallback gracefully if it's missing — see tryCatch).
#   * CSS is consolidated in library_css(); safe to move to www/ if preferred.
# ─────────────────────────────────────────────────────────────────────────────

# ── Small helpers ------------------------------------------------------------

coalesce_na <- function(x, default = "—") {
  if (is.null(x) || length(x) == 0 || is.na(x)) default else x
}

fmt_gbp <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("—")
  if (abs(x) >= 1e6)   sprintf("£%.2fM", x / 1e6)
  else if (abs(x) >= 1e3) sprintf("£%.1fK", x / 1e3)
  else                 sprintf("£%s", format(round(x), big.mark = ","))
}

stat_cell <- function(label, value) {
  div(
    div(class = "lib-stat-label", label),
    div(class = "lib-stat-value", coalesce_na(value))
  )
}

# ── Styling ------------------------------------------------------------------

library_css <- function() {
  tags$style(HTML("
    /* Prevent body width shift when Bootstrap opens/closes modals */
    html { scrollbar-gutter: stable; }
    body.modal-open { padding-right: 0 !important; }

    .lib-toolbar {
      display: flex; align-items: center; gap: 0.75rem;
      flex-wrap: wrap; padding: 1rem 1rem 0.5rem;
    }
    .lib-toolbar .search-wrap { flex: 1 1 280px; min-width: 220px; position: relative; }
    .lib-toolbar .search-wrap input {
      width: 100%; padding: 0.55rem 0.85rem 0.55rem 2.25rem;
      border: 1px solid #e1e7ed; border-radius: 0.5rem;
      font-size: 0.9rem; background: #fff;
      transition: border-color 120ms ease, box-shadow 120ms ease;
    }
    .lib-toolbar .search-wrap input:focus {
      outline: none; border-color: #2b6cb0;
      box-shadow: 0 0 0 3px rgba(43,108,176,0.15);
    }
    .lib-toolbar .search-wrap svg {
      position: absolute; left: 0.75rem; top: 50%;
      transform: translateY(-50%); color: #8a96a3;
    }
    .lib-count { font-size: 0.85rem; color: #697786; margin-left: auto; }

    .lib-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
      gap: 1.25rem; padding: 1rem;
    }

    .lib-card {
      background: #fff; border: 1px solid #eef2f6; border-radius: 0.75rem;
      padding: 1.25rem; cursor: pointer;
      display: flex; flex-direction: column; gap: 0.85rem;
      transition: transform 120ms ease, box-shadow 120ms ease, border-color 120ms ease;
    }
    .lib-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 8px 24px rgba(18,34,48,0.08);
      border-color: #cfe0f0;
    }
    .lib-card:focus-visible {
      outline: 2px solid #2b6cb0; outline-offset: 2px;
    }

    .lib-card-title {
      font-size: 0.98rem; font-weight: 700; color: #1d2a36; line-height: 1.35;
      display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical;
      overflow: hidden;
    }

    .lib-chips { display: flex; flex-wrap: wrap; gap: 0.35rem; }
    .lib-chip {
      padding: 0.18rem 0.6rem; border-radius: 1rem;
      font-size: 0.72rem; font-weight: 600; letter-spacing: 0.01em;
    }
    .lib-chip--scenario { background: #e8f4fd; color: #1f5f8b; }
    .lib-chip--edge     { background: #f0f4f8; color: #556575; }

    .lib-stats {
      display: grid; grid-template-columns: 1fr 1fr; gap: 0.5rem;
      padding: 0.65rem 0;
      border-top: 1px solid #f0f4f8; border-bottom: 1px solid #f0f4f8;
    }
    .lib-stat-label {
      font-size: 0.68rem; color: #8a96a3;
      text-transform: uppercase; letter-spacing: 0.04em;
    }
    .lib-stat-value { font-size: 0.95rem; font-weight: 700; color: #1d2a36; }

    .lib-meta {
      display: flex; justify-content: space-between;
      font-size: 0.78rem; color: #697786;
    }
    .lib-meta i { margin-right: 0.25rem; }

    .lib-empty { text-align: center; padding: 3rem 1rem; color: #697786; }
    .lib-empty p { margin: 0.5rem 0; }

    .lib-modal-title-row { margin-bottom: 0.25rem; }
    .lib-modal-title { font-size: 1.3rem; font-weight: 700; color: #1d2a36; }
    .lib-modal-subtitle { font-size: 0.85rem; color: #697786; }
    .lib-modal-header {
      display: grid; grid-template-columns: repeat(4, 1fr);
      gap: 1rem; padding: 0.75rem 0 1rem;
      border-bottom: 1px solid #eef2f6; margin-bottom: 1rem;
    }
    .lib-modal-header .lib-stat-value { font-size: 1.15rem; }
  "))
}

# ── UI -----------------------------------------------------------------------

libraryUI <- function(id) {
  ns <- NS(id)
  tagList(
    library_css(),
    div(
      class = "lib-toolbar",
      div(
        class = "search-wrap",
        tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          width = "16", height = "16", viewBox = "0 0 24 24",
          fill = "none", stroke = "currentColor",
          `stroke-width` = "2", `stroke-linecap` = "round", `stroke-linejoin` = "round",
          tags$circle(cx = "11", cy = "11", r = "8"),
          tags$path(d = "m21 21-4.35-4.35")
        ),
        tags$input(
          id = ns("search"), type = "search",
          placeholder = "Search studies, CPMS ID, or uploader…",
          oninput = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority: 'event'});",
            ns("search")
          )
        )
      ),
      selectInput(ns("scenario_filter"), label = NULL,
                  choices = c("All scenarios" = ""), width = "180px"),
      div(class = "lib-count", textOutput(ns("count_text"), inline = TRUE))
    ),
    uiOutput(ns("study_cards"))
  )
}

# ── Server -------------------------------------------------------------------

libraryServer <- function(id, auth_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Data: one joined query for the whole grid ---------------------------
    studies_raw <- reactive({
      sql_with_stats <- "
        SELECT  m.cpms_id, m.study_name, m.scenario_id, m.edge_id,
                m.uploaded_by, m.upload_timestamp,
                COALESCE(p.line_count, 0)  AS line_count,
                COALESCE(p.total_cost, 0)  AS total_cost
        FROM    meta_data m
        LEFT JOIN (
          SELECT  cpms_id,
                  COUNT(*)                    AS line_count,
                  SUM(COALESCE(line_total, 0)) AS total_cost
          FROM    posting_lines
          GROUP BY cpms_id
        ) p ON p.cpms_id = m.cpms_id
        ORDER BY m.upload_timestamp DESC
      "
      sql_fallback <- "
        SELECT cpms_id, study_name, scenario_id, edge_id,
               uploaded_by, upload_timestamp,
               CAST(NULL AS INTEGER) AS line_count,
               CAST(NULL AS DOUBLE)  AS total_cost
        FROM meta_data
        ORDER BY upload_timestamp DESC
      "
      tryCatch(
        DBI::dbGetQuery(CON, sql_with_stats),
        error = function(e) {
          warning("library_mod: stats aggregation failed (", conditionMessage(e),
                  "); falling back to plain meta_data query.")
          DBI::dbGetQuery(CON, sql_fallback)
        }
      )
    })
    
    # Populate scenario filter from data
    observe({
      scenarios <- sort(unique(stats::na.omit(studies_raw()$scenario_id)))
      updateSelectInput(
        session, "scenario_filter",
        choices = c("All scenarios" = "",
                    setNames(scenarios, paste("Scenario", scenarios))),
        selected = isolate(input$scenario_filter) %||% ""
      )
    })
    
    # ── Filtering -----------------------------------------------------------
    studies <- reactive({
      df <- studies_raw()
      
      q <- input$search
      if (!is.null(q) && nzchar(trimws(q))) {
        ql <- tolower(trimws(q))
        hit <- grepl(ql, tolower(df$study_name), fixed = TRUE) |
          grepl(ql, tolower(as.character(df$cpms_id)), fixed = TRUE) |
          grepl(ql, tolower(df$uploaded_by), fixed = TRUE)
        df <- df[hit, , drop = FALSE]
      }
      
      sc <- input$scenario_filter
      if (!is.null(sc) && nzchar(sc)) {
        df <- df[as.character(df$scenario_id) == sc, , drop = FALSE]
      }
      df
    })
    
    output$count_text <- renderText({
      n <- nrow(studies()); total <- nrow(studies_raw())
      if (n == total) sprintf("%d studies", n)
      else            sprintf("%d of %d studies", n, total)
    })
    
    # ── Click → modal (single observer, no intermediate reactiveVal) --------
    # Shiny.setInputValue is called with priority:'event', so re-clicking the
    # same card re-fires this even though the value is unchanged.
    observeEvent(input$card_clicked, {
      cpms <- input$card_clicked
      df   <- studies_raw()
      row  <- df[as.character(df$cpms_id) == as.character(cpms), , drop = FALSE]
      req(nrow(row) > 0)
      row  <- row[1, ]
      
      posting_data <- DBI::dbGetQuery(
        CON,
        "SELECT * FROM posting_lines WHERE cpms_id = ?",
        params = list(as.character(row$cpms_id))
      )
      
      total_cost <- if ("line_total" %in% names(posting_data)) {
        sum(posting_data$line_total, na.rm = TRUE)
      } else NA_real_
      
      showModal(modalDialog(
        title = NULL, size = "xl", easyClose = TRUE,
        footer = modalButton("Close"),
        
        div(
          class = "lib-modal-title-row",
          div(class = "lib-modal-title", row$study_name),
          div(class = "lib-modal-subtitle",
              sprintf("CPMS %s · EDGE %s", row$cpms_id, row$edge_id))
        ),
        div(
          class = "lib-modal-header",
          stat_cell("Scenario",      row$scenario_id),
          stat_cell("Posting lines", format(nrow(posting_data), big.mark = ",")),
          stat_cell("Total cost",    fmt_gbp(total_cost)),
          stat_cell("Uploaded by",   row$uploaded_by)
        ),
        
        if (nrow(posting_data) == 0) {
          div(class = "lib-empty",
              icon("inbox", style = "font-size: 2rem; color: #c2ccd6;"),
              p("No posting lines found for this study."))
        } else {
          reactable(
            posting_data,
            striped = TRUE, highlight = TRUE, compact = TRUE,
            rownames = FALSE, pagination = TRUE, searchable = TRUE,
            resizable = TRUE, wrap = FALSE,
            defaultPageSize = 15,
            defaultColDef = colDef(minWidth = 120)
          )
        }
      ))
    }, ignoreInit = TRUE)
    
    # ── Render grid ---------------------------------------------------------
    output$study_cards <- renderUI({
      df <- studies()
      
      if (nrow(df) == 0) {
        return(div(
          class = "lib-empty",
          icon("folder-open", style = "font-size: 2.5rem; color: #c2ccd6;"),
          p(style = "font-weight: 600;", "No studies match your filters."),
          p(style = "font-size: 0.85rem;", "Try clearing the search or scenario filter.")
        ))
      }
      
      cards <- lapply(seq_len(nrow(df)), function(i) {
        row  <- df[i, ]
        cpms <- as.character(row$cpms_id)
        
        click_js <- sprintf(
          "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
          ns("card_clicked"),
          gsub("'", "\\\\'", cpms, fixed = TRUE)
        )
        
        tags$article(
          class        = "lib-card",
          tabindex     = "0",
          role         = "button",
          `aria-label` = paste("View", row$study_name),
          onclick      = click_js,
          onkeydown    = sprintf(
            "if(event.key==='Enter'||event.key===' '){event.preventDefault();%s}",
            click_js
          ),
          div(class = "lib-card-title", row$study_name),
          div(
            class = "lib-chips",
            span(class = "lib-chip lib-chip--scenario",
                 paste("Scenario", row$scenario_id)),
            span(class = "lib-chip lib-chip--edge",
                 paste("EDGE", row$edge_id))
          ),
          div(
            class = "lib-stats",
            div(
              div(class = "lib-stat-label", "Posting lines"),
              div(class = "lib-stat-value",
                  if (is.na(row$line_count)) "—"
                  else format(row$line_count, big.mark = ","))
            ),
            div(
              div(class = "lib-stat-label", "Total cost"),
              div(class = "lib-stat-value", fmt_gbp(row$total_cost))
            )
          ),
          div(
            class = "lib-meta",
            span(icon("user"), row$uploaded_by),
            span(icon("clock"),
                 format(as.POSIXct(row$upload_timestamp), "%d %b %Y"))
          )
        )
      })
      
      div(class = "lib-grid", cards)
    })
  })
}