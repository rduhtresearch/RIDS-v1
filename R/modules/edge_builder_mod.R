edgeBuilderUI <- function(id) {
  ns <- NS(id)
  
  bs4Card(
    title       = "Template builder",
    width       = 12,
    status      = "primary",
    solidHeader = FALSE,
    footer = tagList(
      actionButton(ns("reset"), "Reset to original", class = "btn-outline-secondary btn-sm")
    ),
    fluidRow(
      column(
        width = 4,
        h4("Templates"),
        uiOutput(ns("template_list"))
      ),
      column(
        width = 8,
        h4(textOutput(ns("active_title"))),
        uiOutput(ns("readonly_notice")),
        reactableOutput(ns("rows_table")),
        div(
          style = "margin-top: 0.5rem; display: flex; align-items: center; gap: 1rem;",
          actionButton(ns("move_selected"), "Move selected...", class = "btn-primary"),
          span(
            style = "font-size: 0.85rem; color: #697786;",
            textOutput(ns("selected_count"), inline = TRUE)
          )
        )
      )
    )
  )
}

edgeBuilderServer <- function(id, edge_templates) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    NEW_SENTINEL <- "__new__"
    
    rv <- reactiveValues(
      original  = NULL,
      templates = NULL,
      movable   = character(0),
      active    = NULL,
      selected  = integer(0)
    )
    
    # ── Helpers ──────────────────────────────────────────────────────────────
    compute_movable <- function(tpls) {
      names(tpls)[vapply(tpls, function(d) {
        "Department" %in% names(d) && any(!is.na(d$Department))
      }, logical(1))]
    }
    
    is_movable <- function(nm) nm %in% rv$movable
    
    move_rows <- function(source, target, indices) {
      moving <- rv$templates[[source]][indices, , drop = FALSE]
      moving$`Template Name` <- target
      
      new_templates <- rv$templates
      new_templates[[source]] <- new_templates[[source]][-indices, , drop = FALSE]
      new_templates[[target]] <- bind_rows(new_templates[[target]], moving)
      
      rv$templates <- new_templates
    }
    
    new_name_validity <- reactive({
      raw <- input$new_template_name
      if (is.null(raw)) return(list(valid = FALSE, msg = NULL))
      
      trimmed <- trimws(raw)
      if (trimmed == "")                    return(list(valid = FALSE, msg = "Required"))
      if (trimmed %in% names(rv$templates)) return(list(valid = FALSE, msg = "Name already used"))
      if (nchar(trimmed) > 60)              return(list(valid = FALSE, msg = "Too long (max 60 chars)"))
      list(valid = TRUE, msg = NULL, name = trimmed)
    })
    
    # ── Initialise from upstream ─────────────────────────────────────────────
    observeEvent(edge_templates(), {
      req(edge_templates())
      tpls <- edge_templates()
      
      rv$original  <- tpls
      rv$templates <- tpls
      rv$movable   <- compute_movable(tpls)
      
      if (is.null(rv$active) || !(rv$active %in% names(tpls))) {
        rv$active <- if (length(rv$movable) > 0) rv$movable[1] else names(tpls)[1]
      }
    })
    
    # ── Reset ────────────────────────────────────────────────────────────────
    observeEvent(input$reset, {
      req(rv$original)
      
      showModal(modalDialog(
        title = "Reset templates?",
        p("This will discard all moves and restore the original templates."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_reset"), "Reset", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_reset, {
      rv$templates <- rv$original
      rv$selected  <- integer(0)
      rv$active    <- if (length(rv$movable) > 0) rv$movable[1] else names(rv$original)[1]
      
      removeModal()
      showNotification("Templates reset to original", type = "message", duration = 2)
    })
    
    # ── Left pane ────────────────────────────────────────────────────────────
    output$template_list <- renderUI({
      req(rv$templates)
      
      tagList(
        lapply(names(rv$templates), function(nm) {
          n_rows <- nrow(rv$templates[[nm]])
          label  <- paste0(nm, " (", n_rows, " rows)")
          if (!is_movable(nm)) label <- paste0(label, " — read-only")
          
          div(
            style = "padding: 0.4rem 0;",
            actionLink(
              inputId = ns(paste0("sel_", nm)),
              label   = label
            )
          )
        })
      )
    })
    
    observe({
      req(rv$templates)
      
      lapply(names(rv$templates), function(nm) {
        observeEvent(input[[paste0("sel_", nm)]], {
          rv$active   <- nm
          rv$selected <- integer(0)
        }, ignoreInit = TRUE)
      })
    })
    
    # ── Right pane: title + read-only notice ────────────────────────────────
    output$active_title <- renderText({
      req(rv$active)
      rv$active
    })
    
    output$readonly_notice <- renderUI({
      req(rv$active)
      if (is_movable(rv$active)) return(NULL)
      
      div(
        style = paste(
          "background: #fff8e1;",
          "border-left: 3px solid #f0ad4e;",
          "padding: 0.5rem 0.75rem;",
          "margin: 0.5rem 0;",
          "font-size: 0.85rem;",
          "color: #6b5400;",
          "border-radius: 3px;"
        ),
        "Main arm template — combined activities, read-only in this view"
      )
    })
    
    # ── Reactable ────────────────────────────────────────────────────────────
    output$rows_table <- renderReactable({
      req(rv$active, rv$templates)
      
      df <- rv$templates[[rv$active]]
      
      # Pin column order: Department first, then everything else in its existing order
      preferred_order <- c("Department", "Cost Item Description", "Default Cost", "Analysis Code")
      df_cols         <- c(intersect(preferred_order, names(df)),
                           setdiff(names(df), preferred_order))
      df              <- df[, df_cols, drop = FALSE]
      
      visible_cols <- intersect(
        c("Department", "Cost Item Description", "Default Cost", "Analysis Code"),
        names(df)
      )
      hidden_cols  <- setdiff(names(df), visible_cols)
      
      col_defs <- c(
        list(
          `Department` = colDef(
            name     = "Department",
            minWidth = 140,
            cell = function(value) {
              if (is.na(value) || value == "") return("—")
              tags$span(
                style = paste(
                  "display: inline-block;",
                  "background: #e8f0f7;",
                  "color: #1f5f8b;",
                  "padding: 0.15rem 0.6rem;",
                  "border-radius: 12px;",
                  "font-size: 0.78rem;",
                  "font-weight: 500;",
                  "white-space: nowrap;"
                ),
                value
              )
            },
            html = TRUE
          ),
          `Cost Item Description` = colDef(name = "Description", minWidth = 240),
          `Default Cost` = colDef(
            name     = "Cost",
            minWidth = 100,
            align    = "right",
            format   = colFormat(prefix = "£", separators = TRUE, digits = 2)
          ),
          `Analysis Code` = colDef(name = "Code", minWidth = 110)
        ),
        setNames(lapply(hidden_cols, function(x) colDef(show = FALSE)), hidden_cols)
      )
      
      reactable(
        df,
        columns       = col_defs,
        selection     = "multiple",
        onClick       = "select",
        striped       = TRUE,
        highlight     = TRUE,
        compact       = TRUE,
        rownames      = FALSE,
        pagination    = FALSE,
        height        = 480,
        resizable     = TRUE,
        wrap          = FALSE,
        defaultColDef = colDef(minWidth = 120)
      )
    })
    
    # ── Selection ────────────────────────────────────────────────────────────
    observe({
      sel <- getReactableState("rows_table", "selected")
      rv$selected <- if (is.null(sel)) integer(0) else sel
    })
    
    output$selected_count <- renderText({
      paste0(length(rv$selected), " selected")
    })
    
    observe({
      can_move <- length(rv$selected) > 0 && is_movable(rv$active)
      shinyjs::toggleState("move_selected", condition = can_move)
    })
    
    # ── Move modal ───────────────────────────────────────────────────────────
    observeEvent(input$move_selected, {
      req(length(rv$selected) > 0, rv$active, rv$templates, is_movable(rv$active))
      
      existing_targets <- setdiff(rv$movable, rv$active)
      
      target_choices        <- c(existing_targets, NEW_SENTINEL)
      names(target_choices) <- c(existing_targets, "+ New template...")
      
      showModal(modalDialog(
        title = paste0("Move ", length(rv$selected), " rows to..."),
        selectInput(
          ns("move_target"),
          label   = "Target template",
          choices = target_choices
        ),
        shinyjs::hidden(
          div(
            id = ns("new_name_wrap"),
            textInput(ns("new_template_name"), "New template name", value = "")
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_move"), "Confirm", class = "btn-primary")
        )
      ))
    })
    
    observe({
      req(input$move_target)
      shinyjs::toggle("new_name_wrap", condition = input$move_target == NEW_SENTINEL)
    })
    
    observe({
      req(input$move_target)
      ok <- if (input$move_target == NEW_SENTINEL) {
        new_name_validity()$valid
      } else {
        TRUE
      }
      shinyjs::toggleState("confirm_move", condition = ok)
    })
    
    observe({
      req(input$move_target == NEW_SENTINEL)
      v <- new_name_validity()
      feedbackDanger(
        inputId = ns("new_template_name"),
        show    = !v$valid && !is.null(v$msg),
        text    = v$msg
      )
    })
    
    # ── Confirm move ─────────────────────────────────────────────────────────
    observeEvent(input$confirm_move, {
      req(input$move_target, length(rv$selected) > 0, rv$active)
      
      source  <- rv$active
      indices <- rv$selected
      n       <- length(indices)
      
      if (input$move_target == NEW_SENTINEL) {
        v <- new_name_validity()
        req(v$valid)
        
        new_name <- v$name
        rv$templates[[new_name]] <- rv$templates[[source]][0, ]
        rv$movable <- c(rv$movable, new_name)
        
        move_rows(source, new_name, indices)
        
        rv$active   <- new_name
        rv$selected <- integer(0)
        removeModal()
        
        showNotification(
          paste0("Created '", new_name, "' and moved ", n, " rows"),
          type = "message", duration = 3
        )
      } else {
        target <- input$move_target
        move_rows(source, target, indices)
        
        rv$selected <- integer(0)
        removeModal()
        
        showNotification(
          paste0("Moved ", n, " rows to ", target),
          type = "message", duration = 3
        )
      }
    })
    
    return(reactive(rv$templates))
  })
}