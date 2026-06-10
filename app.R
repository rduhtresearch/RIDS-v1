source("R/setup.r", local = FALSE)
source("global.R", local = FALSE)
source("R/modules/login_mod.R")
source("R/modules/sidebar_mod.R")
source("R/modules/app_mod.R")
source("R/modules/reporting_mod.R")
source("R/modules/settings_mod.R")
source("R/modules/step1_mod.R")
source("R/modules/step2_mod.R")
source("R/modules/step3_mod.R")
source("R/modules/step4_mod.R")
source("R/modules/support_mod.R")
source("R/modules/admin_mod.r")
source("R/modules/progress_mod.R")
source("R/modules/help_mod.R")
source("R/modules/library_mod.R")
source("R/modules/edge_builder_mod.R")
source("R/modules/study_workspace_mod.R")
source("R/modules/custom_activity_module.R",  local = FALSE)

ui <- tagList(
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = sprintf("styles.css?v=%s", as.integer(file.info("www/styles.css")$mtime))
    ),
    tags$script(src = "app-shell.js")
  ),
  dashboardPage(
    dark = NULL,
    help = NULL,
    header = dashboardHeader(title = dashboardBrand(title = span(
      style = "font-weight: 700; color: #1d2a36;",
      "RIDS ",
      span(
        style = "font-size: 0.7rem; color: #697786; font-weight: 400;",
        "VERSION TEST (hello world)"
      )
    )), rightUi = uiOutput("user_badge")),
    sidebar = dashboardSidebar(
      skin = "light",
      collapsed = FALSE,
      minified = FALSE,
      expandOnHover = FALSE,
      fixed = TRUE,
      width = 250,
      sidebarUI("sidebar")
    ),
    body = dashboardBody(
      useWaiter() ,
      useShinyjs(),
      dev_banner(),
      useShinyFeedback(),
      loginUI("login"),
      appUI("app"),
      div(
        style = paste(
          "position: fixed;",
          "bottom: 0.5rem;",
          "left: 50%;",
          "transform: translateX(-50%);",
          "padding: 0.3rem 0.8rem;",
          "font-size: 0.72rem;",
          "color: #aaa;",
          "background: rgba(255,255,255,0.85);",
          "border: 1px solid #eee;",
          "border-radius: 12px;",
          "z-index: 1040;"
        ),
        paste0("v", APP_VERSION, " · last updated ", APP_LAST_UPDATED)
      )
    ) 
  )
)

server <- function(input, output, session) {
  shutdown_requested <- FALSE

  request_app_shutdown <- function(reason) {
    if (isTRUE(shutdown_requested)) {
      return(invisible(FALSE))
    }

    shutdown_requested <<- TRUE
    app_log_info("shutdown", paste("Stopping RIDS app:", reason))
    stopApp()
    invisible(TRUE)
  }

  session$userData$request_app_shutdown <- request_app_shutdown
  session$onSessionEnded(function() {
    request_app_shutdown("browser session ended")
  })
  
  output$user_badge <- renderUI({
    req(auth_state$logged_in)
    
    div(
      style = "display: flex; align-items: center; gap: 0.5rem; padding: 0 1rem;",
      span(
        style = "font-size: 0.85rem; font-weight: 600; color: #1d2a36;",
        auth_state$name %||% auth_state$username
      ),
      span(
        style = sprintf(
          "background: %s; color: %s; padding: 0.2rem 0.6rem; border-radius: 1rem; font-size: 0.75rem; font-weight: 600;",
          if (isTRUE(is_admin(auth_state$role))) "#e8f4fd" else "#f0f4f8",
          if (isTRUE(is_admin(auth_state$role))) "#1f5f8b" else "#6c757d"
        ),
        tools::toTitleCase(auth_state$role %||% "user")
      )
    )
  })
  
  pipeline_tabs <- c("tab_step1", "tab_step2", "tab_step3", "tab_step4")
  current_step <- reactiveVal(NULL)
  auth_state <- loginServer("login")

  observe({
    current <- input$sidebar
    if (!is.null(current) && current %in% pipeline_tabs) {
      current_step(gsub("tab_", "", current))
    } else {
      current_step(NULL)
    }
  })

  observe({
    if (!isTRUE(is_admin(auth_state$role)) && identical(input$sidebar, "tab_admin")) {
      updateTabItems(session, "sidebar", selected = "tab_dashboard")
    }
  })

  observe({
    session$sendCustomMessage(
      "setAppShell",
      isTRUE(auth_state$auth_ready) &&
        isTRUE(auth_state$logged_in) &&
        !isTRUE(auth_state$must_change_password)
    )
  })
  
  observe({
    if (!isTRUE(auth_state$auth_ready)) {
      shinyjs::hide("login-overlay")
      return()
    }

    if (isTRUE(auth_state$logged_in) && !isTRUE(auth_state$must_change_password)) {
      shinyjs::hide("login-overlay")
      updateTabItems(session, "sidebar", selected = "tab_dashboard")
    } else {
      shinyjs::show("login-overlay")
      if (!isTRUE(auth_state$logged_in)) {
        updateTabItems(session, "sidebar", selected = "tab_dashboard")
      }
    }
  })
  
  sidebarServer("sidebar", auth_state, session, current_step)
  appServer("app", auth_state, current_step)
  adminServer("admin", auth_state)
}

shinyApp(ui, server)
