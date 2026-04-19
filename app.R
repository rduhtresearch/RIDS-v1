source("R/setup.r", local = FALSE)
source("global.R", local = FALSE)
source("R/modules/login_mod.R")
source("R/modules/sidebar_mod.R")
source("R/modules/app_mod.R")
source("R/modules/step1_mod.R")
source("R/modules/step2_mod.R")
source("R/modules/step3_mod.R")
source("R/modules/admin_mod.R")
source("R/modules/progress_mod.R")
source("R/modules/help_mod.R")

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
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
        "pre-alpha"
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
      useShinyjs(),
      useShinyFeedback(),
      loginUI("login"),
      appUI("app")
      
    ) 
  )
)

server <- function(input, output, session) {
  
  output$user_badge <- renderUI({
    req(auth_state$logged_in)
    
    div(
      style = "display: flex; align-items: center; gap: 0.5rem; padding: 0 1rem;",
      span(
        style = "font-size: 0.85rem; font-weight: 600; color: #1d2a36;",
        auth_state$username
      ),
      span(
        style = sprintf(
          "background: %s; color: %s; padding: 0.2rem 0.6rem; border-radius: 1rem; font-size: 0.75rem; font-weight: 600;",
          if (isTRUE(auth_state$role == "admin")) "#e8f4fd" else "#f0f4f8",
          if (isTRUE(auth_state$role == "admin")) "#1f5f8b" else "#6c757d"
        ),
        if (isTRUE(auth_state$role == "admin")) "Admin" else "User"
      )
    )
  })
  
  current_step <- reactiveVal(NULL)
  auth_state <- loginServer("login")

  observe({
    session$sendCustomMessage("setAppShell", isTRUE(auth_state$logged_in))
  })
  
  observe({
    if (auth_state$logged_in) {
      shinyjs::hide("login-overlay")
      updateTabItems(session, "sidebar", selected = "tab_dashboard")
    } else {
      shinyjs::show("login-overlay")
    }
  })
  
  sidebarServer("sidebar", auth_state, session, current_step)
  appServer("app", auth_state, current_step)
  adminServer("admin", auth_state)
}

shinyApp(ui, server)
