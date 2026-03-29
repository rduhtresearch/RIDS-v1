source("R/setup.r", local = FALSE)
source("global.R", local = FALSE)
source("R/modules/login_mod.R")
source("R/modules/sidebar_mod.R")
source("R/modules/app_mod.R")
source("R/modules/step1_mod.R")
source("R/modules/step2_mod.R")

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "app-shell.js")
  ),
  dashboardPage(
    header = dashboardHeader(title = dashboardBrand(title = "RIDS")),
    sidebar = dashboardSidebar(
      skin ='light',
      collapsed = TRUE,
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
  
  auth_state <- loginServer("login")

  observe({
    session$sendCustomMessage("setAppShell", isTRUE(auth_state$logged_in))
  })
  
  observe({
    if (auth_state$logged_in) {
      shinyjs::hide("login-overlay")
    } else {
      shinyjs::show("login-overlay")
    }
  })
  
  sidebarServer("sidebar", auth_state, session)
  appServer("app", auth_state)
}

shinyApp(ui, server)
