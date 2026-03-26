source("R/setup.r", local = FALSE)
source("global.R", local = FALSE)
source("R/modules/login_mod.R")
source("R/modules/app_mod.R")

ui <- uiOutput('view')

server <- function(input, output, session) {
  
  auth_state <- loginServer("login")
  
  output$view <- renderUI({
    if (auth_state$logged_in == FALSE) {
       loginUI("login")
    } else {
       mainUI("app")
     }
  })
  
}

shinyApp(ui, server)
# # ==============================================================================
# # STEP BAR CSS
# # ==============================================================================
# step_bar_css <- tags$style(HTML("
#   .step-bar {
#     display: flex;
#     align-items: center;
#     margin: 1rem 0;
#   }
#   .step-bar .step {
#     display: flex;
#     flex-direction: column;
#     align-items: center;
#     position: relative;
#     flex: 1;
#   }
#   .step-bar .step-circle {
#     width: 36px;
#     height: 36px;
#     border-radius: 50%;
#     background: #dee2e6;
#     color: #fff;
#     display: flex;
#     align-items: center;
#     justify-content: center;
#     font-weight: 700;
#     font-size: 14px;
#     z-index: 1;
#     transition: background 0.3s;
#   }
#   .step-bar .step.done   .step-circle { background: #28a745; }
#   .step-bar .step.active .step-circle { background: #007bff; }
#   .step-bar .step-label {
#     margin-top: 6px;
#     font-size: 12px;
#     color: #6c757d;
#     text-align: center;
#   }
#   .step-bar .step.done   .step-label,
#   .step-bar .step.active .step-label { color: #212529; font-weight: 600; }
#   .step-bar .step-line {
#     flex: 1;
#     height: 3px;
#     background: #dee2e6;
#     transition: background 0.3s;
#     margin-bottom: 18px; /* vertically align with circles */
#   }
#   .step-bar .step-line.done { background: #28a745; }
# "))
# 
# # ==============================================================================
# # HELPER: build step bar UI
# # ==============================================================================
# make_step_bar <- function(steps, current) {
#   # steps: character vector of labels
#   # current: integer, 1-based index of active step
#   n <- length(steps)
#   els <- list()
#   
#   for (i in seq_along(steps)) {
#     state <- if (i < current) "done" else if (i == current) "active" else ""
#     
#     els[[length(els) + 1]] <- tags$div(
#       class = paste("step", state),
#       tags$div(class = "step-circle",
#                if (i < current) icon("check") else as.character(i)
#       ),
#       tags$div(class = "step-label", steps[i])
#     )
#     
#     if (i < n) {
#       line_class <- if (i < current) "step-line done" else "step-line"
#       els[[length(els) + 1]] <- tags$div(class = line_class)
#     }
#   }
#   
#   tags$div(class = "step-bar", els)
# }
# 
# # ==============================================================================
# # UI
# # ==============================================================================
# STEPS <- c("Upload", "Validate", "Process", "Review", "Done")
# 
# ui <- dashboardPage(
#   header  = dashboardHeader(title = "Step Progress"),
#   sidebar = dashboardSidebar(
#     sidebarMenu(menuItem("Demo", tabName = "demo", icon = icon("gauge")))
#   ),
#   body = dashboardBody(
#     step_bar_css,
#     tabItems(
#       tabItem(
#         tabName = "demo",
#         fluidRow(
#           box(
#             title = "Pipeline Progress",
#             status = "primary",
#             solidHeader = TRUE,
#             width = 12,
#             uiOutput("step_bar_ui"),
#             br(),
#             uiOutput("step_message"),
#             br(),
#             actionButton("prev_step", "Back",     class = "btn-secondary"),
#             actionButton("next_step", "Continue", class = "btn-primary")
#           )
#         )
#       )
#     )
#   )
# )
# 
# # ==============================================================================
# # SERVER
# # ==============================================================================
# server <- function(input, output, session) {
#   
#   current_step <- reactiveVal(1)
#   
#   observeEvent(input$next_step, {
#     if (current_step() < length(STEPS)) current_step(current_step() + 1)
#   })
#   
#   observeEvent(input$prev_step, {
#     if (current_step() > 1) current_step(current_step() - 1)
#   })
#   
#   output$step_bar_ui <- renderUI({
#     make_step_bar(STEPS, current_step())
#   })
#   
#   output$step_message <- renderUI({
#     msgs <- c(
#       "Upload your ICT activity workbook.",
#       "Validating column structure and data types.",
#       "Applying cost adjustment rules.",
#       "Review the adjusted posting lines.",
#       "All done — ready to export EDGE template."
#     )
#     tags$p(class = "text-muted", msgs[current_step()])
#   })
#   
#   # Disable buttons at boundaries
#   observe({
#     shinyjs::toggleState("prev_step", current_step() > 1)
#     shinyjs::toggleState("next_step", current_step() < length(STEPS))
#   })
# }
# 
# # ==============================================================================
# # RUN
# # ==============================================================================
# shinyApp(ui, server)
# ui <- fluidPage(
#   h1('test')
# ) 
# 
# server <- function(input, output, session) {
#   
# }
# 
# shinyApp(ui, server)
