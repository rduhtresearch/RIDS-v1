step4_UI <- function(id) {
  ns <- NS(id)
  bs4Card(
    title       = "Generate EDGE Templates",
    width       = 12,
    status      = "primary",
    solidHeader = FALSE,
    h1("Step 4")
  )
}

step4_Server <- function(id, auth_state, shared_state, current_step) {
  moduleServer(id, function(input, output, session) {
    
  })
}