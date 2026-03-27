step1_UI <- function(id) {
  ns <- NS(id)
  div(
    selectInput(ns('scenario'))
  )
}

step1_Server <- function(id, auth_state, shared_state) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}