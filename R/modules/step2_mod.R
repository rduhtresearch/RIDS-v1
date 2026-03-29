step2_UI <- function(id) {
  ns <- NS(id)
  tagList(
    h1('step2'),
  )
}

step2_Server <- function(id, auth_state, shared_state) {
  moduleServer(
    id,
    function(input, output, session) {
     

    }
  )
}