mainUI <- function(id) {
  ns <- NS(id)
  tagList(
  h1('helloe world')
  )
}

nameServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      return(NULL)
    }
  )
}

