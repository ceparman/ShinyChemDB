queryModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("query")
  )
}



queryModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {



    }
  )
}
