searchModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Search")
  )
}



searchModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {



    }
  )
}
