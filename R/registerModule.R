registerModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Register")
  )
}



registerModuleServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {



    }
  )
}
