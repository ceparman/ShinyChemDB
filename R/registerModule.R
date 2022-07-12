registerModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Register Molecule to Database"),
    textInput(ns("smilesTextInput"),"Smiles"),
    textInput(ns("id"),"ID for Molecule"),
    textInput(ns("id_type"),"ID Type"),
    textInput(ns("collection"),"Collection"),
    actionButton(ns("register"),"Register"),

    actionButton(ns("readclipButton"),"Copy Smiles form clipboard")

  )
}



registerModuleServer <- function(id,clipboard, db_info) {
  moduleServer(
    id,
    function(input, output, session) {



  observeEvent(input$readclipButton,{
       updateTextInput(session,'smilesTextInput',value = clipboard$smiles)
     })



    observeEvent(input$register,{

      mongochem::load_smiles(input$smilesTextInput, db_info, input$id, input$id_type, input$collection)


      })





    }
  )
}
