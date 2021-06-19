

queryModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(


          textInput(ns('smilesTextInput'), 'Smiles', width='100%'),
          textAreaInput(ns('molFileTextInput'), "Mol File"),
          textInput(inputId =ns('jmeFileTextInput'), 'JME string', width='100%'),
          actionButton(ns('readclipButton'), 'Read Clipboard'),
          actionButton(ns('useSmiButton'), 'Search Smiles'),
          actionButton(ns('useJmeButton'), 'Search JME'),
          actionButton(ns('useMolButton'), 'Search MOL file'),
          actionButton(ns('clearFieldsButton'), 'Clear Fields')
        ),

        mainPanel( fluidPage(
                    h3("Results"),
                   tabsetPanel(
                       tabPanel(title = "Local dB Search",id =ns("local")),
                       tabPanel(title = "PubChem Search", id  = ns("pubchem"))
                   )

        )

        )
      )
    )
  )
}



queryModuleServer <- function(id,clipboard) {
  moduleServer(
    id,

    function(input, output, session) {

  ns<- NS(id)





     # observeEvent(input$useSmiButton,)

      #observeEvent(input$useJmeButton, )

      #observeEvent(input$useMolButton, )


      observeEvent(input$clearFieldsButton, {
        updateTextInput(session, 'smilesTextInput', value='')
        updateTextInput(session, 'molFileTextInput', value='')
        updateTextInput(session, 'jmeFileTextInput', value='')
      })

     observeEvent(input$readclipButton,{
                  updateTextInput(session, 'smilesTextInput', value='')
                  updateTextInput(session, 'molFileTextInput', value='')
                  updateTextInput(session, 'jmeFileTextInput', value='')
                  updateTextInput(session,'smilesTextInput',value = clipboard())
                })

      }


  )
}
