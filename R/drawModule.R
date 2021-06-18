drawModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(


        #  actionButton(ns('smilesButton'), 'Render Smiles'),
        #  actionButton(ns('molFileButton'), 'MOL File'),
        #  actionButton(ns('jmeFileButton'), 'JME File'),
        #  textInput(ns('smilesTextInput'), 'Smiles', width='100%'),
         #textOutput(ns('smilesTextInput')),
        #  tags$textarea(id=ns('molFileTextInput'), rows=6, '', style='width: 100%'),
      #    textInput(inputId =ns('jmeFileTextInput'), 'JME string', width='100%'),
          actionButton(ns('readclipButton'), 'Read Molecule from Clipboard'),
        #  actionButton(ns('readSampleMultipartStructureButton'), 'Read Multipart Structure'),
        #  actionButton(ns('readSampleReactionButton'), 'Read Reaction'),
        #  actionButton(ns('useJmeButton'), 'Use JME'),
        #  actionButton(ns('useMolButton'), 'Use MOL file'),
          actionButton(ns('clearEditorButton'), 'Clear Editor'),
       #   actionButton(ns('clearFieldsButton'), 'Clear Fields')
         fileInput(ns("fileupload"),label = "Load molecule from file",multiple = F,accept = c(".mol",".smi",".sdf")),
         actionButton(ns("sendtoclip"),label = "Send Smiles to Clipboard")
        ),

        mainPanel(
          jsmeOutput(ns('jsmeElement'), '650px', '650px'),
          textOutput(ns("test"))
        )
      )
    )
  )

}



drawModuleServer <- function(id,clipboard) {
  moduleServer(
       id,

      function(input, output, session) {

        ns<- NS(id)
        output$jsmeElement <- renderJsme(
          jsme()
        )



        observeEvent(input$fileupload,{


          type <- tolower(tools::file_ext(input$fileupload$name))

          switch( type,

                  mol =  {  smiles <- rcdk::get.smiles(rcdk::load.molecules( molfiles = input$fileupload$datapath)[[1]]) },

                  smi = { smiles <- readLines(input$fileupload$datapath,n=1)},

                  sdf = { smiles  <- sdf2smiles(read.SDFset(input$fileupload$datapath)[1]  ) },
                  {  } #error code


          ) #end switch

          useMOL(session, ns('jsmeElement'), ChemmineOB::convertFormat("SMI","MOL",smiles))

        })



        observeEvent(input$smilesButton, {
          smiles(session, ns('jsmeElement'), ns('inputSmiles'))
          updateTextInput(session, 'smilesTextInput', value=input$inputSmiles)

        })
    #    observeEvent(input$inputSmiles, {
    #      updateTextInput(session, 'smilesTextInput', value=input$inputSmiles)
    #    })



        observeEvent(input$molFileButton, {
          molFile(session, ns('jsmeElement'), ns('inputMolFile'))
          updateTextInput(session, 'molFileTextInput', value=input$inputMolFile)
        })
        observeEvent(input$inputMolFile, {
          updateTextInput(session, ns('molFileTextInput'), value=input$inputMolFile)
        })



        observeEvent(input$jmeFileButton, {
          jmeFile(session, ns('jsmeElement'), ns('inputJmeFile'))
          updateTextInput(session, ns('jmeFileTextInput'),value=input$inputJmeFile)
        })
        observeEvent(input$inputJmeFile, {
          updateTextInput(session, 'jmeFileTextInput', value=input$inputJmeFile)
        })




        observeEvent(input$useJmeButton, useJME(session, ns('jsmeElement'), input$jmeFileTextInput))

        observeEvent(input$useMolButton, useMOL(session, ns('jsmeElement'), input$molFileTextInput))

        observeEvent(input$readclipButton, useMOL(session, ns('jsmeElement'), ChemmineOB::convertFormat("SMI","MOL", clipboard()) ) )


        observeEvent(input$clearEditorButton, {
          resetEditor(session, ns('jsmeElement'))


        })
        observeEvent(input$clearFieldsButton, {
          updateTextInput(session, 'smilesTextInput', value='')
          updateTextInput(session, 'molFileTextInput', value='')
          updateTextInput(session, 'jmeFileTextInput', value='')

        })

        observeEvent(input$sendtoclip,{

          smiles(session, ns('jsmeElement'), ns('inputSmiles'))
          updateTextInput(session, 'smilesTextInput', value=input$inputSmiles)


        })



     observeEvent(input$inputSmiles,{
                  output$test <- renderText(input$inputSmiles)
                  clipboard(input$inputSmiles)
            })

      }


  )

    }
