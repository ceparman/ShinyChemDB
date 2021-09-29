

generate_pubchem_values <- function(args){
  pubchem_smiles_search(smiles = args$smiles,
                        threshold = args$threshold,
                        maxRecords = args$maxRecords,
                        debug=args$debug)
}



queryModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(


          textInput(ns('smilesTextInput'), 'Smiles for Query', width='100%'),

          hr(),

          h3("Create Query String"),
          actionButton(ns('readclipButton'), 'Read Clipboard Smiles'),
          actionButton(ns('clearFieldsButton'), 'Clear Inputs'),

          hr(),

          fluidRow(
                   column(6,numericInput(ns("threshold"),"Min. Tanimoto score",min = 70,max = 100,step = 5,value = 95)),
                   column(6,numericInput(ns("maxRecords"),"Max, number of hits",min=1,max=100,step=1,value=20))

            ),
          fluidRow(
          column(6,actionButton(ns("runPubChemQuery"),"Run PubChem Similarity Query")),
          column(6,actionButton(ns("runLocalQuery"),"Run local Query"))
          )
          ),

        mainPanel( fluidPage(
                    h3("Results"),


                    tabsetPanel(id=ns("results"),
                                tabPanel(title = "PubChem Search", id  = ns("pubchem"),
                                        uiOutput(ns("pubchemresult")))  ,
                                tabPanel(title = "Local dB Search",id =ns("local"),
                                         uiOutput(ns("localresult")))

                         )

              )
       )
    ) #sidebar
  ) #FluidPage
  )
}



queryModuleServer <- function(id,clipboard) {
  moduleServer(
    id,

    function(input, output, session) {

      worker <- initialize_worker()

     ns<- NS(id)

      pubChemResult <- reactiveVal(NULL)
      localResult <- reactiveVal(NULL)

      observeEvent(input$clearFieldsButton, {
        updateTextInput(session, 'smilesTextInput', value='')
      #  updateTextInput(session, 'molFileTextInput', value='')
      #  updateTextInput(session, 'sdfFileTextInput', value='')
      })

      observeEvent(input$readclipButton,{
                 # updateTextInput(session, 'smilesTextInput', value='')
               #   updateTextInput(session, 'molFileTextInput', value='')
                #  updateTextInput(session, 'sdfFileTextInput', value='')
                  updateTextInput(session,'smilesTextInput',value = clipboard$smiles)
                })

      pubchem_values <- reactive( {
        list( smiles = input$smilesTextInput,
              threshold = input$threshold,
              maxRecords = input$maxRecords,
              debug=DEBUG
           )
      })


      #Define promise for Pubchem results table
      observeEvent(input$runPubChemQuery,{ pubchemValuesPromise <-  worker$run_job("generateValuesPromise",
                                              generate_pubchem_values,
                                              args_reactive = pubchem_values
                                              )



       output$pubchemresult <- renderUI({
                                        t <-  pubchemValuesPromise()


                                         if (!is.null(t$result)) {
                                               tagList(  DT::renderDataTable(t$result))
                                         } else{
                                           tagList(fluidPage(

                                             fluidRow(
                                               column(12,
                                                      align = "center",
                                               h3("Running PubMed Query")
                                               )

                                               ),

                                                    HTML('<center> <img src="hug.gif"></center>'),

                                                    )
                                           )
                                        }



       })
      })



}
)}
