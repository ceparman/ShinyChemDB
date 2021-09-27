

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
          column(6,actionButton(ns("runPubChemQuery"),"Run PubChem Query")),
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




      observeEvent(input$runPubChemQuery,{

        pubChemResult(pubchem_smiles_search(smiles = input$smilesTextInput,threshold = input$threshold,maxRecords = input$maxRecords,debug=DEBUG))


        })

       output$pubchemresult <- renderUI(

                                   tagList(

                                        DT::renderDataTable(pubChemResult())



                                 )
                      )



}
)}
