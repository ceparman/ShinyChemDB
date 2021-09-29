

generate_pubchem_values <- function(args){
  pubchem_smiles_smilarity_search(smiles = args$smiles,
                                  threshold = args$threshold,
                                   maxRecords = args$maxRecords,
                                   debug=args$debug)
}



pubchemQueryModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarLayout(
        sidebarPanel(

          textAreaInput(ns('smilesTextInput'), 'Smiles for Query', width='100%',height = '100%'),


          hr(),

          h3("Create Query String"),
          actionButton(ns('readclipButton'), 'Read Clipboard Smiles'),
          actionButton(ns('clearFieldsButton'), 'Clear Inputs'),

          hr(),
          h3("Similarity Search"),

          fluidRow(
                   column(6,numericInput(ns("threshold"),"Min. Tanimoto score",min = 70,max = 100,step = 5,value = 95)),
                   column(6,numericInput(ns("maxRecords"),"Max, number of hits",min=1,max=100,step=1,value=21))

            ),

          actionButton(ns("runPubChemQuery"),"Run PubChem Similarity Query"),
          hr(),

          h3("Substructure Search"),

          fluidRow(
            column(4,checkboxInput(ns("MatchIsotopes"),"MatchIsotopes",value = F)),
            column(4,checkboxInput(ns("MatchCharges"),"MatchCharges",value = F))
           ),
          fluidRow(
            column(4,checkboxInput(ns("MatchTautomers"),"MatchTautomers",value = F)),
            column(4,checkboxInput(ns("RingsNotEmbedded"),"RingsNotEmbedded",value = F))
          ),
          fluidRow(
            column(4,checkboxInput(ns("SingleDoubleBondsMatch"),"ChainsMatchRing",value = F)),
            column(4,checkboxInput(ns("ChainsMatchRing"),"ChainsMatchRing",value = F))
          ),

          fluidRow(
            column(4,checkboxInput(ns("StripHydrogen"),"StripHydrogen",value = F)),
            column(4,selectInput(ns("Stereo"),"Stereo",
                                  choices = c("ignore", "exact", "relative", "nonconflictingvalue"),
                                      selected= "ignore"))
          ),
           numericInput(ns("maxRecords2"),"Max, number of hits",min=1,max=100,step=1,value=20)


          ),


        mainPanel( fluidPage(
                    h3("Results"),


                    tabsetPanel(id=ns("results"),
                                tabPanel(title = "PubChem Search", id  = ns("pubchemsimilarity"),
                                        uiOutput(ns("pubchemsimilarityresult"))),
                                tabPanel(title = "pubchemsubsructureresult",id =ns("pubchemsubstructure"),
                                         uiOutput(ns("pubchemsubstructure")))

                         )

              )
       )

)
)
)}



pubchemQueryModuleServer <- function(id,clipboard) {
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

      # pubchem_values <- reactive( {
      #   list( smiles = input$smilesTextInput,  #isolate values so you can make changes without rerunning
      #         threshold = input$threshold,
      #         maxRecords = input$maxRecords,
      #         debug=DEBUG
      #      )
      # })

      pubchem_values <- reactive( {
        list( smiles = isolate(input$smilesTextInput),  #isolate values so you can make changes without rerunning
              threshold = isolate(input$threshold),
              maxRecords = isolate(input$maxRecords),
              input$runPubChemQuery,
              debug=DEBUG
        )

      })






      # #Define promise for Pubchem results table

     pubchemValuesPromise <-  worker$run_job("generateValuesPromise",
                                           generate_pubchem_values,
                                           args_reactive =   pubchem_values,
                                           )



     observeEvent(input$runPubChemQuery, priority = 0,{

       output$pubchemsimilarityresult <- renderUI({
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



           }) #end render UI
      })#end observe event



}
)}
