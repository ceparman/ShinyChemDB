

generate_pubchem_similarity_values <- function(args){
  pubchem_smiles_smilarity_search(smiles = args$smiles,
                                  threshold = args$threshold,
                                   maxRecords = args$maxRecords,
                                   debug=args$debug)
}




generate_pubchem_substructure_values <- function(args){
  pubchem_smiles_substructure_search(smiles = args$smiles,
                                     MatchIsotopes=args$MatchIsotopes,
                                     MatchCharges=args$MatchCharges,
                                     MatchTautomers=args$MatchTautomers,
                                     RingsNotEmbedded=args$RingsNotEmbedded,
                                     SingleDoubleBondsMatch=args$SingleDoubleBondsMatch,
                                     ChainsMatchRings=args$ChainsMatchRings,
                                     StripHydrogen=args$StripHydrogen,
                                     Stereo=args$Stereo,
                                     maxRecords=args$maxRecords,
                                     debug=DEBUG)
}





pubchemQueryModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      useShinyjs(),
      sidebarLayout(
          sidebarPanel(width = 3,

          textAreaInput(ns('smilesTextInput'), 'Smiles for Query', width='100%',height = '50%'),
          hr(),

          h4("Create Query String"),
          fluidRow(
            column(6,actionButton(ns('readclipButton'), 'Read Clipboard Smiles')),
            column(6,actionButton(ns('clearFieldsButton'), 'Clear Inputs'))
          ),
          fluidRow(
          column(6,numericInput(ns("maxRecords"),"Max. number of hits",min=1,max=100,step=1,value=20)
                 )
          ),
          hr(),
          h4("Similarity Search"),

          fluidRow(
                   column(6,numericInput(ns("threshold"),"Min. Tanimoto score",min = 70,max = 100,step = 5,value = 95))
                   ),
          fluidRow(
                   column(6,actionButton(ns("runPubChemQuery"),"Run PubChem Similarity Query"))
          ),



          hr(),

          h3("Substructure Search"),
          tabsetPanel(

              tabPanel(id=ns("sim_search"),title = "Search",
            hr(),
            actionButton(ns("runPubChemsubQuery"),"Run PubChem Substructure Query")
             ),


          tabPanel(id=ns("sim_search_details"),title = "Search Options",
          fluidRow(
            column(6,checkboxInput(ns("MatchIsotopes"),"MatchIsotopes",value = F)),
            column(6,checkboxInput(ns("MatchCharges"),"MatchCharges",value = F))
           ),
          fluidRow(
            column(6,checkboxInput(ns("MatchTautomers"),"MatchTautomers",value = F)),
            column(6,checkboxInput(ns("RingsNotEmbedded"),"RingsNotEmbedded",value = F))
          ),
          fluidRow(
            column(6,checkboxInput(ns("SingleDoubleBondsMatch"),"ChainsMatchRing",value = T)),
            column(6,checkboxInput(ns("ChainsMatchRing"),"ChainsMatchRing",value = T))
          ),

          fluidRow(
            column(6,checkboxInput(ns("StripHydrogen"),"StripHydrogen",value = F)),
            column(6,selectInput(ns("Stereo"),"Stereo",
                                  choices = c("ignore", "exact", "relative", "nonconflictingvalue"),
                                      selected= "ignore"))
          )
          )
          )
          ),


        mainPanel( fluidPage(
                    h3("Results"),

                    tabsetPanel(id=ns("results"),
                                tabPanel(title = "PubChem Similarity Search", id  = ns("pubchemsimilarity"),
                                        uiOutput(ns("pubchemsimilarityresult"))),
                                tabPanel(title = "PubChem Substructure Search",id =ns("pubchemsubstructure"),
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



      observeEvent(input$clearFieldsButton, {
        updateTextInput(session, 'smilesTextInput', value='')
      })

      observeEvent(input$readclipButton,{
                  updateTextInput(session,'smilesTextInput',value = clipboard$smiles)
                })



      pubchem_similarity_values <- reactive( {
        list( smiles = isolate(input$smilesTextInput),  #isolate values so you can make changes without rerunning
              threshold = isolate(input$threshold),
              maxRecords = isolate(input$maxRecords),
              input$runPubChemQuery,
              debug=DEBUG
        )

      })

      pubchem_substructure_values <- reactive( {
        list( smiles = isolate(input$smilesTextInput),  #isolate values so you can make changes without rerunning
              MatchIsotopes=isolate(input$MatchIsotopes),
              MatchCharges=isolate(input$MatchCharges),
              MatchTautomers=isolate(input$MatchTautomers),
              RingsNotEmbedded=isolate(input$RingsNotEmbedded),
              SingleDoubleBondsMatch=isolate(input$SingleDoubleBondsMatch),
              ChainsMatchRings=isolate(input$ChainsMatchRings),
              StripHydrogen=isolate(input$StripHydrogen),
              Stereo=isolate(input$Stereo),
              maxRecords = isolate(input$maxRecords),
              input$runPubChemsubQuery,
              debug=DEBUG
        )

      })




      # #Define promise for Pubchem results table

     pubchem_similarity_ValuesPromise <-  worker$run_job("generatesimilarityValuesPromise",
                                           generate_pubchem_similarity_values,
                                           args_reactive =   pubchem_similarity_values,
                                           )

     pubchem_substructure_ValuesPromise <-  worker$run_job("generatesubstructureValuesPromise",
                                                         generate_pubchem_substructure_values,
                                                         args_reactive =   pubchem_substructure_values,
     )



     observeEvent(input$runPubChemQuery, priority = 0,{

       output$pubchemsimilarityresult <- renderUI({
                                        t <-  pubchem_similarity_ValuesPromise()


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



      }) #end observe event





     observeEvent(input$runPubChemsubQuery, priority = 0,{

       output$pubchemsubstructure <- renderUI({
         t <-  pubchem_substructure_ValuesPromise()


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



     }) #end observe event





}
)}
