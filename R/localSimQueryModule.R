

generate_local_similarity_values <- function(args){
                     local_similarity_search(smiles = args$smiles,
                                             db_info = args$db_info,
                                             threshold = args$threshold,
                                             max_records=args$max_records
                                             )
}


#
#
# generate_local_substructure_values <- function(args){
#                 local_smiles_substructure_search(smiles = args$smiles,
#                                                  max_mismatches = args$max_mismatches,
#                                                  min_overlap_coefficient = args$min_overlap_coefficient,
#                                                  al = args$al,
#                                                  au = args$au,
#                                                  bl = args$bl,
#                                                  bu = args$bu,
#                                                  db_info = args$db_info,
#                                                  numParallel = args$numParallel,
#                                                  threshold = args$threshold,
#                                                  max_records=args$max_records
#                                                  )
# }




localSimQueryModuleUI <- function(id) {
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
                   column(6,numericInput(ns("threshold"),"Min. Tanimoto score",min = 40,max = 100,step = 5,value = 70))
                   ),
          fluidRow(
                   column(6,actionButton(ns("runlocalQuery"),"Run Local Similarity Query"))
          ),



          hr(),

   #       h3("Substructure Search"),
         #  tabsetPanel(
         #
         #   tabPanel(id=ns("sim_search"),title = "Search",
         #    hr(),
         #    actionButton(ns("runlocalsubQuery"),"Run Local Substructure Query")
         #     ),
         #
         #
         #  tabPanel(id=ns("sim_search_details"),title = "Search Options",
         #  h4("Initial Search"),
         #  fluidRow(
         #    column(6,numericInput(ns("sim_threshold"),"Smilarity threshold",min = 40,max = 100,step = 5,value = 70)),
         #    column(6,numericInput(ns("maxmm"),"Max. fingerprint mismatches",min = 0,max = 5,step = 1,value = 2))
         #  ),
         # hr(),
         #  h4("Secondary Search"),
         #  fluidRow(
         #    column(6,numericInput(ns("min_overlap"),"Minimum Overlap",min = 40,max = 100,step = 5,value = 70)),
         #  ),
         #
         #  fluidRow(
         #   column(6,numericInput(ns("al"),"Lower bound for the number of atom mismatches",min = 0,max = 5,step = 1,value = 0)),
         #   column(6,numericInput(ns("au"), "Upper bound for the number of atom mismatches",min = 0,max = 10,step = 1,value = 2))
         #          ),
         #  fluidRow(
         #    column(6,numericInput(ns("bl"),"Lower bound for the number of bond mismatches", min = 0,max = 5,step = 1,value = 0)),
         #    column(6,numericInput(ns("bu"), "Upper bound for the number of bond mismatches",min = 0,max = 10,step = 1,value = 2))
         #   ),
         # fluidRow(
         #   column(6,numericInput(ns("ncores"),"Number of cores to use", min = 1,max = 5,step = 1,value = 2))
         # )
         #
         #
         #
         #
         #  )
         #  )

          ), #end sidepanel




        mainPanel(
          fluidPage(
                    h3("Results"),


                     reactable::reactableOutput(ns("localsimilarityresult")),
                      shinyjs::hidden(actionButton(ns("copy_sim"),"Copy SMILES to clipboard"))
                    )
                              #  tabPanel(title = "Local Substructure Search",id =ns("localsubstructure"),
                               #          uiOutput(ns("localsubstructure")))



       ) #end main panel

)
)#end fluidpage

)}



localSimQueryModuleServer <- function(id,clipboard,db_info) {
  moduleServer(
    id,

    function(input, output, session) {

      worker <- shiny.worker::initialize_worker()

     ns<- NS(id)

     # Store search results

     search_results <- reactiveValues()




     sim_result_table <- reactive({


       if( exists( "local_similarity_ValuesPromise")) {

         t <-  local_similarity_ValuesPromise()


         if (!is.null(t$result)) {

           data <- t$result |>
             select( "Common name", "tanimoto","ID","smiles", "Collection",
                     "smiles", "Molecular Weight", "Formula","link" )


           search_results$sim <- data

           reactable::reactable(
              data |> select(-link),
             columns = list(

               `Common name` = colDef(html = TRUE, cell = function(value,index) {
                 sprintf('<a href="%s" target="_blank">%s</a>',  data$link[index],data$`Common name`[index])
               }),

               tanimoto = colDef(name = "Tanimoto Score"),
               smiles= colDef(name = "Canonical SMILES"),
               Collection = colDef(name ="Collection"),
               ID = colDef(name = "ID"),
               Formula = colDef(name = "Molecular Formula"),
               `Molecular Weight`= colDef(name = "Molecular Weight")
             ),
             selection = "single", onClick = "select",
             searchable = TRUE
           )

         }else {

           reactable::reactable(data.frame(Message = "Searching"))

         } } else {

           reactable::reactable(data.frame(Message = "No Results"))
         }




     })



      observeEvent(input$clearFieldsButton, {
        updateTextInput(session, 'smilesTextInput', value='')
      })

      observeEvent(input$readclipButton,{
                  updateTextInput(session,'smilesTextInput',value = clipboard$smiles)
                })



      local_similarity_values <- reactive( {
        list( smiles = isolate(input$smilesTextInput),  #isolate values so you can make changes without rerunning
              threshold = isolate(input$threshold),
              max_records = isolate(input$maxRecords),
              db_info=db_info,
              input$runlocalQuery,
              debug=DEBUG
        )

      })

      local_substructure_values <- reactive( {
        list( smiles = isolate(input$smilesTextInput),  #isolate values so you can make changes without rerunning

               max_mismatches = isolate(input$maxmm),
               min_overlap_coefficient = isolate(input$min_overlap),
               al = isolate(input$al),
               au = isolate(input$au),
               bl = isolate(input$bl),
               bu = isolate(input$bu),
               db_info = db_info,
               numParallel = isolate(input$ncores),
               threshold = isolate(input$sim_threshold),
               max_records=isolate(input$maxRecords),
               input$runlocalsubQuery
              )
      })





      # #Define promise for Local results table

     local_similarity_ValuesPromise <-  worker$run_job("generatesimilarityValuesPromise",
                                           generate_local_similarity_values,
                                           args_reactive =   local_similarity_values,
                                           )

     local_substructure_ValuesPromise <-  worker$run_job("generatesubstructureValuesPromise",
                                                         generate_local_substructure_values,
                                                         args_reactive =   local_substructure_values,
     )



     observeEvent(input$runlocalQuery, priority = 0,{

       output$localsimilarityresult <- reactable::renderReactable( {  sim_result_table() })



      }) #end observe event





     observeEvent(input$runlocalsubQuery, priority = 0,{

       output$localsubstructure <- renderUI({
         t <-  local_substructure_ValuesPromise()


         if (!is.null(t$result)) {
           if(nrow(t$result) == 0) {

                  tagList(h3("No Results Found!"))

                } else{
                 tagList( reactable::reactable(t$result))
               }
           } else{
           tagList(fluidPage(

             fluidRow(
               column(12,
                      align = "center",
                      h3("Running Local Substructure Query")
               )

             ),

             HTML('<center> <img src="hug.gif"></center>'),

           )
           )
         }



       }) #end render UI



     }) #end observe event


     observeEvent(input$copy_sim,{

       selected_row <-   getReactableState("localsimilarityresult")$selected


       clipboard$smiles <- search_results$sim$smiles[selected_row]


     })

observe( {

   req(getReactableState("localsimilarityresult"))

  if(is.null(getReactableState("localsimilarityresult")$selected )) {
    shinyjs::hide(id= "copy_sim")
    shinyjs::disable(id= "copy_sim")

  } else {

    shinyjs::show(id= "copy_sim")
    shinyjs::enable(id= "copy_sim")
  }



})


}
)}
