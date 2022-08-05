library(BiocManager)
options(repos = BiocManager::repositories())


source("./global.R")

#options(shiny.port=9001)
#options(shiny.host = "192.168.1.70")

# Define UI for application
    ui <- dashboardPage(

            dashboardHeader(title="ShinyChemDB"),
            dashboardSidebar(
              sidebarMenu(
              menuItem("Dashboard", tabName = "Dashboard", icon = icon("tachometer-alt")),
              menuItem("Draw", tabName = "Draw", icon = icon("pencil-alt")),
              menuItem("Register", tabName = "Register", icon = icon("clipboard-list")),
              menuItem("Local dB Searching",

                 menuSubItem("Local Similarity Query", tabName = "LocalSimQuery", icon = icon("search")),
                 menuSubItem("Local SubstructureQuery", tabName = "LocalSubQuery", icon = icon("search"))
              ),

              menuItem("PubChem dB Searching",
                  menuSubItem("PubChem Similarity Query", tabName = "PubChemSimQuery", icon = icon("search")),
                  menuSubItem("PubChem Substructure Query", tabName = "PubChemSubQuery", icon = icon("search"))
                  ),


              box(
                title = "Clipboard",
                width=12,
                id = "mybox",
                collapsible = TRUE,
                closable = FALSE,
                background = "black",
                "Smiles",
                verbatimTextOutput("clip_smiles"),
                "ID",
                verbatimTextOutput("clip_id")
              )



              )
            ),

            dashboardBody(
            #  setBackgroundImage(
            #    src = "https://www.fillmurray.com/1920/1080",
            #    shinydashboard = TRUE
            #  ),

              tabItems(
                tabItem(tabName = "Dashboard",

                       dashModuleUI("dashModule")
                       ),
                tabItem(tabName = "Draw",

                        drawModuleUI("drawModule")
                ),

                tabItem(tabName = "LocalSimQuery",
                        localSimQueryModuleUI("localSimQueryModule")
                ),
                tabItem(tabName = "LocalSubQuery",
                        localSubQueryModuleUI("localSubQueryModule")
                ),


                tabItem(tabName = "PubChemSimQuery",
                        pubchemSimQueryModuleUI("pubchemSimQueryModule")
                ),
                tabItem(tabName = "PubChemSubQuery",
                        pubchemSubQueryModuleUI("pubchemSubQueryModule")
                ),

                tabItem(tabName = "Register",
                        registerModuleUI ("registerModule")
                )

                   )
            )
    )


# Define server logic required to draw a histogram
server <- function(input, output,session) {


#Connect to database

  all_creds <- jsonlite::fromJSON(safer::decrypt_string(Sys.getenv("mongo_db_string")))
  creds <-all_creds[["user"]]
  #



  db_info <- list (

    dbscheme  = 'mongodb+srv://',
    dbinstance =  '@cluster0.41ox5.mongodb.net',
    dbname  = 'chemdb',
    creds = creds
  )

#create element to hold internal clipboard contents

clipboard <- reactiveValues( smiles = "", id = "")

output$clip_smiles <- renderText(clipboard$smiles)
output$clip_id <- renderText(clipboard$id)

#Load modules

dashModuleServer("dashModule")

pubchemSimQueryModuleServer("pubchemSimQueryModule",clipboard,db_info)
pubchemSubQueryModuleServer("pubchemSubQueryModule",clipboard,db_info)


localSimQueryModuleServer("localSimQueryModule",clipboard,db_info)

localSubQueryModuleServer("localSubQueryModule",clipboard,db_info)

drawModuleServer("drawModule",clipboard)

registerModuleServer("registerModule",clipboard, db_info)

}





# Run the application
shinyApp(ui = ui, server = server)
