
source("./global.R")

options(shiny.port=9001)
options(shiny.host = "192.168.1.70")

# Define UI for application
    ui <- dashboardPage(

            dashboardHeader(),

            dashboardSidebar(
              sidebarMenu(
              menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
              menuItem("Query", tabName = "Query", icon = icon("search")),
              menuItem("Draw", tabName = "Draw", icon = icon("pencil-alt")),
              menuItem("Register", tabName = "Register", icon = icon("clipboard-list"))

              )
            ),

            dashboardBody(
              tabItems(
                tabItem(tabName = "Dashboard",
                       dashModuleUI("dashModule")
                       ),
                tabItem(tabName = "Query",
                        queryModuleUI("queryhModule")
                ),
                tabItem(tabName = "Draw",
                        drawModuleUI("drawModule")
                ),
                tabItem(tabName = "Register",
                           drawModuleUI("registerModule")
                )

                   )
            )
    )


# Define server logic required to draw a histogram
server <- function(input, output,session) {



#Load modules

dashModuleServer("dashModule")

queryModuleServer("searchModule")

drawModuleServer("drawModule")

registerModuleServer("registerModule")

}








# Run the application
shinyApp(ui = ui, server = server)
