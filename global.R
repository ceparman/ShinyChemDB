

#Shiny related libraries
library(shiny)
library(shinydashboard)
#library(shinydashboardPlus)
library(reactable)
library(shinycssloaders)
library(htmlwidgets)
library(shiny.worker)
library(shinyjs)
library(shinyWidgets)
library(mongolite)
library(mongochem)
library(jsonlite)



#tidyverse related libraries
#options(repos = BiocManager::repositories())

#chemistry related libraries



library(ChemmineOB)
library(webchem)

library(jsme)
library(ChemmineR)

library(fmcsR)

library(dplyr)
library(tools)
library(httr2)

DEBUG = T

#initialize jme editor

useSMI <- function(session, element, jmeFile) {
  session$sendCustomMessage(type = 'useSMI',
                            message = list(el=element, jmeFile = jmeFile))
}

