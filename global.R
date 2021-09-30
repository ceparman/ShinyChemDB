



#Shiny related libraries
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(htmlwidgets)
library(shiny.worker)
library(shinyjs)

#tidyverse related libraries
#options(repos = BiocManager::repositories())

#chemistry related libraries
library(BiocManager)


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

