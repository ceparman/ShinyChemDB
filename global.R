



#Shiny related libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(htmlwidgets)

#tidyverse related libraries
#options(repos = BiocManager::repositories())

#chemistry related libraries
library(BiocManager)


library(ChemmineOB)

library(jsme)
library(ChemmineR)

library(fmcsR)

library(tools)



useSMI <- function(session, element, jmeFile) {
  session$sendCustomMessage(type = 'useSMI',
                            message = list(el=element, jmeFile = jmeFile))
}

