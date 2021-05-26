



#Shiny related libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(htmlwidgets)

#tidyverse related libraries



#chemistry related libraries
library(BiocManager)
#options(repos = BiocManager::repositories())

library(ChemmineOB)

library(jsme)
library(ChemmineR)

library(fmcsR)

molRefs = ChemmineOB::forEachMol("SMILES","C1CCCCC1\ttest-compound-name",identity)

print(ChemmineOB::fingerprint_OB(molRefs,"FP3"))

