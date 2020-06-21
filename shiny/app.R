##
## APP ENTRY SCRIPT
##
##  - called by run.vbs script
##

## BASE LIBRARIES
library(shiny)
library(shinycssloaders)
library(shinyjqui)
library(dplyr)
# library(tidyverse)
# library(TraMineR)
#library(shinydashboard)

## APP MODULES
source('analysisModule.R')  ## loads ui,server helper functions
source('appUi.R') 		## loads `.ui()` function that returns `ui` object
source('appServer.R')   ## loads `.server()` function that returns `server(input, output, session)` function

## RUN APP
shinyApp(ui=.ui(), server=.server(), enableBookmarking = "url")  ## enableBookmarking = "server", "url"