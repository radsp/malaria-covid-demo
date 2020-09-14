## -----------------------------------------------------------------
## 
## app.R
## 
## This is a wrapper script that defines the libraries, calls other
## scripts that contains app components
## 
##
## -----------------------------------------------------------------


## -----------------------------------------------------------------
# Load packages
## -----------------------------------------------------------------

library(shiny)
library(civis)
library(ggplot2)
library(dplyr)
library(reshape2)
library(markdown)
library(rgdal)
library(leaflet)
library(devtools)
library(shinyjs)
library(shinyWidgets)
library(tidyr)
library(shinydashboard)
library(civis)
library(ggplot2)
library(dplyr)


# Packages that (may) not in the platform

pckg_list <- c("ggthemes", "tidyverse", "plotly", "rmarkdown",
               "scales", "rmarkdown")

for (i in pckg_list) {
  if (!(require(i, character.only = TRUE))) {
    install.packages(i)
    library(i)
  }
}




## -----------------------------------------------------------------
# Sources
## -----------------------------------------------------------------


source('util.R')
source('dat-gather.R')
source('global.R')
source("styling.R")
source("constant.R")
source("myUI.R")
source("myServer.R")


## -----------------------------------------------------------------
# Call the app
## -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
