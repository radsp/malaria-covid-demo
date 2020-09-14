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

# Packages that are not in the platform


if(!require(ggthemes)){
  install.packages("ggthemes")
}

if(!require(plotly)){
  install.packages("plotly")
}

if(!require(rmarkdown)){
  install.packages("rmarkdown")
}

library(ggthemes)
library(rmarkdown)

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
