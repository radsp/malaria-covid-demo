## -----------------------------------------------------------------
## 
## app.R
## 
## This is a wrapper script that defines the libraries, calls other
## scripts that contains app components
## 
##
## -----------------------------------------------------------------

options(civis.default_db = "PMI")

## -----------------------------------------------------------------
# Load packages
## -----------------------------------------------------------------
install.packages("ggthemes", dependencies = TRUE)
install.packages("Hmisc", dependencies = TRUE)
install.packages("civis", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("reshape2", dependencies = TRUE)
install.packages("markdown", dependencies = TRUE)
install.packages("rgdal", dependencies = TRUE)
install.packages("leaflet", dependencies = TRUE)
install.packages("gridExtra", dependencies = TRUE)
install.packages("scales", dependencies = TRUE)
install.packages("data.table", dependencies = TRUE)
install.packages("shinyalert", dependencies = TRUE)


library(htmltools)
library(shiny)
library(civis)
library(tidyverse)
library(reshape2)
library(markdown)
library(rgdal)
library(leaflet)
library(devtools)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(ggthemes)
library(gridExtra)
library(plotly)
library(rmarkdown)
library(scales)
library(data.table)
library(Hmisc)
library(shinyalert)


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
