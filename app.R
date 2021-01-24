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
# install.packages("ggthemes")
# install.packages("Hmisc")

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
library(ggthemes)
library(gridExtra)
library(tidyverse)
library(plotly)
library(rmarkdown)
library(scales)
library(htmltools)
library(data.table)
library(Hmisc)



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
