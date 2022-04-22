library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(statnet)
library(igraph)
library(network)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(magrittr)
library(mwss)
library(SimInf)
library(data.table)
library(shinyWidgets)
library(shinyhelper)
library(shinyTime)
library(shinyalert)
library(knitr)

# Parameters dataset
source('data/O1.R', local = TRUE)
source('data/O2.R', local = TRUE)

#### Source function
source('functions/buttonsUI.R', local = TRUE)

source('functions/wardEditModule.R', local = TRUE)
source('functions/wardRemoveModule.R', local = TRUE)
source('functions/wardAdd.R', local = TRUE)
source('functions/contactEdit.R', local = TRUE)
source('functions/updateParamsModule.R', local = TRUE)
source('functions/downloadParamsModule.R', local = TRUE)
source('functions/setIMMstateModule.R', local = TRUE)
source('functions/diseasetimelineModule.R', local = TRUE)
source('functions/valueboxoutputModule.R', local = TRUE)
source('functions/plotsoutputModule.R', local = TRUE)
source('functions/resetreactivesModule.R', local = TRUE)
source('functions/synthreportModule.R', local = TRUE)


# App Structure function

source('body/presentation.R', local = TRUE)
source('body/structure/tabItemStructure.R', local = TRUE)
source('body/parameters/tabItemParams.R', local = TRUE)
source('body/simulations/tabItemSim.R', local = TRUE)
source('body/about.R', local = TRUE)

source('header/header_ui.R', local = TRUE)
source('body/sidebar_ui.R', local = TRUE)
source('body/body_ui.R', local = TRUE)

