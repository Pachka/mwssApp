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
library(magrittr)
library(data.table)
library(shinyWidgets)
library(shinyhelper)


# Parameters dataset
# source('data/params_dataset.R', local = TRUE)
# source('data/deltaParams.R', local = TRUE)
# source('data/omicronParams.R', local = TRUE)

params_dataset <- readRDS("data/params_dataset.rds")
deltaParams <- readRDS("data/deltaParams.rds")
omicronParams <- readRDS("data/omicronParams.rds")

#### Source function
source('functions/buttonsUI.R', local = TRUE)

source('functions/addWardbutton.R', local = TRUE)
source('functions/editWardbutton.R', local = TRUE)
source('functions/deleteWardbutton.R', local = TRUE)


# App Structure function


source('body/presentation.R', local = TRUE)
source('body/structure/tabItemStructure.R', local = TRUE)
source('body/parameters/tabItemParamDesc.R', local = TRUE)
source('body/parameters/tabItemParamepi.R', local = TRUE)
source('body/parameters/tabItemParamtest.R', local = TRUE)
source('body/parameters/tabItemParamvacc.R', local = TRUE)
source('body/parameters/tabItemParamAll.R', local = TRUE)
source('body/simulations/tabItemSimuDescription.R', local = TRUE)
source('body/simulations/tabItemSimSurvContr.R', local = TRUE)
source('body/simulations/tabItemSimulations.R', local = TRUE)
source('body/about.R', local = TRUE)

source('header/header_ui.R', local = TRUE)
source('body/sidebar_ui.R', local = TRUE)
source('body/body_ui.R', local = TRUE)

