# Testing the data tree module
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyTree)
library(odbc)
library(DBI)
library(bslib)
library(leafpop)

setwd("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/ORRU Data Team/DORIS_DASHBOARD")

# Source in functions, modules and data files need for testing, can use production data as well
source("R/helper_functions_v4.R")
source("R/modules/data explorer/data_explorer_module_RG.R")
source("R/modules/data explorer/data_tree_module_dev.R")
load("data/all_data_layers.rda")
load("tests/dev_data/test_selection_jstree.rda")
load("tests/dev_data/base_layers.rda")
load("data/level_data.rda")
load("data/disease_outcomes.Rda")
base64plots <- readRDS("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/Bilal/doris/base64plots2.rds")
  
  
  


ui <- page_sidebar(
  titlePanel("Data Explorer Test"),
  sidebar = sidebar(dataTreeUI("tree"),
                    outcomeUI("outcome")),
  mapVizUI("map_explorer")
)

server <- function(input, output, session) {
  
  # Call server module to create tree input
  selection <- dataTreeServer(
    id = "tree", 
    data_layers = reactive({ all_data_layers }))
  
  # call server module to create outcome tree input
  selected_outcomes <- outcomeServer(
    id = "outcome", 
    disease_list = reactive({ disease_list }) )
  
  
  selection_combined <- reactive({c(selection(),selected_outcomes())})
  
  # Call server module to create interactive leaflet map responding to input
  mapVizServer(
    id = "map_explorer",
    base_layers = base_layers,
    data_layers = c(level_data,level_data_dis),
    selected_layers = selection_combined
  )
}

shinyApp(ui, server)

  

    
    
    