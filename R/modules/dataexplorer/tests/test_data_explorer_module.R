# Testing the data tree module
library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(shinyTree)

# Source in functions, modules and data files need for testing, can use production data as well
source("R/helper_functions_v2.R")
source("R/modules/data explorer/data_explorer_module.R")
source("R/modules/data explorer/data_tree_module.R")
load("data/all_data_layers.rda")
load("tests/dev_data/test_selection_jstree.rda")
load("tests/dev_data/base_layers.rda")
load("data/level_data.rda")

# Create test app function
dataVizTestApp <- function() {
  
  ui <- fluidPage(
    titlePanel("Data Explorer Test"),
    dataTreeUI("tree"),
    mapVizUI("map_explorer"),
  )
  
  server <- function(input, output, session) {
    
    # Call server module to create tree input
    selection <- dataTreeServer(
      id = "tree", 
      data_layers = reactive({ all_data_layers }))
    
    # Call server module to create interactive leaflet map responding to input
    map_explorer <- mapVizServer(
      id = "map_explorer",
      base_layers = base_layers,
      data_layers = level_data,
      selected_layers = selection
    )
  }
  
  shinyApp(ui, server)
}


# Run the test app
dataVizTestApp()
