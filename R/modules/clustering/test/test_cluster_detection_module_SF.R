# Testing the data tree module
library(shiny)
library(shinyjs)
library(DBI)
library(leaflet)
library(leafgl)
library(raster)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(conflicted)
library(stringr)
library(htmltools)
library(dbscan)
library(spdep)
library(purrr)
library(shinycssloaders)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("card", "bslib")

setwd("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/ORRU Data Team/DORIS_DASHBOARD")
# Source in functions, modules and data files need for testing, can use production data as well
source("R/modules/clustering/cluster_detection_module_devSF.R")
source("global.R")
load("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/Steffen/DORIS/app_data/layers.rda")
load("tests/dev_data/level_data.rda")
source("R/helper_functions_v3.R")


# Create test app function
dataVizTestApp <- function() {
  
  ui <- fluidPage(
    titlePanel("Run Cluster Test"),
    clusterUI("runcluster"),
    verbatimTextOutput("tab")
  )
  
  server <- function(input, output, session) {
    
    # Process manually selected layers
    layers <- names(level_data)[1:2]
    print(layers[[1]])
    # manually select base extent
    boundary <- zcta
    
    # Call server module to create tree input
    results <- clusterServer(
      id = "runcluster", 
      selected_layers = layers,  # character vector like c("layer1", "layer2")
      sf_layers_list = level_data,  # full list of sf layers, named by layer
      boundary = zcta
    )

    
    output$tab <- renderPrint({
      req(results())
      results()
    })
    
  }
  
  shinyApp(ui, server)
}


# Run the test app
dataVizTestApp()
