# Testing the data tree module
library(shiny)
library(shinyjs)
library(DBI)
library(leaflet)
library(leafgl)
library(raster)
# library(terra)
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
# library(rmapshaper)
library(shinycssloaders)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("card", "bslib")

setwd("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/ORRU Data Team/DORIS_DASHBOARD")
# Source in functions, modules and data files need for testing, can use production data as well
source("R/modules/clustering/_archive/cluster_detection_moduleRG_2.R")
source("global.R")
load("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/Steffen/DORIS/app_data/layers.rda")
source("R/helper_functions_v3.R")




# ### Simplify boundary layers
# ct <- sf::st_simplify(ct, dTolerance = 0.01)
# nta <- sf::st_simplify(nta, dTolerance = 0.01)
# uhf <- sf::st_simplify(uhf, dTolerance = 0.01)
# boro <- sf::st_simplify(boro, dTolerance = 0.01)
# 
# 
# 

# ct_layers_list <- list(
#   "Households with overcrowding" = list(
#     "layer" = ct, 
#     "column" = "Percent_crowding"
#   ),
#   "Social Vulnerability" = list(
#     "layer" = ct, 
#     "column" = "social_vuln_score_3_plus"
#   ),
#   "Males 0-17" = list(
#     "layer" = ct, 
#     "column" = "male_0_to_17"
#   ),
#   "Females 0-17" = list(
#     "layer" = ct, 
#     "column" = "female_0_to_17"
#   ),
#   "Males 65+" = list(
#     "layer" = ct, 
#     "column" = "male_65_above"
#   ),
#   "Females 65+" = list(
#     "layer" = ct, 
#     "column" = "female_65_above"
#   )
# )
# 
# 
# 
# # Group 2: NTA
# nta_layers_list <- list(
#   "Poverty" = list(
#     "layer" = nta, 
#     "column" = "PercentPoverty"
#   ),
#   "Rent Burden" = list(
#     "layer" = nta, 
#     "column" = "PercentBurden"
#   ),
#   "Unemployment" = list(
#     "layer" = nta, 
#     "column" = "PercentUnemployed"
#   )
# )
# 
# 
# # Group 3: UHF
# uhf_layers_list <- list(
#   "Premature Mortality Rate" = list(
#     "layer" = uhf, 
#     "column" = "PremMortRate"
#   )
# )
# 
# 
# color_palette <- scales::hue_pal()(length(unique(facilities$FACTYPE))) # Generates 21 distinct colors
# # Assign colors dynamically
# factypes <- unique(facilities$FACTYPE)
# group_colors <- setNames(color_palette[seq_along(factypes)], factypes)
# 
# point_layers_list <- facilities %>%
#   split(.$FACTYPE) %>% # Split the data by FACGROUP
#   lapply(function(subset) {
#     group <- unique(subset$FACTYPE)
#     color <- group_colors[group]
#     
#     subset <- subset %>%
#       mutate(color = color) # Add the color column
#     
#     list(
#       layer = subset,
#       column = "FACNAME"
#     )
#   })
# 
# 
# # Rename list elements with FACGROUP values as keys
# names(point_layers_list) <- unique(facilities$FACTYPE)
# 
# sf_layers_list <- list(
#   "CT Layers" = ct_layers_list, 
#   "NTA Layers" = nta_layers_list, 
#   "UHF Layers" = uhf_layers_list, 
#   "Point Layers" = point_layers_list
# )






# Process manually selected layers
#selected_layers <- names(ct_layers_list)[1:2]
selected_layers <- names(level_data)[1:2]

# manually select base extent
boundary <- zcta






# Create test app function
dataVizTestApp <- function() {
  
  ui <- fluidPage(
    titlePanel("Run Cluster Test"),
    clusterUI("runcluster",selected_layers),
    verbatimTextOutput("tab")
  )
  
  server <- function(input, output, session) {
    
    # Call server module to create tree input
    results <<- clusterServer(
      id = "runcluster", 
      selected_layers = selected_layers,
      sf_layers_list = level_data,
      boundary = zcta 
    )
    
    output$tab <- renderText({
      req(results())
      # results()[[1]]
      print(str(results()))
      #unlist(results())
    })
    
    
  }
  
  shinyApp(ui, server)
}


# Run the test app
dataVizTestApp()
