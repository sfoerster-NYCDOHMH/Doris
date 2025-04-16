## Global settings and parameters for DORIS application
## Author: Steffen Foerster sfoerster@health.nyc.gov
## Mantainer: Steffen Foerster sfoerster@health.nyc.gov
## Data updated: 2025-03-12


# Load libraries ####
library(waiter)
library(shiny)
library(shinyjs)
library(shinyTree)
library(DBI)
library(leaflet)
library(leafgl)
library(raster)
library(exactextractr)
library(terra)
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(conflicted)
library(stringr)
library(htmltools)
library(dbscan)
library(spdep)
library(purrr)
library(shinycssloaders)
library(jsTreeR)
library(conductor)
useConductor()
library(reactlog)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("card", "bslib")
reactlog_enable()


# Load data layers ####
load("data/layers.rda")
load("data/all_data_layers.rda")
load("data/facilities.rda")
load("data/level_data.rda")


# Simplify boundary layers ####
ct <- sf::st_simplify(ct, dTolerance = 0.01)
zcta <- sf::st_simplify(zcta, dTolerance = 0.01)
uhf <- sf::st_simplify(uhf, dTolerance = 0.01)
boro <- sf::st_simplify(boro, dTolerance = 0.01)

# Prepare base layers
base_layers <- list(
  "Borough" = boro,
  "ZCTA" = zcta,
  "UHF Neighborhood" = uhf,
  "Census Tract" = ct
)


# Set up sidebar panels
panel_dynamic_filters <- accordion_panel(
  title = "Dynamic Filters",
  value = "panel_dynamic_filters", 
  card(
    card_header(
      "Use the filters below to look for areas low and/or high on selected criteria."
    ),
    card_body(
      selectizeInput(
        "layer_high",
        "Areas high in all of these metrics:",
        choices = NULL,
        multiple = TRUE
      ),
      selectInput(
        "combine",
        "If selecting another set below, how do you want to combine the two sets?",
        choices = c("OR", "AND"),
        selected = "AND"
      ),
      selectizeInput(
        "layer_low",
        "Areas low in all of these metrics:",
        choices = NULL,
        multiple = TRUE
      ),
      actionButton("apply_filters", "Apply Filters"),
      div(id = "spinner-placeholder"),  # Placeholder for the spinner <div class="d-flex justify-content-center">
    )
  ), # end card 1
  card(
    ### Additional menu items hidden in accordion for proximity analysis
    card_header(
      "Below you can identify points of interest around the areas filtered above, based on either distance or % overlap."
    ),
    card_body(
      accordion(open = FALSE,
                accordion_panel(
                  "Proximity Analysis",
                  selectizeInput("facgroup", "Facilities by Group:", choices = unique(facilities$FACGROUP), multiple = TRUE),
                  selectizeInput("facsubgrp", "Facilities by Subgroup:", choices = unique(facilities$FACSUBGRP), multiple = TRUE),
                  selectizeInput("factype", "Facilities by Type:", choices = unique(facilities$FACTYPE), multiple = TRUE),
                  accordion(open = "Distance Threshold",
                            accordion_panel("Distance Threshold",
                                            card(
                                              card_header("This method looks for points of interest in or within a given distance from the edge of the filtered areas above."),
                                              card_body(
                                                sliderInput(
                                                  "distance_threshold", 
                                                  "Distance Threshold (in miles):", 
                                                  min = 0.1, max = 10, value = 1, step = 0.1
                                                ),
                                                actionButton("proximity_button", "Run Proximity-Based Selection", class = "btn-primary btn-block")
                                              )
                                            )
                            ),
                            accordion_panel("Overlap Threshold",
                                            card(
                                              card_header("This method looks for points whose catchment area - defined by the distance threshold - includes a given % of the total filtered area above."),
                                              card_body(
                                                sliderInput(
                                                  "overlap_threshold", 
                                                  "Area Overlap Threshold (in %):", 
                                                  min = 0, max = 100, value = 90
                                                ),
                                                actionButton("weighted_button", "Run Area-Weighted Selection", class = "btn-secondary btn-block")
                                              )
                                            )
                            )
                  )
                ),
      ), # end accordion
    ) # end card body
  ) # end card 2
) # end accordion_panel 2



panel_cluster_detection <- accordion_panel(
  title = "Cluster Detection",
  value = "panel_cluster_detection",
  card(
    card_header(
      "Clicking the button below will initiate a hierarchical dbscan machine learning cluster detection algorithm to detect spatial units with similar qualities across all dimensions currently 
       selected in the 'Data Explorer'."
    ),
    card_body(
      accordion(
        id = "parameter_section", 
        open = FALSE,
        accordion_panel(
          title = "Adjust Parameters",
          value = "panel_adjust_parameters",
          card(
            card_header("The hierarchical dbscan algorithm implemented here is influenced by the minimum number of points required in a cluster, which you can change below. 
                        Any value selected here is scaled (multiplied) by the number of layers selected in the analysis plus 1 to account for increasing dimensionality of the 
                         data."),
            card_body(
              sliderInput("minpts", "Minimum Number of Points", min = 1, max = 20, value = 4, step = 1)
            )
          )
        )
      )
    ),
    card_footer(
      actionButton("calculate", "Calculate Clusters", class = "btn-primary btn-block")
      # div(id = "spinner2-placeholder"),  # Placeholder for the spinner <div class="d-flex justify-content-center">
    )
  )
)



# CSS Styling ####
doris_css <- tags$head(
  tags$style(HTML("
      .bslib-sidebar-layout .sidebar-title {
        margin-bottom: 0px !important;
      }
      
      .card bslib-card bslib-mb-spacing html-fill-item html-fill-container {
        margin: 10px;
      }
      
      /* Minimalist styling for jsTree */
      .jstree-default .jstree-anchor {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        font-size: 0.9rem;
        color: #212529;
      }
      
      /* Folder styling */
      .jstree-default .jstree-icon.jstree-folder {
        color: #6c757d;
      }
      
      /* Selected item styling */
      .jstree-default .jstree-clicked {
        background: #f8f9fa;
        border-radius: 3px;
        box-shadow: none;
      }
      
      /* Hover styling */
      .jstree-default .jstree-hovered {
        background: #e9ecef;
        border-radius: 3px;
        box-shadow: none;
      }
      
      /* Checkbox styling */
      .jstree-default .jstree-checkbox {
        background-position: -260px -4px;
      }
      
      /* Search box styling */
      #tree_search {
        border: 1px solid #ced4da;
        border-radius: 3px;
        padding: 6px 12px;
        margin-bottom: 10px;
        width: 100%;
        font-size: 0.9rem;
      }
      
      /* Wholerow selection styling */
      .jstree-default .jstree-wholerow-clicked {
        background: #f8f9fa;
      }
      
      /* Hide node icons for a cleaner look */
      .jstree-default .jstree-icon:not(.jstree-checkbox):not(.jstree-ocl) {
        display: none;
      }
      
      /* Clean up the spacing */
      .jstree-default .jstree-node {
        margin-left: 12px;
      }
    "))
)


