library(shiny)
library(shinyjs)
library(DBI)
library(leaflet)
library(ggplot2)
library(sf)
library(dplyr)
source("global.R")  # Load packages and global settings
source("R/helper_functions_v3.R")
load("tests/dev_data/hdbscan_sample_output.rda")

# Simulate your data layers (replace with actual data in production)
sf_layers_list <- list(
  "Layer 1" = st_as_sf(data.frame(x = c(1, 2), y = c(2, 3)), coords = c("x", "y"), crs = 4326),
  "Layer 2" = st_as_sf(data.frame(x = c(2, 3), y = c(3, 4)), coords = c("x", "y"), crs = 4326)
)

sf_layers_list <- processed_results

selected_layers <- names(sf_layers_list)[1:2]
boundary <- st_as_sf(data.frame(x = c(1, 2), y = c(1, 2)), coords = c("x", "y"), crs = 4326)

# Define the clustering UI module
clusterUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("calculate"), "Calculate Clusters", class = "btn-primary"),
    verbatimTextOutput(ns("debug_output"))
  )
}

# Define server-side logic for clustering (HDBSCAN)
clusterServer <- function(id, selected_layers, sf_layers_list, boundary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define a reactive value for storing cluster results
    cluster_results_reactive <- reactiveVal(NULL)
    
    # Fake clustering process (just returning the selected layers for debugging)
    observeEvent(input$calculate, {
      shinyjs::disable("calculate")
      shinyjs::html("calculate", "Crunching numbers...")
      # Simulating processing (replace this with your actual clustering logic)
      cluster_results <- list(
        layer_1 = selected_layers[1],
        layer_2 = selected_layers[2]
      )
      # Output for debugging (see if clustering works)
      output$debug_output <- renderText({
        paste("Clustering Results:", paste(cluster_results, collapse = ", "))
      })
      shinyjs::enable("calculate")
      shinyjs::html("calculate", "Calculate Clusters")
      # Store the cluster results in the reactive value
      cluster_results_reactive(cluster_results)
    })
    # Return the reactive value
    return(cluster_results_reactive)
  })
}

# Define the map visualization UI
clusterVizUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("clustermap"))
}

# Define the server-side logic for cluster visualization
clusterVizServer <- function(id, cluster_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Create dummy data representing a polygon in NYC (e.g., Manhattan)
    # Coordinates for a small polygon (Manhattan)
    nyc_polygon_coords <- matrix(c(
      -74.011, 40.706,  # Bottom-left (Southwest)
      -73.955, 40.706,  # Bottom-right (Southeast)
      -73.955, 40.783,  # Top-right (Northeast)
      -74.011, 40.783,  # Top-left (Northwest)
      -74.011, 40.706   # Closing the polygon
    ), ncol = 2, byrow = TRUE)
    # Create an sf object with the polygon
    nyc_polygon <- st_polygon(list(nyc_polygon_coords))
    dummy_data <- st_sf(id = 1, geometry = st_sfc(nyc_polygon, crs = 4326))  # EPSG:4326 for geographic coordinates
    
    # Render cluster map
    output$clustermap <- renderLeaflet({
      req(cluster_results())  # Ensure that cluster_results is available
      # Check if cluster_results() is valid; if not, use dummy data
      processed_results <- cluster_results()
      # Check if the processed results are valid sf object, if not use dummy data
      if (is.null(processed_results) || length(processed_results) == 0 || !inherits(processed_results[[1]], "sf")) {
        processed_results <- list(dummy_data)  # Use dummy data if results are invalid
        print("Using dummy data for visualization")
      }
      # Ensure processed_results is an sf object
      clustered_sf <- processed_results[[1]]
      # Check if the data is an sf object with geometry
      if (!inherits(clustered_sf, "sf")) {
        stop("The processed results are not an sf object!")
      }
      # Use the cluster values for coloring the polygons
      cluster_pal <- colorFactor(palette = "viridis", domain = clustered_sf$id)
      leaflet(clustered_sf) %>%
        addProviderTiles("CartoDB") %>%
        addPolygons(data = clustered_sf, 
                    fillColor = ~cluster_pal(id),
                    stroke = FALSE,
                    fillOpacity = 0.5, 
                    group = "Clusters",
                    highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
                    label = ~id) %>% 
        addLegend(pal = cluster_pal, values = ~id, title = "Clusters")
    })
  })
}

# Create the test app function
dataVizTestApp <- function() {
  ui <- fluidPage(
    useShinyjs(),  # Ensure shinyjs is enabled
    titlePanel("Run Cluster Test"),
    clusterUI("runcluster"),
    clusterVizUI("runviz")
  )
  server <- function(input, output, session) {
    # Call the clusterServer module and store results in a reactive value
    cluster_results_reactive <- clusterServer(
      id = "runcluster", 
      selected_layers = selected_layers,
      sf_layers_list = sf_layers_list,
      boundary = boundary
    )
    # Ensure cluster_results_reactive is passed into clusterVizServer
    clusterVizServer(
      id = "runviz",
      cluster_results = cluster_results_reactive  # Pass the reactive cluster results
    )
    # Render the cluster results (to view the structure of the output)
    output$tab <- renderText({
      req(cluster_results_reactive())  # Access the reactive value
      print(str(cluster_results_reactive()))  # Debug the structure of cluster results
    })
  }
  shinyApp(ui, server)
}

# Run the test app
dataVizTestApp()