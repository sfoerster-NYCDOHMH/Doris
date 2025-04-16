# Version 2
# Updated: 2025-04-04
# Author: Rebecca Goldberg, rgoldberg2@health.nyc.gov
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Module responsible for running hdbscan on selected layers
#' @description
#' This module runs the hdbscan function on layers selected and returned by data_tree_module. 
#' It outputs a list of cluster results 
#' 
#' @param id 
#' @param selected_layers reactive expression returning a character vector of selected layers from tree input. 
#' @param sf_layers_list List of layers containing sf objects with geometry. 
#'    Naming of each list item is table_name.group, e.g. "age_by_gender.Female, 0-17"
#'    Currently users layers_data.rda which is derived from all_layers_data.rda, split on group column
#' @param boundary Multi-polygon sf object, currently set to zcta boundaries
#'
#' @return List with elements: 
#'  \describe{
#'     \item{output}{List of rasters with cluster ID as well as boundary polygons}
#'   }



# Module for running hierarchical dbscan cluster detection algorithm
clusterUI <- function(id) {
  ns <- NS(id)
  accordion_panel(
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
                tagList(
                sliderInput(ns("minpts"), "Minimum Number of Points", min = 1, max = 100, value = 10)
                )
              )
            )
          )
        )
      ),
      card_footer(
        actionButton(ns("calculate"), "Calculate Clusters", class = "btn-primary btn-block")
        # div(id = "spinner2-placeholder"),  # Placeholder for the spinner <div class="d-flex justify-content-center">
      )
    )
  )
}

clusterServer <- function(id, selected_layers, sf_layers_list, boundary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #hdbscan function
    do_hdbscan <- function(sf_layers, boundary, minPts = 10) {
      
      # List of required packages
      packages <- c("raster", "dplyr", "dbscan", "cluster", "spdep", "sf")
      
      # Load required packages
      load_required_packages(packages)
      
      # Get the extent of the 'ct' polygon layer and calculate cell size for square cells
      ct_extent <- st_bbox(boundary)
      x_range <- ct_extent["xmax"] - ct_extent["xmin"]
      y_range <- ct_extent["ymax"] - ct_extent["ymin"]
      
      # Define number of columns and rows, then compute a common cell size for square cells
      ncol <- 50
      nrow <- 50
      cellsize <- min(x_range / ncol, y_range / nrow)  # Ensure square cells
      
      # Create a raster with square cells that matches the extent of the 'ct' layer
      raster_template <- raster(
        xmn = ct_extent["xmin"], xmx = ct_extent["xmax"],
        ymn = ct_extent["ymin"], ymx = ct_extent["ymax"],
        res = cellsize  # Ensure square cells
      )
      
      # 1. Rasterize all input layers (field_list must match the sf_layers list length)
      raster_list <- lapply(1:length(sf_layers), function(i) {
        if (any(st_geometry_type(sf_layers[[i]]) %in% c("POINT", "MULTIPOINT"))) {
          rasterize(sf_layers[[i]], raster_template, field = "percent_all", fun = "count")
        } else {
          rasterize(sf_layers[[i]], raster_template, field = "percent_all", fun = mean)
        }
      })
      
      
      # 2. Standardize each raster layer before stacking
      raster_list <- lapply(raster_list, function(r) {
        raster_values <- values(r)
        scaled_values <- scale(raster_values)
        values(r) <- scaled_values
        return(r)
      })
      
      names(raster_list) <- selected_layers
      
      # 3. Stack all the rasters
      raster_stack <- stack(raster_list)
      
      # 4. Convert raster stack to a data frame (including coordinates x and y)
      raster_data <- as.data.frame(raster_stack, xy = TRUE)
      
      # 5. Remove NA values
      raster_data <- na.omit(raster_data)
      
      # 6. Extract the pixel values for clustering (omit x and y coordinates)
      values_matrix <- as.matrix(raster_data[, -(1:2)])
      
      # 7. Run HDBSCAN
      hdbscan_result <- hdbscan(values_matrix, minPts = minPts * (length(raster_list)+1))
      
      # 8. Add the cluster results back to the raster data
      raster_data$cluster <- hdbscan_result$cluster
      
      # Turn into fishnet
      raster_extent <- extent(raster_template)
      fishnet <- st_make_grid(
        x = st_as_sfc(st_bbox(raster_extent)), 
        cellsize = cellsize,  # Square grid
        what = "polygons"
      ) %>% st_as_sf(crs = st_crs(extent))  # Ensure CRS matches 'ct'
      
      fishnet$id <- 1:nrow(fishnet)
      
      
      # Assign the raster values to the grid (fishnet)
      df_sf <- st_as_sf(raster_data, coords = c("x", "y"), crs = st_crs(fishnet))
      
      # Perform a spatial join to assign the point values to the fishnet polygons
      fishnet_with_values <- st_join(fishnet, df_sf, join = st_intersects)
      
      # 9. Return cluster results as df as well as the polygons
      output <- list(raster_data, fishnet_with_values)
      
      return(output)
    }
    
    
    
    # Store cluster results
    cluster_results <- reactiveVal(NULL)
    
    # Handle calculate button
    observeEvent(input$calculate, {
      print(input$calculate)
      shinyjs::disable("calculate")
      shinyjs::html("calculate", "Crunching numbers...")
      
      
      print('working')
      
      # get selected layers
      layers_list <- sf_layers_list[names(sf_layers_list) %in% selected_layers]
      
      # Run clustering
      processed_results <- do_hdbscan(
        sf_layers = layers_list, 
        boundary = boundary, 
        minPts = input$minpts
      ) 
      
      # Store results
      cluster_results(processed_results)
      
      # Re-enable button
      shinyjs::enable("calculate")
      shinyjs::html("calculate", "Calculate Clusters")
    })
    
    # Return results
    return(cluster_results)
  })
}
