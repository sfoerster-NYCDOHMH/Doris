# Module for running hierarchical dbscan cluster detection algorithm
clusterUI <- function(id,selected_layers) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      # title = "Data Explorer",
      width = "25%",
      card(
        card_body(
          "Clicking the button below will initiate a hierarchical dbscan machine learning cluster detection algorithm to detect spatial units with similar qualities across all dimensions currently selected in the 'Data Explorer'."
        ),
        card_body(
          accordion(
            open = TRUE,
            accordion_panel(
              "Adjust Parameters",
              card(
                card_header("The hierarchical dbscan algorithm implemented here is influenced by the minimum number of points required in a cluster, which you can change below..."),
                card_body(
                  sliderInput(ns("minpts"), "Minimum Number of Points", min = 1, max = 20, value = length(selected_layers) + 1, step = 1)
                )
              )
            )
          )
        ),
        card_footer(
          actionButton(ns("calculate"), "Calculate Clusters", class = "btn-primary btn-block")
        )
      )
    )
  )
}

clusterServer <- function(id, selected_layers, sf_layers_list, base_extent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    #hdbscan function
    do_hdbscan <- function(sf_layers, field_list, extent = NULL, minPts = 10) {
      
      # List of required packages
      packages <- c("raster", "dplyr", "dbscan", "cluster", "spdep", "sf")
      
      # Load required packages
      load_required_packages(packages)
      
      # Get the extent of the 'ct' polygon layer and calculate cell size for square cells
      ct_extent <- st_bbox(base_extent)
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
        sf_layer_clean <- sf_layers[[i]] %>%
          dplyr::filter(!is.na(.data[[field_list[[i]]]]) & !is.nan(.data[[field_list[[i]]]]))
        
        if (any(st_geometry_type(sf_layer_clean) %in% c("POINT", "MULTIPOINT"))) {
          rasterize(sf_layer_clean, raster_template, field = field_list[[i]], fun = "count")
        } else {
          rasterize(sf_layer_clean, raster_template, field = field_list[[i]], fun = mean)
        }
      })
    #percent_all = field_list
      
      
      # 2. Standardize each raster layer before stacking
      raster_list <- lapply(raster_list, function(r) {
        raster_values <- values(r)
        scaled_values <- scale(raster_values)
        values(r) <- scaled_values
        return(r)
      })
      
      names(raster_list) <- unlist(field_list)
      
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
      
      # 9. Return the updated raster data with cluster results
      return(list(raster_data, fishnet_with_values))
    }
    
    
    
    
    
    # Store cluster results
    cluster_results <- reactiveVal(NULL)
    
    # Handle calculate button
    observeEvent(input$calculate, {
      shinyjs::disable("calculate")
      shinyjs::html("calculate", "Crunching numbers...")

      selected_sf_layers <- lapply(selected_layers, function(name) {
        # Loop through the list to find the matching sf object and its column
        found <- NULL
        for (group in names(sf_layers_list)) {
          if (name %in% names(sf_layers_list[[group]])) {
            found <- sf_layers_list[[group]][[name]]
            break
          }
        }
        return(found)
      })

      # Make sure to extract both the layer and its corresponding field name
      selected_layer_info <- lapply(selected_layers, function(name) {
        for (group in names(sf_layers_list)) {
          if (name %in% names(sf_layers_list[[group]])) {
            return(sf_layers_list[[group]][[name]])  # returns the entire list with layer and column
          }
        }
        return(NULL)  # return NULL if not found
      })
      
      # Remove any NULLs in case any selected layers were not found
      selected_layer_info <- selected_layer_info[!sapply(selected_layer_info, is.null)]
      

      # Now separate them into two lists
      layers_list <- lapply(selected_layer_info, function(x) x$layer)
      fields_list <- lapply(selected_layer_info, function(x) x$column)
      

      
      # Run clustering
      processed_results <- do_hdbscan(
        sf_layers = layers_list, 
        field_list = fields_list, 
        extent = ct, #should this be ct?
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
