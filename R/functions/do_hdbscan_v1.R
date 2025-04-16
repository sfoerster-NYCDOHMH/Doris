## Function to conduct hierarchical dbscan analysis to detect clusters of similar areas for selected features
## Author: Steffen Foerster, sfoerster@health.nyc.gov
## Last edited: 2025-04-02

## Change Log
## 2025-04-02
##   - modified function to output additional objects, including hdbscan result itself
##   - added method to map cluster IDs to a polygon layer like zcta based on dominant cluster area per polygon
##   - decreased raster size for better resolution without dramatically affecting calculation time


### To do: 
###   - adapt to new flat list
###   - fix issue where output does not change with minPts after first generation

### Function to run hierarchical cluster detection with hdbscan ####
do_hdbscan <- function(sf_layers, field_list, minPts = 10, boundary = zcta) {
  
  # List of required packages
  packages <- c("raster", "dplyr", "dbscan", "cluster", "spdep", "sf")
  
  # Load required packages
  load_required_packages(packages)
  
  # Get the extent of the 'ct' polygon layer and calculate cell size for square cells
  extent <- st_bbox(boundary)
  x_range <- extent["xmax"] - extent["xmin"]
  y_range <- extent["ymax"] - extent["ymin"]
  
  # Define number of columns and rows, then compute a common cell size for square cells
  ncol <- 100
  nrow <- 100
  cellsize <- min(x_range / ncol, y_range / nrow)  # Ensure square cells
  
  # Create a raster with square cells that matches the extent of the 'ct' layer
  raster_template <- raster(
    xmn = extent["xmin"], xmx = extent["xmax"],
    ymn = extent["ymin"], ymx = extent["ymax"],
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
  
  
  ### NEW !!! 2025-04-02
  ### Make a raster with the cluster ID values that covers original area (including NAs)
  # Take one layer as a template for dimensions and NA structure
  template <- raster_stack[[1]]
  
  # Create an empty raster for cluster IDs, same shape and NAs as template
  cluster_raster <- template
  values(cluster_raster) <- NA
  
  # Find non-NA cell positions (same rows used in clustering)
  valid_cells <- which(!is.na(values(template)))
  
  # Assign cluster IDs to the non-NA cells
  values(cluster_raster)[valid_cells] <- hdbscan_result$cluster
  
  # Map this to the boundary layer given to function, to map cluster IDs to polygons
  boundary[["cluster_id"]] <- exact_extract(cluster_raster, boundary,
                                            function(values, coverage_fractions) {
                                              valid <- !is.na(values)
                                              values <- values[valid]
                                              coverage_fractions <- coverage_fractions[valid]
                                              
                                              if (length(values) == 0) return(NA)
                                              tab <- tapply(coverage_fractions, values, sum)
                                              names(which.max(tab))
                                            })
  
  
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
  results <- list(raster_data, 
                  fishnet_with_values, 
                  cluster_raster, 
                  hdbscan_result, 
                  boundary)
  names(results) <- c("raster data", 
                      "fishnet with values", 
                      "cluster raster", 
                      "hdbscan results", 
                      "boundary layer")
  return(results)
}

