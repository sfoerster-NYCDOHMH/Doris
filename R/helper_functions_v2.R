### Helper functions for Doris
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author: sfoerster@health.nyc.gov
# Last updated: 2024-10-24

# Description: 
# These are functions that are meant to streamline processing of multiple spatial layers for display and analysis in DORIS
# They facilitate multivariate cluster and hotspot detection and cluster descriptive analysis


# Helper function to check and load packages ####
load_required_packages <- function(packages) {
  lapply(packages, function(pkg) {
    tryCatch({
      library(pkg, character.only = TRUE)
    }, error = function(e) {
      stop(paste("Package not found:", pkg, ". Please install it first."))
    })
  })
}


# Create quartiles for selected columns in a dataframe
add_quartiles <- function(x, measure = "percent_all"){
  column_name <- measure
  x[["quartile"]] <- cut(x[[column_name]], 
                         breaks = jitter(quantile(x[[measure]], probs = 0:4/4, na.rm = TRUE), amount = 1e-6), 
                         labels = 1:4, 
                         include.lowest = TRUE)
  x[["quartile"]] <- as.numeric(as.character(x[["quartile"]]))
  return(x[c(column_name, "quartile")])
}



# Function to load datasets from doris database at specified spatial resolutions
# Works only for datasets with a geography column
get_data <- function(table = NULL, geotype_field = GeoType, resolution = NTA){
  
  ### Database connection ####
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 18 for SQL Server",
    Server = "SQLIT04C.health.dohmh.nycnet",
    database = "DIS_DORIS",
    Trusted_Connection = "yes",
    Port = 1433
  )
  
  data <- dbGetQuery(conn, paste0("select * from doris.", 
                                  table, 
                                  " where ",
                                  geotype_field, " = '", 
                                  resolution,
                                  "'"))
  dbDisconnect(conn)
  return(data)
}




retrieve_tables <- function(table_names, resolution_map) {
  # Initialize an empty list to store retrieved data
  layers_list <- list()
  
  # Establish database connection
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 18 for SQL Server",
    Server = "SQLIT04C.health.dohmh.nycnet",
    database = "DIS_DORIS",
    Trusted_Connection = "yes",
    Port = 1433
  )
  
  for (table_name in table_names) {
    resolution <- resolution_map[[table_name]]
    
    # Query the table
    data <- dbGetQuery(conn, paste0("SELECT * FROM doris.", paste0("acs__", table_name, "_summary"))) %>% 
      rename_with(tolower) %>% 
      # dplyr::filter(year == 2022) %>% 
      {
        # Add "Age" to beginning of all age group levels
        if ("var_age_group" %in% names(.)) {
          mutate(., var_age_group = paste0("Age ", var_age_group))
        } else . 
      } %>% 
      {
        # If there is an aggregated label, roll up counts
        if (sum(str_count(names(.), "aggregated_")) > 0) { 
          aggregated_var <- names(.)[str_detect(names(.), "aggregated_")][1]
          
          group_by(., 
                   !!sym(aggregated_var), 
                   across(starts_with("var_")), 
                   concept, 
                   geoid, 
                   geospatial_resolution) %>% 
            summarize(percent_all = sum(percent_all, na.rm = TRUE)) %>% 
            ungroup()
        } else .
      }
    
    # Combined aggregate and other var_ variables to final group
    levels_combined <- data %>% 
      {
        if("var_age_group" %in% names(.)) {
          dplyr::select(., -var_age_group)
        } else . 
      } %>% 
      dplyr::select(., starts_with(c("var_", "aggregated_"))) %>% 
      tidyr::unite("group", sep = ", ") %>% 
      pull(group)
    data <- data %>% mutate(group = levels_combined)
    
    # Identify grouping variables (non-numeric columns excluding 'geoid')
    all_columns <- tolower(names(data))
    grouping_vars <- "group"
    numeric_vars <- all_columns[grepl("^percent_", all_columns)]
    
    # Ensure 'geoid' is a character for joining
    data$geoid <- as.character(data$geoid)
    
    # Join with the appropriate spatial boundary layer
    if (resolution == "census_tract") {
      data <- left_join(ct, data, by = c("CT_2010" = "geoid"))
    } else if (resolution == "zcta") {
      data <- left_join(zcta, data, by = c("ZCTA_2010" = "geoid"))
    } else if (resolution == "nta") {
      data <- left_join(nta, data, by = c("NTA_2010" = "geoid"))
    }
    
    # Store in layers list with metadata
    layers_list[[table_name]] <- list(
      layer = data,
      grouping_vars = grouping_vars,
      numeric_vars = numeric_vars,
      resolution = resolution
    )
  }
  
  # Close database connection
  dbDisconnect(conn)
  
  return(layers_list)
}



retrieve_tables2 <- function(table_names, resolution_map) {
  # Initialize an empty list to store retrieved data
  layers_list <- list()
  
  # Establish database connection
  conn <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 18 for SQL Server",
    Server = "SQLIT04C.health.dohmh.nycnet",
    database = "DIS_DORIS",
    Trusted_Connection = "yes",
    Port = 1433
  )
  
  for (table_name in table_names) {
    resolution <- resolution_map[[table_name]]
    
    # Query the table
    data <- dbGetQuery(conn, paste0("SELECT * FROM doris.", paste0("acs__", table_name, "_summary"))) %>% 
      rename_with(tolower) %>% 
      dplyr::filter(year == 2023) %>%
      filter(denominator_geoid > 0 & !is.na(denominator_geoid)) %>% 
      group_by(group, 
               concept, 
               geoid, 
               denominator_geoid,
               denominator_all,
               geospatial_resolution) %>% 
      summarize(percent_all = sum(percent_all, na.rm = TRUE)) %>% 
      # filter(!is.na(year)) %>% 
      ungroup()
    
    # Identify grouping variables (non-numeric columns excluding 'geoid')
    all_columns <- tolower(names(data))
    grouping_vars <- "group"
    numeric_vars <- all_columns[grepl("^percent_", all_columns)]
    
    # Ensure 'geoid' is a character for joining
    data$geoid <- as.character(data$geoid)
    
    # Join with the appropriate spatial boundary layer
    if (resolution == "census_tract") {
      data <- left_join(ct, data, by = c("CT_2010" = "geoid"))
    } else if (resolution == "zcta") {
      data <- left_join(zcta, data, by = c("ZCTA_2010" = "geoid"))
    } else if (resolution == "nta") {
      data <- left_join(nta, data, by = c("NTA_2010" = "geoid"))
    }
    
    # Store in layers list with metadata
    layers_list[[table_name]] <- list(
      layer = data,
      grouping_vars = grouping_vars,
      numeric_vars = numeric_vars,
      resolution = resolution
    )
  }
  
  # Close database connection
  dbDisconnect(conn)
  
  return(layers_list)
}

# Example usage
# table_resolution_map <- list(
#   "age_by_gender" = "zcta",
#   "age_group" = "zcta",
#   "age_group" = "census_tract",
#   "age_by_gender" = "zcta",
#   "age_by_gender" = "census_tract",
#   "age_by_gender_poverty" = "zcta",
#   "age_poverty" = "zcta",
#   "age_poverty" = "census_tract"
# )
# all_layers <- retrieve_tables(names(table_resolution_map), table_resolution_map)


.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}


# ## Function to generate a choices list for virtualSelectInput
# ## Deprecated!!
# generate_virtual_select_list <- function(all_data_layers) {
#   dataset_tree <- list()
#   
#   for (table_name in names(all_data_layers)) {
#     layer_info <- all_data_layers[[table_name]]
#     
#     if (!is.null(layer_info)) {
#       grouping_vars <- layer_info$grouping_vars
#       primary_levels <- NULL
#       
#       if (length(grouping_vars) == 1) {
#         primary_levels <- unique(na.omit(layer_info$layer[[grouping_vars[1]]]))
#       } else if (length(grouping_vars) == 2) {
#         first_var <- unique(na.omit(as.character(layer_info$layer[[grouping_vars[1]]])))
#         second_var <- unique(na.omit(as.character(layer_info$layer[[grouping_vars[2]]])))
#         
#         if (length(first_var) > 0 && length(second_var) > 0) {
#           primary_levels <- unique(
#             apply(expand.grid(first_var, second_var, stringsAsFactors = FALSE), 1, paste, collapse = ", ")
#           )
#         }
#       }
#       
#       if (!is.null(primary_levels)) {
#         dataset_tree[[table_name]] <- primary_levels
#       }
#     }
#   }
#   return(dataset_tree)
# }




# Custom function to load shapefiles from opt/shapeFiles which are mostly in Lambert Comformal Conic or others
# Convert to WGS84 which is the standard used by leaflet for visualization purposes and will be used by other data
get_shapefile <- function(x) {
  t <- st_read(x)
  t2 <- st_transform(t, crs = 4326)
  return(t2)
}


# Get a standard set of admin boundary files into the global environment and transforms them into sf layers
# Also gets crosswalk
# Update v0.5 - layers are now prepared separately and loaded in from .rda file

load_boundary_layers <- function(){
  
  # List of required packages
  packages <- c("RSocrata", "sf")
  load_required_packages(packages)
  
  # Census tracts
  ct <<- get_shapefile("/opt/shapeFiles/CT_2010/CT_2010.shp") %>% rmapshaper::ms_simplify(keep = 0.05)
  # ZCTA
  zcta <<- get_shapefile("/opt/shapeFiles/ZCTA_2010/ZCTA_2010.shp") %>% rmapshaper::ms_simplify(keep = 0.05)
  # boro
  boro <<- get_shapefile("/opt/shapeFiles/nybb/nybb.shp")
  # uhf
  uhf <<- get_shapefile("/opt/shapeFiles/UHF42/UHF42.shp")
  # NTAs
  nta <<- get_shapefile("/opt/shapeFiles/NTA_2010_gen/NTA_2010_gen.shp")
  # nta2020 <<- get_shapefile("data/nta2020/nta2020.shp") %>%
  nta2020 <<- get_shapefile("/run/user/1701701623/gvfs/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/Steffen/DORIS/data/nta2020/nta2020.shp") %>%
    mutate(GeoID = paste0(str_extract(countyfips, "[1-9]+"), str_extract(nta2020, "[0-9]+")))
  
  # To do: need to get this from Socrata through API as NTA2020 not in Gretchen's repo
  
  # crosswalk
  crosswalk <<- read_csv("/opt/shapeFiles/lookup_tables/nyc2020census_tract_nta_cdta_relationships.csv")
}



### Function to calculate local Moran clusters ####
get_local_clustering <- function(x, value = NULL){
  
  # Remove missing values in crowding layer
  nona <- x %>% filter(!is.na(.[[value]]))
  # Create spatial weights
  nb <- poly2nb(st_geometry(nona))
  # Create spatial weights list
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Moran's I for crowding
  moran_test <- moran.test(nona[[value]], lw)
  
  # Local Moran’s I
  local_moran <- localmoran(nona[[value]], lw)
  
  # Add results to polygon layer x
  nona[["local_moran"]] <- local_moran[, 1] # I-value
  # Flag significant values
  nona[["significant"]] <- local_moran[, 5] < 0.05
  
  # Cluster type
  nona[["cluster_type"]] <- ifelse(nona[["local_moran"]] > 0 & nona[["significant"]], "Cluster", 
                                   ifelse(nona[["local_moran"]] < 0 & nona[["significant"]], "Outlier", 
                                          "Not Significant"))
  
  return(list(moran_test = moran_test, 
              data = nona))
  
}



### Function to make ggplot map of clusters ####
make_ggplot_map <- function(data, column){
  
  if(is.numeric(data[[column]])){ # for continues variables
    
    ggplot(data) +
      geom_sf(aes(fill = !!sym(column))) +
      scale_fill_viridis_b()
    
  } else { # for categorical variables
    
    ggplot(data) +
      geom_sf(aes(fill = !!sym(column))) +
      # scale_fill_viridis_d(option = "H") + 
      scale_fill_brewer(palette = "Set2")
    # scale_fill_manual(values = c("Cluster" = "red", "Outlier" = "blue", "Not Significant" = "grey"))
    
  }
}


### Test function
# make_ggplot_map(test1$data, column = "local_moran")
# make_ggplot_map(test1$data, column = "cluster_type")




### Function to add polygon layer with cluster results to existing leaflet map ####

add_polygon_layer <- function(map, data, value_col, palette_func, label_col, group) {
  map %>%
    addPolygons(
      fillColor = ~ palette_func(data[[value_col]]),
      stroke = FALSE,
      fillOpacity = 0.5,
      label = ~ paste(data[[label_col]]),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        direction = "auto"
      ),
      group = group
    )
}


### Test function

# # Prepare a color palette (example)
# mypalette <- colorNumeric(palette = "viridis", domain = test1$data$Percent_crowding)
# 
# # Example Leaflet map
# leaflet(data = test1$data, options = leafletOptions(preferCanvas = TRUE)) %>% 
#   addProviderTiles("CartoDB") %>% 
#   add_polygon_layer(data = test1$data, 
#                     value_col = "Percent_crowding", 
#                     palette_func = mypalette, 
#                     label_col = "Percent_crowding", 
#                     group = "Crowding")





# Function to process sf layers, rasterize, stack, and perform DBSCAN ####
do_dbscan <- function(sf_layers, field_list, eps = NULL, minPts = NULL, extent = NULL) {
  
  # List of required packages
  packages <- c("raster", "dplyr", "dbscan", "cluster", "spdep")
  
  # Load required packages
  load_required_packages(packages)
  
  # Get the extent of the 'ct' polygon layer and calculate cell size for square cells
  ct_extent <- st_bbox(extent)
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
    res = cellsize # Ensure square cells
  )
  
  
  # 1. Rasterize all input layers (field_list must match the sf_layers list length)
  raster_list <- lapply(1:length(sf_layers), function(i) {
    
    # Filter out NA and NaN values for the specific field in each sf layer
    sf_layer_clean <- sf_layers[[i]] %>%
      dplyr::filter(!is.na(.data[[field_list[[i]]]]) & !is.nan(.data[[field_list[[i]]]]))
    
    # Check geometry type (Polygon or Point) and apply appropriate rasterization function
    if (any(st_geometry_type(sf_layer_clean) %in% c("POINT", "MULTIPOINT"))) {
      # If point layer, count occurrences per raster cell
      rasterize(sf_layer_clean, raster_template, field = field_list[[i]], fun = "count")
    } else {
      # If polygon layer, compute the mean value per raster cell
      rasterize(sf_layer_clean, raster_template, field = field_list[[i]], fun = mean)
    }
    
  })
  
  # names(raster_list) <- selected_layers
  
  # 2. Standardize each raster layer before stacking
  raster_list <- lapply(raster_list, function(r) {
    # Scale the raster values (Z-score normalization)
    raster_values <- values(r)
    scaled_values <- scale(raster_values)
    values(r) <- scaled_values
    return(r)
  })
  
  names(raster_list) <- unlist(field_list)
  
  # 2. Stack all the rasters
  raster_stack <- stack(raster_list)
  
  # 3. Convert raster stack to a data frame (including coordinates x and y)
  raster_data <- as.data.frame(raster_stack, xy = TRUE)
  
  # 4. Remove NA values
  raster_data <- na.omit(raster_data)
  
  # 5. Extract the pixel values for clustering (omit x and y coordinates)
  values_matrix <- as.matrix(raster_data[, -(1:2)])
  
  # Helper function to calculate silhouette score for DBSCAN
  get_silhouette_score <- function(eps, minPts) {
    
    db_result <- dbscan(values_matrix, eps = eps, minPts = minPts)
    cluster_labels <- db_result$cluster
    
    # Calculate silhouette score only if there are clusters (not noise)
    if (length(unique(cluster_labels)) > 1 && sum(cluster_labels) > 0) {
      silhouette_score <- silhouette(cluster_labels, dist(values_matrix))
      avg_sil_score <- mean(silhouette_score[, 3])
      return(avg_sil_score)
    } else {
      return(NA)
    }
  }
  
  # 6. Determine optimal eps using k-nearest neighbors distance plot
  kNNdistances <- kNNdist(values_matrix, k = 4)
  optimal_eps <- kNNdistances[which.max(diff(kNNdistances))]
  
  # 7. Grid search over minPts and eps
  eps_values <- seq(optimal_eps * 0.8, optimal_eps * 1.2, length.out = 10)
  minPts_values <- seq(4, 10, by = 1)
  
  results <- expand.grid(eps = eps_values, minPts = minPts_values)
  
  # 8. Calculate silhouette score for each combination
  results$silhouette_score <- mapply(get_silhouette_score, results$eps, results$minPts)
  
  # 9. Find the best combination of eps and minPts based on the highest silhouette score
  best_params <- results[which.max(results$silhouette_score), ]
  
  # override eps and minPts if needed
  if(!is.null(eps)){
    best_params$eps <- eps
  }
  
  if(!is.null(minPts)){
    best_params$minPts <- minPts
  }
  
  # 10. Run DBSCAN with optimal parameters
  # db_result_optimized <- dbscan(values_matrix, eps = best_params$eps, minPts = best_params$minPts)
  db_result_optimized <- dbscan(values_matrix, eps = 0.008, minPts = length(unlist(field_list))+1)
  
  
  # 11. Add the cluster results back to the raster data
  raster_data$cluster_optimized <- db_result_optimized$cluster
  
  
  # Turn into fishnet
  raster_extent <- extent(raster_template)
  fishnet <- st_make_grid(
    x = st_as_sfc(st_bbox(raster_extent)), 
    cellsize = cellsize,    # Square grid
    what = "polygons"
  ) %>% st_as_sf(crs = st_crs(extent))  # Ensure CRS matches 'ct'
  
  fishnet$id <- 1:nrow(fishnet)
  
  # Assign the raster values to the grid (fishnet)
  df_sf <- st_as_sf(raster_data, coords = c("x", "y"), crs = st_crs(fishnet))
  
  # Perform a spatial join to assign the point values to the fishnet polygons
  fishnet_with_values <- st_join(fishnet, df_sf, join = st_intersects)
  
  # 12. Return the updated raster data with cluster results
  return(list(raster_data, fishnet_with_values))
}



#### Test function
# sf_layers <- list(ct_crowding_sf, hc_facilities_sf)
# fields <- c("Percent_crowding", "FACNAME")
# 
# # Run the function
# dbscan_output <- do_dbscan(sf_layers, fields)





### Function to run hierarchical cluster detection with hdbscan ####
do_hdbscan <- function(sf_layers, field_list, extent = NULL, minPts = 10) {
  
  # List of required packages
  packages <- c("raster", "dplyr", "dbscan", "cluster", "spdep", "sf")
  
  # Load required packages
  load_required_packages(packages)
  
  # Get the extent of the 'ct' polygon layer and calculate cell size for square cells
  ct_extent <- st_bbox(extent)
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






### Function to add all selected layers to leaflet map ####

# Function to add polygon layers from a list to the Leaflet map without legends
addLayersToMap <- function(map, layers_list, var_list, layer_names) {
  for (i in seq_along(layers_list)) {
    layer_data <- layers_list[[i]]
    variable <- var_list[i]
    layer_name <- layer_names[i]
    
    # Generate color palette for the current variable
    palette <- colorQuantile(palette = "viridis", domain = layer_data[[variable]], na.color = "transparent")
    
    # Prepare label text with HTML formatting for the current layer
    label_text <- paste(
      "Census Tract: ", layer_data$CT_2010, "<br/>",
      paste(variable, ": ", layer_data[[variable]], "<br/>", sep = ""),
      sep = ""
    ) %>% lapply(htmltools::HTML)
    
    # Add the layer as polygons without legend
    map <- map %>%
      addPolygons(
        data = layer_data,
        fillColor = ~palette(get(variable)),
        stroke = FALSE,
        fillOpacity = 0.5,
        label = label_text,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        group = layer_name
      )
  }
  
  return(map)
}




# ## Convert to jsTreeR format
# ## Deprecated!!
# convert_to_jstree <- function(list_data) {
#   result <- list()
#   
#   for (group_name in names(list_data)) {
#     group_values <- list_data[[group_name]]
#     
#     # Create children nodes
#     children <- lapply(group_values, function(value) {
#       list(
#         text = value,  # Display text
#         id = value,    # Unique identifier for selection
#         icon = FALSE   # No icon for leaf nodes
#       )
#     })
#     
#     # Create parent node with children
#     group_node <- list(
#       text = group_name,  # Group display name
#       icon = "jstree-folder",  # Folder icon
#       children = children  # Child nodes
#     )
#     
#     result <- c(result, list(group_node))
#   }
#   
#   return(result)
# }




generate_jstree_data <- function(all_data_layers) {
  # Helper function to capitalize first letter of each word
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  }
  
  result <- list()
  
  for (table_name in names(all_data_layers)) {
    layer_info <- all_data_layers[[table_name]]
    
    if (!is.null(layer_info) && !is.null(layer_info$layer)) {
      # Get unique group values directly from the group column
      if ("group" %in% names(layer_info$layer)) {
        group_values <- unique(na.omit(as.character(layer_info$layer$group)))
        
        if (length(group_values) > 0) {
          # Format the group name (replacing underscores with spaces and capitalizing)
          group_name <- .simpleCap(gsub("_", " ", table_name))
          
          # Create children nodes
          children <- lapply(group_values, function(value) {
            list(
              text = value,  # Display text
              id = value,    # Unique identifier for selection
              icon = FALSE   # No icon for leaf nodes
            )
          })
          
          # Create parent node with children
          group_node <- list(
            text = group_name,  # Group display name
            icon = "jstree-folder",  # Folder icon
            children = children  # Child nodes
          )
          
          result <- c(result, list(group_node))
        }
      }
    }
  }
  
  return(result)
}
