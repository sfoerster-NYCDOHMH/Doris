### Helper functions for DORIS
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Author: sfoerster@health.nyc.gov, 
# Modified by: rgoldberg2@health.nyc.gov
# Last updated: 2025-04-03

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




.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}



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
  ### RG edit 4/15/25 to conver user id to network drives
  nta2020 <<- get_shapefile("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/Steffen/DORIS/data/nta2020/nta2020.shp") %>%
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



### Function get census tables from database
### Used in data_prep_v2.R to produce all_data_layers.rda
### Now excludes zcta with denominator_geoid = 0 (unpopulated areas)
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


