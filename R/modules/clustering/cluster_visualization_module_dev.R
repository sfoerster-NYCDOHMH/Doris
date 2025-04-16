# Version 1
# Updated: 2025-04-08
# Author: Christopher Singh, csingh2@health.nyc.gov
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Module responsible for generating cluster map from hdbscan outputs
#' @description
#' This module takes the output of the hdbscan clustering algorithm and generates. 
#' an overlay all the map with the plotted clusters
#' 
#' @param id namespace
#' @param processed_results reactive dataframe that contains the results of hdbscan
#' @param base_layers reactive expression returning a character vector of selected layers from tree input. 
#' @param sf_layers_list List of layers containing sf objects with geometry. 
#'
#' @return List with elements: 
#'  \describe{
#'     \item{output}{map visualization of clusters}
#'   }

# Module for generating cluster map
library(shiny)
library(shinyjs)
library(DBI)
library(leaflet)
library(ggplot2)
library(sf)
library(dplyr)
source("R/helper_functions_v3.R")

clusterVizUI <- function(id) {
  ns <- NS(id)
  useShinyjs()
  
  fluidRow(
    column(6,
           card(
             card_body(
               leafletOutput(ns("clustermap"), height = "525px")
             )
           )
    ),
    column(6,
           card(
             card_body(
               plotOutput(ns("cluster_plot"), height = "525px")
             )
           )
    )
  )
}


clusterVizServer <- function(id, processed_results, base_layers = NULL, sf_layers_list = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Store the processed results
    processed_results <<- processed_results
    processed_results[[2]] <- processed_results[[2]] %>% tidyr::drop_na(cluster)  
    # Convert the raster data back to an sf object for clustering visualization
    clustered_sf <<- st_as_sf(processed_results[[1]], coords = c("x", "y"), crs = 4326)
    # Check and convert the geometry of `processed_results[[2]]`
    processed_results[[2]] <- st_as_sf(processed_results[[2]], coords = c("x", "y"), crs = 4326)
 
    # Define color palette for clusters
    cluster_pal <- colorFactor(palette = "viridis", domain = clustered_sf$cluster)
    cluster_pal2 <- colorFactor(palette = "viridis", domain = processed_results[[2]]$cluster, na.color = "transparent")
    
    # Render the Leaflet map
    output$clustermap <- renderLeaflet({
      leaflet(processed_results[[2]]) %>%
        addProviderTiles("CartoDB") %>%
        addPolygons(data = boro,
                    fillOpacity = 0,
                    weight = 2,
                    color = "red",
                    group = "Borough") %>%
        addPolygons(data = zcta,
                    fillOpacity = 0,
                    weight = 1,
                    color = "black",
                    group = "ZCTA") %>%
        addPolygons(data = uhf,
                    fillOpacity = 0,
                    weight = 1,
                    color = "blue",
                    group = "UHF Neighborhood") %>%
        addPolygons(data = ct,
                    fillOpacity = 0,
                    weight = 1,
                    color = "darkgreen",
                    group = "Census Tract") %>%
        addPolygons(data = processed_results[[2]], 
                    fillColor = ~cluster_pal2(cluster),
                    stroke = FALSE,
                    fillOpacity = 0.5, 
                    group = "Clusters",
                    highlight = highlightOptions(
                      weight = 2,
                      color = "#666",
                      fillOpacity = 0.9,
                      bringToFront = TRUE
                    ),
                    label = ~cluster,
                    layerId = ~cluster) %>%
        addLegend(pal = cluster_pal, values = ~cluster, title = "Clusters") %>%
        addLayersControl(baseGroups = c("Clusters"), 
                         overlayGroups = c("Borough", "ZCTA", "UHF Neighborhood", "Census Tract"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("UHF Neighborhood") %>%
        hideGroup("Borough") %>%
        hideGroup("Census Tract") %>%
        onRender("
          function(el, x) {
            var map = this;
            map.eachLayer(function(layer) {
              if (layer.feature && layer.feature.properties && layer.feature.properties.cluster) {
                layer.on('mouseover', function(e) {
                  console.log('Mouseover on cluster:', e.target.feature.properties.cluster); 
                  Shiny.setInputValue('clustermap_shape_mouseover', {
                    id: e.target.feature.properties.cluster,
                    lng: e.latlng.lng,
                    lat: e.latlng.lat
                  }, {priority: 'event'});
                });
              }
            });
          }
        ")
    })
    
    # Capture mouse hover event over the map
    # Observe mouse hover event
    observeEvent(input$clustermap_shape_mouseover, {
      req(input$clustermap_shape_mouseover$id)
      cluster_id <- input$clustermap_shape_mouseover$id
      
      tryCatch({
        cluster_id <- as.integer(cluster_id)
        if (!is.na(cluster_id)) {
          # Process the data for the hovered cluster
          data_subset <- processed_results[[2]] %>%
            as.data.frame() %>% 
            tidyr::drop_na() %>%
            mutate(cluster = as.factor(cluster)) %>%
            tidyr::pivot_longer(2:(length(.)-2), names_to = "variable", values_to = "value") %>%
            tidyr::drop_na() %>%
            group_by(cluster, variable) %>%
            summarize(mean = mean(value, na.rm = TRUE),
                      sd = sd(value, na.rm = TRUE),
                      se = sd / sqrt(n())) %>%
            filter(cluster == cluster_id)
          
          # Create the ggplot
          plot <- ggplot(data_subset, aes(x = variable, y = mean, fill = variable)) +
            geom_col(position = position_dodge(0.9)) +
            geom_errorbar(aes(ymin = mean - se*2, ymax = mean + se*2),
                          position = position_dodge(0.9),
                          width = 0.5,
                          size = 0.3) +
            scale_fill_viridis_d() +
            theme_minimal() +
            labs(title = "Characteristics of identified clusters",
                 subtitle = "Based on selected spatial layers",
                 caption = "Cluster 0 represents noise or background grid cells",
                 x = "Cluster ID",
                 y = "Mean z-score (+- 95% CI)") +
            theme(legend.position = "top",
                  axis.title.y = element_text(angle = 0, vjust = 0.5),
                  axis.title = element_text(size = 4),
                  axis.text = element_text(size = 3),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 3),
                  plot.title = element_text(size = 4),
                  plot.subtitle = element_text(size = 3),
                  plot.caption = element_text(size = 3, face = "italic"))
          
          # Add popup
          leafletProxy("clustermap") %>%
            clearPopups() %>%
            addPopups(
              lng = input$clustermap_shape_mouseover$lng,
              lat = input$clustermap_shape_mouseover$lat,
              popup = leafpop::popupGraph(plot),
              options = popupOptions(closeButton = FALSE, autoClose = TRUE)
            )
        }
      }, error = function(e) {
        #print(paste("Error:", e$message))  # Debug error messages
      })
    })
    
    
    # Load necessary libraries
    library(ggplot2)
    library(dplyr)
    library(tidyr)
    library(ggridges)  # For ridgeline plots
    
    output$cluster_plot <- renderPlot({
      # Extract unique clusters and variables
      cluster_levels <- unique(processed_results[[2]]$cluster)
      variable_levels <- names(processed_results[[2]])[2:(length(processed_results[[2]]) - 2)]  
      
      # Map cluster IDs to colors from the cluster map legend
      cluster_palette <- cluster_pal(cluster_levels)  
      cluster_palette <- setNames(cluster_palette, as.character(cluster_levels))  
      
      # Prepare the raw data for plotting
      raw_data <- processed_results[[2]] %>%
        as.data.frame() %>%
        tidyr::drop_na() %>%
        mutate(cluster = as.factor(cluster)) %>%
        tidyr::pivot_longer(cols = 2:(length(.) - 2), names_to = "variable", values_to = "value") %>%
        tidyr::drop_na()
      
      # Plot ridgeline visualization
      ggplot(raw_data, aes(x = value, y = variable, fill = cluster)) +
        geom_density_ridges(alpha = 0.7, scale = 1.2) +  # Ridgeline density plot
        scale_fill_manual(values = cluster_palette, name = "Cluster") +  # Match colors to map legend
        facet_wrap(~ cluster, ncol = 2, scales = "free_y") +  # Each cluster gets its own plot
        theme_minimal() +
        labs(title = "Cluster-Specific Ridgeline Plots",
             subtitle = "Ridgeline plots showing distributions across clusters",
             caption = "Cluster 0 represents noise or background grid cells",
             x = "Value",
             y = "Variable") +  
        theme(legend.position = "top",
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 12),
              plot.title = element_text(size = 18),
              plot.subtitle = element_text(size = 14),
              plot.caption = element_text(size = 12, face = "italic"),
              strip.text = element_text(size = 14, face = "bold"))
    })
    
    

  })
}
