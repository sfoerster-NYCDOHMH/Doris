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
#source("global.R")  # Load packages and global settings
source("R/helper_functions_v3.R")

clusterVizUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("clustermap"))
    #plotOutput(ns("cluster_plot"))
  )
}

clusterVizServer <- function(id, processed_results, base_layers = NULL, sf_layers_list = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #UNCOMMENT THE BELOW IN PRODUCTION
    #req(processed_results())
    processed_results <- processed_results
    
    # Render cluster map
    # Convert the `raster_data` back to an `sf` object
    clustered_sf <- st_as_sf(processed_results[[1]], coords = c("x", "y"), crs = 4326)

        # Define a color palette for the clusters
    cluster_pal <- colorFactor(palette = "viridis", domain = clustered_sf$cluster)
    cluster_pal2 <- colorFactor(palette = "viridis", domain = processed_results[[2]]$cluster, na.color = "transparent")
    
    # Prepare clustered data for Leaflet
    output$clustermap <- renderLeaflet({
      
      # Plot the clusters using Leaflet
      leaflet(clustered_sf) %>%
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
                    label = ~cluster) %>% 
        addLegend(pal = cluster_pal, values = ~cluster, title = "Clusters") %>%
        addLayersControl(baseGroups = c("Clusters"), 
                         overlayGroups = c("Borough", "ZCTA", "UHF Neighborhood", "Census Tract"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("UHF Neighborhood") %>% 
        hideGroup("Borough") %>% 
        hideGroup("Census Tract")
      
    })
    
    output$cluster_plot <- renderPlot({
      
      # selected_layer_info <- lapply(selected_layers, function(name) {
      #   for (group in names(sf_layers_list)) {
      #     if (name %in% names(sf_layers_list[[group]])) {
      #       return(sf_layers_list[[group]][[name]])  # returns the entire list with layer and column
      #     }
      #   }
      #   return(NULL)  # return NULL if not found
      # })
      # 
      # 
      # # Remove any NULLs in case any selected layers were not found
      # selected_layer_info <- selected_layer_info[!sapply(selected_layer_info, is.null)]
      # 
      # 
      # # Now separate them into two lists
      # layers_list <- lapply(selected_layer_info, function(x) x$layer)
      # fields_list <- lapply(selected_layer_info, function(x) x$column) 
      # print(fields_list)

      processed_results[[2]] %>%
        as.data.frame() %>% 
        tidyr::drop_na() %>%
        mutate(cluster = as.factor(cluster)) %>%
        tidyr::pivot_longer(2:(length(.)-2), names_to = "variable", values_to = "value") %>%
        tidyr::drop_na() %>%
        group_by(cluster, variable) %>%
        summarize(mean = mean(value, na.rm = TRUE),
                  sd = sd(value, na.rm = TRUE),
                  se = sd / sqrt(n())) %>%
        ggplot(aes(x = cluster, y = mean, fill = variable)) +
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
             y = "Mean z-score\n(+- 95% CI)") +
        theme(legend.position = "top",
              axis.title.y = element_text(angle = 0, vjust = 0.5),
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(size = 14),
              plot.title = element_text(size = 18),
              plot.subtitle = element_text(size = 14),
              plot.caption = element_text(size = 12, face = "italic"))

    })

    runjs("document.querySelector('a[data-value=\"Cluster Map\"]').click();")
    
    # Re-enable the button and reset its label
    enable("calculate")
    shinyjs::html("calculate", "Calculate Clusters")
  })
}
