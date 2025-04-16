# Version 1
# Updated: 2025-04-08
# Author: Christopher Singh, csingh2@health.nyc.gov
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title Module responsible for generating cluster characteristics plot from hdbscan outputs
#' @description
#' This module takes the output of the hdbscan clustering algorithm and generates. 
#' a plot of all clusters at each selected layer
#' 
#' @param id namespace
#' @param processed_results reactive dataframe that contains the results of hdbscan
#' @param base_layers reactive expression returning a character vector of selected layers from tree input. 
#' @param sf_layers_list List of layers containing sf objects with geometry. 
#'
#' @return List with elements: 
#'  \describe{
#'     \item{output}{clusters characteristics plot}
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

clusterPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    #leafletOutput(ns("clustermap")),
    plotOutput(ns("cluster_plot"))
  )
}

clusterPlotServer <- function(id, processed_results, base_layers = NULL, sf_layers_list = NULL) {
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
    
    output$cluster_plot <- renderPlot({

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
    
    #runjs("document.querySelector('a[data-value=\"Cluster Map\"]').click();")
    
    # Re-enable the button and reset its label
    #enable("calculate")
    #shinyjs::html("calculate", "Calculate Clusters")
  })
}
