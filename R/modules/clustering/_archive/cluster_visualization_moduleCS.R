# R/modules/cluster_viz_module.R
clusterVizUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("clustermap")),
    leafletOutput(ns("cluster_plot"))
  )
}

clusterVizServer <- function(id, cluster_results, base_layers) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render cluster map
    output$clustermap <- renderLeaflet({
      req(cluster_results())
      
      processed_results <- cluster_results()
      clustered_sf <- st_as_sf(processed_results[[1]], coords = c("x", "y"), crs = 4326)
      
      # Define color palette
      cluster_pal <- colorFactor(palette = "viridis", domain = clustered_sf$cluster)
      cluster_pal2 <- colorFactor(palette = "viridis", domain = processed_results[[2]]$cluster, na.color = "transparent")
      
      # Create map
      # Plot the clusters using Leaflet
      leaflet(clustered_sf) %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(data = boro,
                    fillOpacity = 0,
                    weight = 2,
                    color = "red",
                    group = "Borough") %>%
        addPolygons(data = nta,
                    fillOpacity = 0,
                    weight = 1,
                    color = "black",
                    group = "NTA") %>%
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
                         overlayGroups = c("Borough", "NTA", "UHF Neighborhood", "Census Tract"),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("UHF Neighborhood") %>% 
        hideGroup("Borough") %>% 
        hideGroup("Census Tract")
    })
    
    output$cluster_plot <- renderPlot({
      
      processed_results[[2]] %>% 
        as.data.frame() %>% 
        mutate(cluster = as.factor(cluster)) %>% 
        tidyr::pivot_longer(2:(2+length(fields_list)-1), names_to = "variable", values_to = "value") %>% 
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
    
    # No return value needed
  })
}