#' @title Proximity Analyses
#' @description
#' This module adds further sidebar inputs for conducting proximity analyses of point features in relation to polygon features, 
#' including a slider for changing the distance threshold and a button to initiate the analyses
#' 
#' @param id 
#' @param filtered_areas reactive object from the filter module that creates a union of areas matching filter conditions 
#'    This should be a multi-polygon sf feature layer matching the coordinate system (WGS84) and projection of other data
#' @param facilities static sf point geometry layer holding facility information
#'    Currently relies on the existence of following columns: FACGROUP, FACSUBGRP, FACTYPE
#' @param map_proxy a leaflet proxy to which results can be added   
#'
#' @return Returns a point sf feature layer with point geographies falling within the selected distance
#'    

### Sidebar items for proximty analysis including inputs for point features, distance slider, and button
# Note: May need some optimization in terms of nested accordion panels... 
#       Another planned feature is to conduct overlap analyses based on spatial overlap of catchment areas rather than simple distance
proximityUI <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      open = TRUE,
      accordion_panel(
        "Proximity Analysis",
        selectizeInput(ns("facgroup"), "Facilities by Group:", choices = NULL, multiple = TRUE),
        selectizeInput(ns("facsubgrp"), "Facilities by Subgroup:", choices = NULL, multiple = TRUE),
        selectizeInput(ns("factype"), "Facilities by Type:", choices = NULL, multiple = TRUE),
        accordion(
          open = "Distance Threshold",
          accordion_panel(
            "Distance Threshold",
            card(
              card_header("This method looks for points of interest in or within a given distance from the edge of the filtered areas above."),
              card_body(
                sliderInput(ns("distance_threshold"), "Distance Threshold (in miles):", 
                            min = 0, max = 2, value = 0.1, step = 0.2),
                actionButton(ns("proximity_button"), "Run Proximity-Based Selection", class = "btn-primary btn-block")
              )
            )
          )
        )
      )
    )
  )
}


proximityServer <- function(id, filtered_areas = filtered_areas, facilities = facilities) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    allpoints <- reactiveVal()
    
    # Update input choices
    observe({
      updateSelectizeInput(session, "facgroup", choices = unique(facilities()$FACGROUP))
      updateSelectizeInput(session, "facsubgrp", choices = unique(facilities()$FACSUBGRP))
      updateSelectizeInput(session, "factype", choices = unique(facilities()$FACTYPE))
    })
    
    # Filter facilities based on selection
    filtered_point_data <- reactive({
      req(facilities())
      
      filtered <- facilities()
      
      if (length(input$facgroup) > 0) {
        filtered <- filtered[filtered$FACGROUP %in% input$facgroup, ]
      }
      
      if (length(input$facsubgrp) > 0) {
        filtered <- filtered[filtered$FACSUBGRP %in% input$facsubgrp, ]
      }
      
      if (length(input$factype) > 0) {
        filtered <- filtered[filtered$FACTYPE %in% input$factype, ]
      }
      
      return(filtered)
    })
    
    # Handle proximity button
    observeEvent(input$proximity_button, {
      req(filtered_areas(), filtered_point_data())
      
      area <- st_transform(filtered_areas(), crs = 32618)
      points <- filtered_point_data()
      points <- st_transform(points, crs = 32618)["FACNAME"]
      
      # Convert distance threshold from miles to meters
      distance_threshold_meters <- input$distance_threshold * 1609.34
      # distance_threshold_meters <- 1 * 1609.34
      
      # Identify points within the polygons
      points_within <- points[st_within(points, area, sparse = FALSE), ]
      
      # Identify points within the distance threshold from polygon boundaries
      near_indices <- which(
        lengths(st_is_within_distance(points, area, dist = distance_threshold_meters)) > 0
      )
      
      points_near <- points[near_indices, ]
      
      # Combine the two results to avoid duplicates
      all_points <- unique(rbind(points_within, points_near)) %>% st_transform(crs = 4326)
      allpoints(all_points)
    })
    
    # Return any relevant values
    return(allpoints)
  })
}

