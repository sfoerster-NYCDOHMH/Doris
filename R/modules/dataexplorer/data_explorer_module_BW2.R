#' @title Module responsible for leaflet map visualization on data explorer tab
#' @description
#' This module manages the visualization of spatial features selected and returned by data_tree_module. 
#' It sets up a leaflet map and includes various observers to update the map as users select layers in tree. 
#' 
#' @param id 
#' @param selected_layers reactive expression returning a character vector of selected layers in input. 
#' @param data_layers List of layers containing sf objects with geometry. 
#'    Naming of each list item is table_name.group, e.g. "age_by_gender.Female, 0-17"
#' @param base_layers List of sf objects for drawing admin boundaries for zcta, uhf, boro etc
#' @param variable Variable used for chloropleth maps, default is "percent_all" which is the 
#'    percentage of persons in NYC that belong to the specified group. This can be used to answer questions like
#'    "which area accounts for the highest proportion of people below the poverty line in NYC? 
#'    An alternative variable option is percent_geoid, which would give the proportion of the population in the chosen area 
#'    that belong to the specified group, and can answer questions about "in which area do people below the poverty line 
#'    account for the highest proportion of residents?". 
#'
#' @return List with elements: 
#'  \describe{
#'     \item{map_proxy}{Leaflet proxy object for external modifications}
#'     \item{active_layers}{Reactive expression returning character vector of active layer names}
#'   }


base64plots <- readRDS("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/Bilal/doris/base64plots2.rds")


mapVizUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map_explorer"))
  )
}


mapVizServer <- function(id, selected_layers, data_layers, base_layers, variable = "percent_all") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ### Set up the base map and proxy
    output$map_explorer <- renderLeaflet({
      
      bounds <- st_bbox(base_layers[["Borough"]])
      lng1 <- as.numeric(bounds$xmin)
      lat1 <- as.numeric(bounds$ymin)
      lng2 <- as.numeric(bounds$xmax)
      lat2 <- as.numeric(bounds$ymax)
      
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(data = base_layers[["Borough"]],
                    fillOpacity = 0,
                    weight = 2,
                    color = "red",
                    group = "Borough") %>%
        addPolygons(data = base_layers[["ZCTA"]],
                    fillOpacity = 0,
                    weight = 1,
                    color = "black",
                    group = "ZCTA") %>%
        addPolygons(data = base_layers[["UHF Neighborhood"]],
                    fillOpacity = 0,
                    weight = 1,
                    color = "blue",
                    group = "UHF Neighborhood") %>%
        addPolygons(data = base_layers[["Census Tract"]],
                    fillOpacity = 0,
                    weight = 1,
                    color = "darkgreen",
                    group = "Census Tract") %>%
        addLayersControl(
          overlayGroups = c("Borough", "ZCTA", "UHF Neighborhood", "Census Tract"),
          baseGroups = character(0),
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        hideGroup("UHF Neighborhood") %>% 
        hideGroup("Borough") %>% 
        hideGroup("Census Tract") %>% 
        fitBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2)
    })
    
    ### Make the proxy
    map_proxy <- leafletProxy("map_explorer")
    
    
    ## Update legend to last selected layer
    observeEvent(input$map_explorer_groups, {
      
      # Check for all active layers based on groups in leaflet
      active_layers <- input$map_explorer_groups
      print(paste("Names of active layers: ", names(active_layers)))
      
      # Check if any polygon layer is currently visible
      # active_polygon_layers <- base::intersect(active_layers, layer_groups$polygon_layers)
      active_polygon_layers <- base::intersect(active_layers, selected_layers())
      
      if(length(active_polygon_layers) > 0) {
        
        # Use the most recent active polygon layer to update the legend
        most_recent_polygon_layer <- tail(active_layers, 1)
        print(paste("Most recent polygon layer: ", names(most_recent_polygon_layer)))
        
        # Generate a palette for the legend
        pal <- colorNumeric(palette = "viridis", domain = data_layers[[most_recent_polygon_layer]][[variable]], na.color = "transparent")
        
        print(paste("Data from most recent polygon layer: ", head(data_layers[[most_recent_polygon_layer]][[variable]])))
        
        # Update the legend for the active polygon layer
        map_proxy %>%
          clearControls() %>%  # Clear previous legend
          addLegend(
            position = "bottomleft",
            pal = pal,
            values = data_layers[[most_recent_polygon_layer]][[variable]],
            title = most_recent_polygon_layer,
            opacity = 1
          )
        
      } else {
        
        # If no polygon layers are visible, clear the legend
        map_proxy %>% clearControls()
        
      }
    })
    
    
    # Keep track of what layers have been added
    added_layers <- reactiveVal(character())
    
    ### Dynamically adding and removing layers to/from leaflet ####
    observe({
      
      active_layers <- input$map_explorer_groups
      
      # Get layers selected in input
      selection <- selected_layers()
      print(paste("Variables selected in input: ", selected_layers()))
      
      # Get layers that have already been added to the map
      current_layers <- as.character(added_layers())
      print(paste("Current layers present before selection: ", current_layers, collapse = ", "))
      
      # Initialize character vectors for points and polygons
      # point_layers <- character(0)
      # polygon_layers <- character(0)
      
      # Layers to be added
      layers_to_add <- setdiff(selection, current_layers)
      print(paste("Layers being added to selection: ", layers_to_add))
      
      # Layers to be removed
      layers_to_remove <- setdiff(current_layers, selection)
      print(paste("Layers being removed from selection: ", layers_to_remove))
      
      # Update added_layers to only keep currently selected ones
      added_layers(selection)
      
      # Remove layers that are no longer selected
      purrr::walk(layers_to_remove, ~{
        map_proxy %>% clearGroup(.x)
        current_layers <- setdiff(current_layers, .x)
      })
      
      
      # Loop to add layers with dynamic palettes and layer control updates
      for (layer_name in layers_to_add) {
        
        print(paste("Layer being added: ", layer_name))
        
        # Find the corresponding layer in levels_data, a list of sf layers for each level
        layer_data <- data_layers[[layer_name]]
        # remove empty geometries before trying to add them to map
        layer_data <- layer_data[!st_is_empty(layer_data) & !is.na(st_geometry(layer_data)), ]
        # print(paste("Layer data: ", data_layers[[layer_name]]))
        
        # Determine if the layer is a point or polygon and add accordingly
        if (sf::st_geometry_type(layer_data)[1] == "POINT") {
          
          ## To do: this needs testing on new point layers in the data tree
          map_proxy %>%
            addCircleMarkers(
              data = layer_data,
              radius = 5,
              fillOpacity = 1,
              popup = ~paste0(
                "<div class='table-responsive'>",
                "<table class='table table-striped table-bordered'>",
                "<tr><td><strong>", variable, ":</strong></td><td>", layer_data[[variable]], "</td></tr>",
                "<tr><td><strong>Street Address:</strong></td><td>", layer_data$ADDRESS, "</td></tr>",
                "<tr><td><strong>City:</strong></td><td>", layer_data$CITY, "</td></tr>",
                "<tr><td><strong>Zip:</strong></td><td>", layer_data$ZIPCODE, "</td></tr>",
                "<tr><td><strong>Facility Type:</strong></td><td>", layer_data$FACTYPE, "</td></tr>",
                "<tr><td><strong>Facility Group:</strong></td><td>", layer_data$FACGROUP, "</td></tr>",
                "<tr><td><strong>Facility Subgroup:</strong></td><td>", layer_data$FACSUBGRP, "</td></tr>",
                "<tr><td><strong>Operated by:</strong></td><td>", layer_data$OPNAME, "</td></tr>",
                "<tr><td><strong>Overseeing Agency:</strong></td><td>", layer_data$OVERAGENCY, "</td></tr>",
                "</table>"
              ),
              label = ~lapply(paste0(
                "<div class='table-responsive'>",
                "<tr><td><strong>", variable, ": </strong></td><td>", layer_data[[variable]], "</td></tr>",
                "<tr><td><strong>", "<br><font style='color: red;'>CLICK POINT FOR MORE INFO</font>", "</strong></td><td></tr>"
              ), htmltools::HTML),
              group = layer_name
            )
          
        } else {
          
          # Generate color palette dynamically for polygon layers
          pal <- colorNumeric(palette = "viridis", domain = layer_data[[variable]], na.color = "transparent")
          
          most_recent <- selection[[length(selection)]]

          map_proxy %>%
            addPolygons(
              data = layer_data,
              fillColor = ~pal(layer_data[[variable]]),
              stroke = FALSE,
              weight = 0.5,
              fillOpacity = 0.5,
              popup = popupImage(img = unlist(base64plots[which(str_detect(names(base64plots), layer_name))]), src = "local"),
              #popup = paste0('<img src="', dataURI(file = "www/acs__age_group_summary-age_group-10001-10 to 14 years.png", mime = "image/png"),'" height="200" width="200">'),
              #popup = popupGraph(trendplots, data = layer_data, type = 'svg'),
              label = ~paste(variable, ": ", layer_data[[variable]]),
              group = layer_name
            )
        }
      } # end for loop for adding layers
      
      
      # Show only the most recently added layer
      if (length(selection) > 0 & sum(selection == "") == 0) {
        
        most_recent_layer <- selection[[length(selection)]]
        print(paste("Most recently added layer: ", most_recent_layer))
        
        purrr::walk(selection, ~{
          if (.x == most_recent_layer) {
            map_proxy %>% showGroup(.x)
          } else {
            map_proxy %>% hideGroup(.x)
          }
        })
        
        # Update Layers Control with point layers as overlayGroups and polygon layers as baseGroups
        map_proxy %>%
          addLayersControl(
            # Add all polygon layers as base groups
            # Point layers would need to be added as overlayGroups so they can be shown on top of base layers
            overlayGroups = c("Borough", "ZCTA", "UHF Neighborhood", "Census Tract"),
            baseGroups = selection,
            options = layersControlOptions(collapsed = FALSE)
          )
        
      } else {
        
        map_proxy %>%
          clearControls() %>% 
          addLayersControl(
            # Attempt to add all polygon layers as base groups but point layers as overlaygroups so multiple can be shown together
            overlayGroups = c("Borough", "ZCTA", "UHF Neighborhood", "Census Tract"),
            baseGroups = character(),
            options = layersControlOptions(collapsed = FALSE)
          )
        
      }
      
      
      # Selection is empty, reset the baseGroups layer
      if (length(selection) > 0 & sum(selection == "") == 1) {
        map_proxy %>%
          addLayersControl(
            # Attempt to add all polygon layers as base groups but point layers as overlaygroups so multiple can be shown together
            overlayGroups = c("Borough", "ZCTA", "UHF Neighborhood", "Census Tract"),
            baseGroups = character(),
            options = layersControlOptions(collapsed = FALSE)
          )
      }
      
    }) # end observer
    
    
    # Return the map proxy and active layers for other modules to use
    return(list(
      map_proxy = map_proxy,
      active_layers = reactive({ input$map_groups })
      
    ))
  })
}

