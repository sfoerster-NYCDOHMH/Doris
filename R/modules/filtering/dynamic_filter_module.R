#' @title Dynamic Filters
#' @description
#' This module contains the dynamic filter sidebar for selection of low/high expression of variables based on quartiles (UI). 
#' It also contains the logic used to intersect the selected areas to produce a final polygon that matches all conditions. 
#' 
#' @param id 
#' @param data_layers List of layers containing sf objects with geometry. 
#'    Naming of each list item is table_name.group, e.g. "age_by_gender.Female, 0-17"
#'    This is the same object used by the data explorer module and not reactive, it is preloaded at start of app from saved data
#'
#' @return Returns a multi-polygon sf feature for plotting that unions all matching areas to speed up plotting
#'    Note that for zcta resolution, it may become feasible to keep the original polygons and plot them individually; 
#'    This will allow selection of sub-areas for further characterization, though selecting one large contiguous area would also be useful
#'    Note that there could be severl disconnected large polygons resulting from the spatial intersection, each could be selected and characterized

# Filter UI sidebar section
filterUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Use the filters below to look for areas low and/or high on selected criteria."),
      card_body(
        selectizeInput(ns("layer_high"), "Areas high in all of these metrics:", choices = NULL, multiple = TRUE),
        selectInput(ns("combine"), "If selecting another set below, how do you want to combine the two sets?",
                    choices = c("OR", "AND"), selected = "AND"),
        selectizeInput(ns("layer_low"), "Areas low in all of these metrics:", choices = NULL, multiple = TRUE),
        actionButton(ns("apply_filters"), "Apply Filters"),
        div(id = ns("spinner-placeholder"))
      )
    )
  )
}


# Filter server logic
filterServer <- function(id, data_layers) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Add quartiles to layers
    level_data_quartiles <- reactive({
      req(data_layers())
      lapply(data_layers(), add_quartiles)
    })
    
    # Update input choices
    observe({
      updateSelectizeInput(session, "layer_high", choices = names(level_data_quartiles()))
      updateSelectizeInput(session, "layer_low", choices = names(level_data_quartiles()))
    })
    
    # Store the final filtered areas
    filtered_areas <- reactiveVal(NULL)
    
    # Handle filter button click
    observeEvent(input$apply_filters, {
      # Show spinner
      shinyjs::runjs(sprintf("
        $('#%s').html(
          '<div class=\"spinner-border text-warning\" role=\"status\">' +
          '<span class=\"sr-only\"></span>' +
          '</div>'
        );
      ", ns("spinner-placeholder")))
      
      # Retrieve selected layers
      selected_layers_high <- input$layer_high
      selected_layers_low <- input$layer_low
      
      
      # Filter high value layers (Quartile 4)
      high_layers <- lapply(selected_layers_high, function(layer) {
        level_data_quartiles()[[layer]] %>% filter(quartile == 4) %>% 
          select(quartile)
      })
      
      # Combine high-value layers by intersecting
      if (length(high_layers) > 1) {
        combined_high <- st_make_valid(reduce(high_layers, st_intersection)["quartile"] %>% 
                                         group_by(quartile) %>% 
                                         summarise(geometry = st_union(geometry), .groups = "drop"))
      } else {
        combined_high <- st_make_valid(high_layers[[1]]["quartile"] %>% 
                                         group_by(quartile) %>% 
                                         summarise(geometry = st_union(geometry), .groups = "drop"))
      }
      
      # Filter low value layers (Quartile 1, if selected)
      if (length(selected_layers_low) > 0) {
        low_layers <- lapply(selected_layers_low, function(layer) {
          level_data_quartiles()[[layer]] %>% filter(quartile == 1)
        })
        
        # Combine low-value layers by intersecting
        if (length(low_layers) > 1) {
          combined_low <- st_make_valid(reduce(low_layers, st_intersection)["quartile"] %>% 
                                          group_by(quartile) %>% 
                                          summarise(geometry = st_union(geometry), .groups = "drop"))
        } else {
          combined_low <- st_make_valid(low_layers[[1]]["quartile"] %>% 
                                          group_by(quartile) %>% 
                                          summarise(geometry = st_union(geometry), .groups = "drop"))
        }
        
        # Combine high and low layers based on user selection
        if (input$combine == "OR") {
          final_combined <- st_union(combined_high, combined_low)["quartile"]
        } else if (input$combine == "AND") {
          final_combined <- st_intersection(combined_high, combined_low)["quartile"]
        }
      } else {
        final_combined <- combined_high
      }
      
      # Store results
      filtered_areas(final_combined)
      
      # Save out as test object for other modules for debugging
      tryCatch({
        save(final_combined, file = "tests/dev_data/filtered_areas.rda")
        }, error = function(e) {
          print("Result not saved")
        }
      )
      
      # Hide spinner
      shinyjs::runjs(sprintf("$('#%s').html('');", ns("spinner-placeholder")))
    
    }) # end observeEvent on filter button
    
    # Return the filtered areas
    return(filtered_areas)
    
  }) # end module server function
} # end filterServer function



