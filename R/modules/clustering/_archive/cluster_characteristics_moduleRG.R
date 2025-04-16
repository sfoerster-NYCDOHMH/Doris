# Module for running hierarchical dbscan cluster detection algorithm
clusterUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_body(
        "Clicking the button below will initiate a hierarchical dbscan machine learning cluster detection algorithm to detect spatial units with similar qualities across all dimensions currently selected in the 'Data Explorer'."
      ),
      card_body(
        accordion(
          open = FALSE,
          accordion_panel(
            "Adjust Parameters",
            card(
              card_header("The hierarchical dbscan algorithm implemented here is influenced by the minimum number of points required in a cluster, which you can change below..."),
              card_body(
                sliderInput(ns("minpts"), "Minimum Number of Points", min = 1, max = 20, value = 10, step = 1)
              )
            )
          )
        )
      ),
      card_footer(
        actionButton(ns("calculate"), "Calculate Clusters", class = "btn-primary btn-block")
      )
    )
  )
}

clusterServer <- function(id, selected_layers, sf_layers_list, base_extent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store cluster results
    cluster_results <- reactiveVal(NULL)
    
    # Handle calculate button
    observeEvent(input$calculate, {
      shinyjs::disable("calculate")
      shinyjs::html("calculate", "Crunching numbers...")
      
      # Process selected layers
      # ...
      
      # Run clustering
      processed_results <- do_hdbscan(
        sf_layers = layers_list, 
        field_list = fields_list, 
        extent = base_extent, 
        minPts = input$minpts
      )
      
      # Store results
      cluster_results(processed_results)
      
      # Re-enable button
      shinyjs::enable("calculate")
      shinyjs::html("calculate", "Calculate Clusters")
    })
    
    # Return results
    return(cluster_results)
  })
}
