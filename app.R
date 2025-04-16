### Doris v0.10.1
### Last updated: 2025-04-16

# Change log
# Changes from 0.9.8
# - fixed cluster calculation to work on flat list of layers
# Changes from 0.9.9
# - all original content now modularized
# Changed from 0.10
# - added logic to make sourcing and loading of files compatible with Databricks architecture

# Simple environment check: is this Databricks?
is_databricks <- Sys.getenv("DATABRICKS_RUNTIME_VERSION") != ""

# Define base paths depending on environment
if (is_databricks) {
  app_root <- "/databricks/driver/DorisDashboard"
} else {
  # Local dev path in RStudio Server
  app_root <- "."
}

setwd(app_root)
source("global.R")
source("R/helper_functions_v3.R")
source("R/modules/data explorer/data_tree_module.R")
source("R/modules/data explorer/data_explorer_module.R")
source("R/modules/filtering/dynamic_filter_module.R")
source("R/modules/filtering/proximity_analysis_module.R")
source("R/modules/clustering/cluster_detection_module.R")
source("R/modules/clustering/cluster_visualization_module.R")
source("R/modules/clustering/cluster_plot_module.R")

useShinyjs()

ui <- page_navbar(
  title = "DORIS v0.10.1",
  id = "nav_tabs", 
  fillable = TRUE,
  
  sidebar = sidebar(
    useWaiter(),
    tags$style(
      ".waiter-overlay{
      background-repeat: no-repeat;
      background-position: center;
      background-size: 600px;
    }"),
    waiterPreloader(
      html = spin_2(),
      image = "doris_splash1.png",
      fadeout = TRUE,
      logo = ""
    ),
    # title = "Inputs & Parameters",
    width = "25%",
    # Base content that appears in all tabs
    accordion(
      id = "sidebar_sections",
      open = TRUE,
      multiple = TRUE,
      
      # Show tree input on all tabs for now, but close on all but first tab
      accordion_panel(
        title = "Select Data Layers",
        value = "panel_layer_tree",
        dataTreeUI("tree")
      ),
      
      # Show dynamic filters sidebar panel on second tab
      conditionalPanel(
        condition = "input.nav_tabs == 'tab_dynamic_filters'",
        filterUI("filter_module"),
        proximityUI("proximity_module")
      ),
      # Show cluster detection sidebar panel on tab 3
      conditionalPanel(
        condition = "input.nav_tabs == 'tab_cluster_detection'",
        clusterUI("runcluster")
      )
    )
  ),
  
  # Tab 1: Interactive Data Explorer
  nav_panel(
    title = "Interactive Data Explorer", 
    value = "tab_data_explorer",
    card(
      card_body(
        mapVizUI("map_explorer")
      )
    )
  ),
  
  # Tab 2 UI: Dynamic Filtering & Proximity Analysis ####
  nav_panel(
    title = "Dynamic Filters",
    value = "tab_dynamic_filters", 
    card(
      card_header("Map output of filtered areas"),
      card_body(
        leafletOutput("selected_areas_map")
      )
    )
  ), # end tab 2
  
  
  # Tab 3 UI: Cluster Map ####
  nav_panel(
    title = "Cluster Detection",
    value = "tab_cluster_detection",
    card(
      card_header("Cluster Visualization"),
      card_body(
        clusterVizUI(id="clusterviz")
      )
    )
  ),
  
  # Tab 4 UI: Cluster Characteristics ####
  nav_panel(
    title = "Cluster Characteristics",
    value = "tab_cluster_characteristics", 
    card(
      card_header("Cluster Characteristics Summary"),
      clusterPlotUI(id="clusterplot")
    )
  )
)



server <- function(input, output, session) {
  
  Sys.sleep(2)
  # Control what is shown on the sidebar depending on tab
  observeEvent(input$nav_tabs, {
    req(input$nav_tabs)
    if (input$nav_tabs == "tab_data_explorer") {
      # For Tab 1, only open the tree panel
      accordion_panel_open(
        id = "sidebar_sections",
        values = "panel_layer_tree"
      )
    } else if (input$nav_tabs == "tab_dynamic_filters") {
      # For Tab 2, open both panels (or just the dynamic filters)
      accordion_panel_close(
        id = "sidebar_sections",
        values = "panel_layer_tree"
      )
      accordion_panel_set(
        id = "sidebar_sections",
        values = "panel_dynamic_filters"
      )
    } else if (input$nav_tabs == "tab_cluster_detection") {
      accordion_panel_close(
        id = "parameter_section",
        values = "panel_adjust_parameters"
      )
    }
  })
  
  
  
  ## LOAD MODULAR COMPONENTS ##
  ## ~~~~~~~~~~~~~~~~~~~~~~~ ##
  
  
  ### Tab 1 - Data Explorer ####
  
  # Create tree input
  tree_selection <- dataTreeServer(
    id = "tree", 
    data_layers = reactive ({ all_data_layers })
  )
  
  # Create interactive leaflet map responding to tree input
  map_explorer <- mapVizServer(
    id = "map_explorer",
    data_layers = level_data, # static
    selected_layers = tree_selection, # reactive
    base_layers = base_layers # static object
  )
  
  
  ### Tab 2 - Dynamic Filters & Proximity Analysis ####
  
  ## Load dynamic filter module
  filtered_areas <- filterServer("filter_module", reactive({ level_data }))
  
  ### Plot results as applicable
  # Render Leaflet map
  output$selected_areas_map <- renderLeaflet({
    req(filtered_areas())
    
    if(length(filtered_areas())>0){
      
      leaflet(options = leafletOptions(minZoom = 11, maxZoom = 14)) %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(
          data = filtered_areas(),
          color = "red",
          weight = 2,
          opacity = 0.8,
          fillOpacity = 0.5,
          group = "Area matching filter criteria"
        ) %>% 
        addLayersControl(overlayGroups = character(0),
                         baseGroups = "Area matching filter criteria",
                         options = layersControlOptions(collapsed = FALSE))
    } else {
      
      show_alert(
        title = "Nothing to see here",
        text = tags$span(
          "Your combination of layers and filter criteria produced no overlapping areas.",
          tags$br(),
          tags$br(),
          "Try reducing the number of features combined with AND conditions.",
          tags$br(),
          tags$br(),
          tags$h2("Good luck!")
        ),
        type = "warning",
        btn_labels = "Ok",
        btn_colors = "#3085d6",
        html = TRUE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE
      )
    }
  }) # end renderLeaflet
  
  
  ## Create a leaflet proxy for further use below
  selected_areas_proxy <- leafletProxy("selected_areas_map")
  
  
  ## Load proximity analysis module
  all_points <- proximityServer(
    id = "proximity_module",
    filtered_areas = filtered_areas,
    facilities = reactive({ facilities })
  )
  
  observe({
    req(all_points())
    # Update map with results
    selected_areas_proxy %>%
      clearGroup("Matched Locations of Interest") %>%
      addCircleMarkers(
        data = all_points(),
        color = "green",
        radius = 5,
        fillOpacity = 0.7,
        popup = ~paste("Facility Name: ", FACNAME),
        group = "Matched Locations of Interest"
      ) %>%
      addLayersControl(
        overlayGroups = c("Matched Locations of Interest"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  
  
  ### Tab 3 -- Cluster Map ####
  
  ### Cluster detection ####
  # Observe the calculate button
  # This results in a map with detected clusters
  processed_results <- clusterServer(
    id = "runcluster", 
    selected_layers = tree_selection(),
    sf_layers_list = level_data,
    boundary = zcta 
  )
  
  
  
  observeEvent(processed_results(), {
    
    ### Tab 3 -- Cluster Characteristics ####
    clusterVizServer("clusterviz",
                     processed_results = processed_results(), 
                     base_layers = tree_selection(), 
                     sf_layers_list = level_data)
    
    clusterPlotServer("clusterplot",
                     processed_results = processed_results(), 
                     base_layers = tree_selection(), 
                     sf_layers_list = level_data)

    # Sys.sleep(3)
    runjs("document.querySelector('a[data-value=\"Cluster Map\"]').click();")
    
    # Re-enable the button and reset its label
    enable("calculate")
    shinyjs::html("calculate", "Calculate Clusters")
    
  })

  ### Plotting individual layers- Static ####
  
}

shinyApp(ui, server)
