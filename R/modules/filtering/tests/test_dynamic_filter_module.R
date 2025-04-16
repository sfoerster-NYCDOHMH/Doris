# Testing the dynamic filter module
library(shiny)
library(dplyr)
library(leaflet)
source("R/helper_functions_v2.R")
source("R/modules/filtering/dynamic_filter_module.R")

# Load test data
load("data/all_data_layers.rda")
load("tests/dev_data/level_data.rda")

# Create test app function
filterUITestApp <- function() {
  
  ui <- fluidPage(
    filterUI("filter_module"),
    verbatimTextOutput("selection_text"),
    plotOutput("selection_plot")
  )
  
  server <- function(input, output, session) {
    
    # Make all_data_layers reactive for testing
    data_reactive <- reactive({ all_data_layers })
    
    # Initialize the module with the reactive data
    selection <- filterServer(id = "filter_module", 
                              data_layers = reactive({ level_data }))
    
    # Text output of selection, should be a list of coordinates to draw polygons
    output$selection_text <- renderPrint({
      if (is.null(selection()) || length(selection()) == 0) {
        "Nothing selected yet"
      } else {
        paste("Current selection:", paste(selection()[1]))
      }
    })
    
    
    output$selection_plot <- renderPlot({
      if (is.null(selection()) || length(selection()) == 0) {
        "Nothing selected yet"
      } else {
        plot(selection())
      }
    })
  }
  
  shinyApp(ui, server)
}

# Run the test app
filterUITestApp()