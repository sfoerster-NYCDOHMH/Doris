# Testing the proximity analysis module
library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(bslib)
library(conflicted)
conflicts_prefer(bslib::card)
source("R/helper_functions_v2.R")
source("R/modules/filtering/proximity_analysis_module.R")


# Load objects/data needed to test functionality
# To do: 
# - Need to save out a sample leaflet proxy object from filter module, along with a sample polygon feature
load("data/all_data_layers.rda")
load("data/facilities.rda")
load("tests/dev_data/level_data.rda")
load("tests/dev_data/filtered_areas.rda")


# Create test app function
proximityTestApp <- function() {
  
  ui <- fluidPage(
    proximityUI("proximity_module"),
    plotOutput("selection_plot")
  )
  
  server <- function(input, output, session) {
    
    # Make all_data_layers reactive for testing
    # data_reactive <- reactive({ all_data_layers })
    print(head(facilities))
    print(head(final_combined))
    
    # Initialize the module with the reactive data
    selection <- proximityServer(id = "proximity_module", 
                                 filtered_areas = reactive({ final_combined}),
                                 facilities = reactive({ facilities }))
    
    
    # Plot resulting points on simple map
    output$selection_plot <- renderPlot({
      print(paste("Selection: ", head(selection())))
      req(selection())
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
proximityTestApp()