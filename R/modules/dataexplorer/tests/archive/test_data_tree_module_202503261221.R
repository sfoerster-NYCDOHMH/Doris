# Testing the data tree module
library(shiny)
library(jsTreeR)
library(stringr)

# Source in functions, modules and data files need for testing, can use production data as well
source("R/helper_functions_v2.R")
source("R/modules/data explorer/data_tree_module.R")
load("data/all_data_layers.rda")



# Create test app function
dataTreeTestApp <- function() {
  
  ui <- fluidPage(
    titlePanel("Data Tree Test"),
    dataTreeUI("tree"),
    verbatimTextOutput("selection_text")
  )
  
  server <- function(input, output, session) {
    
    # Make all_data_layers reactive for testing
    data_reactive <- reactive({ all_data_layers })
    
    # Initialize the module with the reactive data
    selection <- dataTreeServer("tree", data_reactive)
    
    output$selection_text <- renderPrint({
      if (is.null(selection()) || length(selection()) == 0) {
        "Nothing selected yet"
      } else {
        paste("Current selection:", paste(selection(), collapse=", "))
      }
    })
  }
  
  shinyApp(ui, server)
}


# Run the test app
dataTreeTestApp()
