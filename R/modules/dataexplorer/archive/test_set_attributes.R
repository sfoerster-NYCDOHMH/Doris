library(shiny)
library(shinyTree)

ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE),
  verbatimTextOutput("selected")
)

server <- function(input, output, session) {
  treeData <- list(
    "Parent 1" = list(
      "Child 1" = structure(1, stopened = TRUE),
      "Child 2" = 2
    ),
    "Parent 2" = list(
      "Child 3" = 3,
      "Child 4" = 4
    )
  )
  
  output$tree <- renderTree({
    treeData
  })
  
  tree_prev <- reactiveVal(NULL) # Initialize tree_prev as a reactiveVal
  
  observeEvent(input$tree, {
    
    #tree_save <<- input$tree
    selected_nodes <<- get_selected(input$tree, format = "names")
    deselected_node <<- isolate(tree_prev()) # Access the reactive value using tree_prev()
    
    print(selected_nodes)
    print(deselected_node)
    
  
      if (!is.null(deselected_node)) {
        deselected_node_names <- get_selected(deselected_node, format = "names")
        
        if (!is.null(deselected_node_names)) {
          #find the node that was just deselected.
          just_deselected = setdiff(get_selected(isolate(tree_prev()), format = "names"), get_selected(input$tree, format = "names"))
          
          if (length(just_deselected) > 0) {
            # attrs <- list(`just_deselected[1]` = list(stopened = FALSE))
            # set_node_attrs(session, "tree", attrs)
            
            #attr(tree_save$`Parent 1`[[1]],"stopened")
            
            str(set_node_attrs(input$tree, attr_name = "stopened", inner_val = FALSE,leaf_val = FALSE))
            str(set_node_attrs(input$tree$`Parent 1`[[1]], attr_name = "stopened", inner_val = FALSE,leaf_val = FALSE))
          }
        }
      }
    tree_prev(input$tree) # Update tree_prev using tree_prev(value)
  })
  
  output$selected <- renderPrint({
    get_selected(input$tree, format = "names")
  })
}

shinyApp(ui, server)