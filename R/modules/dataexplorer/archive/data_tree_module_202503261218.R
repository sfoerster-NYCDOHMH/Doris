#' @title Data Tree Input Module
#' @description
#' This module is responsible for creating a tree-type select input using the jsTree java library
#' It creates a nested list of tables and groups contained within them in a parent-child node relationship
#' Parent nodes can be expanded, all children can be selected at once or individually, and both parent and children
#' nodes can be searched without breaking the nesting between them. 
#' 
#' @param id 
#' @param data_layers reactive expression returning a list of data layers with structure: 
#'  \describe {
#'    \item{layer}{spatial features sf object with geometry. needs to have a column called "group" that will be used to create child nodes}
#'    \item{grouping_vars}{variables used for grouping, this is currently not used}
#'    \item{numeric_vars}{numeric variables}
#'    \item{resolution}{resolution of the sf layer, either one of zcta, uhf, borough}
#'  }
#'
#' @return character vector of selected layers by name. names are used to subset lists of sf layers so should match 
#'    names used for those objects. if input type is changed to another style select input, as long as the function returns 
#'    a character vector with sf layer names, it should not break any dependencies 
dataTreeUI <- function(id) {
  # create namespace
  ns <- NS(id)
  tagList(
    jstreeOutput(ns("tree"))
  )
}

  
dataTreeServer <- function(id, data_layers) {
  moduleServer(id, function(input, output, session) {
    # use namespace
    ns <- session$ns
    
    # Generate tree data
    tree_data <- reactive({
      req(data_layers())
      generate_jstree_data(data_layers())
    })
    
    output$tree <- renderJstree({
      req(tree_data())
      jstree(
        nodes = tree_data(),
        checkboxes = TRUE,
        search = list(
          show_only_matches = FALSE,  # Shows parent nodes when children match
          close_opened_folders = FALSE,  # Keep folders open when filtering
          show_matches_children = TRUE
        ),
        elementId = ns("tree_search"),
        selectLeavesOnly = TRUE
      )
    })
    
    # Return selected layers
    # This is the only result generated from this module that is consumed by other modules/app components
    reactive({
      # If nothing is selected, return empty character vector
      if (is.null(input$tree_selected)) {
        character(0)
      } else {
        selections <- unlist(input$tree_selected)
        selections <- selections[!grepl("^GROUP_", selections)]
        as.character(selections)
      }
    })
  })
}

