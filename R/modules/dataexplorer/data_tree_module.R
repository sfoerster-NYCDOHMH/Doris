#' @title Data Tree Input Module
#' @description
#' This module is responsible for creating a tree-type select input using the shinyTree library
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
    actionButton(ns('reset'), 'Reset nodes'),
    "Search:",
    shinyTree(ns("tree"), theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE, search = TRUE,
              types = "{ 'default': { 'a_attr' : { 'style' : 'font-size:16px;' }}}"
    )
  )
}


dataTreeServer <- function(id, data_layers) {
  moduleServer(id, function(input, output, session) {
    # use namespace
    ns <- session$ns
    
    treeSelection <- reactiveVal(character(0))
    
    # Generate tree data
    tree_data <- reactive({
      req(data_layers())
      generate_jstree_data(data_layers())
    })
    
    # Generate tree
    output$tree <- renderTree({
      tree_data()
    })
    
    # reset tree button
    observeEvent(input$reset, {
      updateTree(session, "tree", data = tree_data())
      treeSelection(character(0))
    })
    
    # Return selected layers
    observeEvent(input$tree,{
      tree.sel <- get_selected(input$tree, format = "names")
      selections <- unlist(tree.sel)[tree.sel %notin% names(input$tree)]
      treeSelection(as.character(selections))
    })
    
    reactive({
      treeSelection()
    })
    
  })
}


# helper function
`%notin%` <- Negate(`%in%`) 


# helper function to create the tree input from the input list 'all_data_layers'
generate_jstree_data <- function(all_data_layers) {
  
  # Helper function to capitalize first letter of each word
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  }
  
  result <- list()
  
  for (table_name in names(all_data_layers)) {
    
    layer_info <- all_data_layers[[table_name]]
    
    if (!is.null(layer_info) && !is.null(layer_info$layer)) {
      # Get unique group values directly from the group column
      if ("group" %in% names(layer_info$layer)) {
        group_values <- unique(na.omit(as.character(layer_info$layer$group)))
        
        if (length(group_values) > 0) {
          
          # Format the group name (replacing underscores with spaces and capitalizing)
          group_name <- .simpleCap(gsub("_", " ", table_name))
          
          # create vector of children
          vec <- as.list(1:length(group_values))
          names(vec) <- group_values
          
          # Nest the 'vec' list within another level, using the value of group_name as the key
          result[[group_name]] <- vec
        }
      }
    }
  }
  
  return(result)
}

