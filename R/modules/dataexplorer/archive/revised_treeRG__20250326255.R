library(shiny)
library(shinyTree)

`%notin%` <- Negate(`%in%`) 

setwd("~/networkDrives/smb-share:server=naslocshare240,share=agencyshare/Informatics_Share/ORRU Data Team/DORIS_DASHBOARD")
 load("data/all_data_layers.rda")

options(shinyTree.setState = FALSE)
options(shinyTree.refresh  = FALSE)
options(shinyTree.show_only_matches  = FALSE)
options(shinyTree.show_only_matches_children  = FALSE)





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

         # browser()
          # Format the group name (replacing underscores with spaces and capitalizing)
          group_name <- .simpleCap(gsub("_", " ", table_name))

          test <- as.list(1:length(group_values))
          names(test) <- group_values
          
          # Nest the 'test' list within another level, using the value of group_name as the key
          result[[group_name]] <- test
          
          #result <- c(result, list(group_node))
        }
      }
    }
  }

  return(result)
}





tree <- generate_jstree_data(all_data_layers)

ui <- fluidPage(  # Switch from pageWithSidebar to fluidPage
  # Application title
  headerPanel("shinyTree with built-in themes"),
  
  mainPanel(
    actionButton('reset', 'Reset nodes'),
    shinyTree("tree", theme="proton", themeIcons = FALSE, themeDots = FALSE, checkbox = TRUE, search = TRUE,#list(TRUE,show_only_matches  = TRUE),
              types = "{ 'default': { 'a_attr' : { 'style' : 'font-size:16px;' }}}"
             ),
    textOutput("sel_names")
  )
  

  
)

server <- function(input, output, session) {
  
  output$tree <- renderTree({
    tree
  })
  
  observeEvent(input$reset, {
    updateTree(session, "tree", data = tree)
  })
  
  output$sel_names <- renderPrint({
    tree <- input$tree
    test <- get_selected(tree, format = "names")
    
    req(length(test)>0)
    
    paste0(unlist(test)[test %notin% names(tree)],collapse = ",")

  })
  
  
  # observeEvent(input$tree, {
  #   selected_nodes <- get_selected(input$tree, format = "names")
  #   checked_nodes <- get_checked(input$tree, format = "names")
  #   
  #   print(selected_nodes)
  #   print(checked_nodes)
  # 
  #   # if (is.null(selected_nodes)) {
  #   #   updateTree(session, "tree", data = tree)
  #   # } else {
  #   #   condensed_tree <- condense_tree(tree, selected_nodes)
  #   #   updateTree(session, "tree", data = condensed_tree)
  #   # }
  # }, ignoreNULL = FALSE)
  # 
  # 
  # 
  # 
  # set_stopened_if_stchecked_false <- function(tree) {
  #   lapply(tree, function(node) {
  #     # If it's a parent node, check if 'stchecked' is FALSE
  #     if (is.list(node$children)) {
  #       # If stchecked is FALSE, set stopened to FALSE
  #       if (node$stchecked == FALSE) {
  #         node$stopened <- FALSE
  #       }
  #       # Recursively call for child nodes
  #       node$children <- set_stopened_if_stchecked_false(node$children)
  #     }
  #     return(node)
  #   })
  # }
  
  # condense_tree <- function(tree_data, selected_nodes) {
  #   if (is.null(selected_nodes) || length(selected_nodes) == 0) {
  #     return(tree_data)
  #   }
  # 
  #   condensed_tree <- list()
  #   for (node_path in selected_nodes) {
  #     path_elements <- strsplit(node_path, "/")[[1]]
  #     condensed_tree <- add_node_recursive(condensed_tree, tree_data, path_elements)
  #   }
  #   return(condensed_tree)
  # }

  # add_node_recursive <- function(condensed_tree, original_tree, path_elements) {
  #   if (length(path_elements) == 0) {
  #     return(original_tree)
  #   }
  # 
  #   node_name <- path_elements[1]
  #   rest_path <- path_elements[-1]
  # 
  #   if (is.null(condensed_tree[[node_name]])) {
  #     condensed_tree[[node_name]] <- list()
  #   }
  # 
  #   condensed_tree[[node_name]] <- add_node_recursive(condensed_tree[[node_name]], original_tree[[node_name]], rest_path)
  #   return(condensed_tree)
  # }
  
  

}

shinyApp(ui, server)
