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
      #print(tree.sel)
      selections <- unlist(tree.sel)[tree.sel %notin% names(input$tree)]
      print(selections)
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
generate_jstree_data <- function(layer,grp = "group") {

  # Helper function to capitalize first letter of each word
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  }
  
  result <- list()
  
  for (table_name in names(layer)) {
    
    layer_info <- layer[[table_name]]
    
    if (!is.null(layer_info)) {
      # Get unique group values directly from the group column
      # if ("group" %in% names(layer_info$layer)) {
      group_values <- #unique(na.omit(as.character(layer_info$layer["group"])))
        unique(na.omit(as.character(pull(st_drop_geometry(layer_info$layer[,grp])))))
      
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
    #}
  }
  
  return(result)
}





# level_data <- unlist(
#   lapply(all_data_layers, function(x) {
#     split(x[["layer"]], x[["layer"]][["group"]])
#   }), 
#   recursive = FALSE)
# names(level_data) <- str_remove_all(names(level_data), ".*?\\.")













# retrieve_tables2 <- function(table_names, resolution_map) {
#   # Initialize an empty list to store retrieved data
#   layers_list <- list()
# 
#   # Establish database connection
#   conn <- DBI::dbConnect(
#     odbc::odbc(),
#     Driver = "ODBC Driver 18 for SQL Server",
#     Server = "SQLIT04C.health.dohmh.nycnet",
#     database = "DIS_DORIS",
#     Trusted_Connection = "yes",
#     Port = 1433
#   )
#   
#   for (table_name in table_names) {
#     resolution <- resolution_map[[table_name]]
#     
#     # Query the table
#     data <- dbGetQuery(conn, paste0("SELECT * FROM doris.", paste0("acs__", table_name, "_summary"))) %>% 
#       rename_with(tolower) %>% 
#       dplyr::filter(year == 2023) %>%
#       filter(denominator_geoid > 0 & !is.na(denominator_geoid)) %>% 
#       group_by(group, 
#                concept, 
#                geoid, 
#                denominator_geoid,
#                denominator_all,
#                geospatial_resolution) %>% 
#       summarize(percent_all = sum(percent_all, na.rm = TRUE)) %>% 
#       # filter(!is.na(year)) %>% 
#       ungroup()
#     
#     # Identify grouping variables (non-numeric columns excluding 'geoid')
#     all_columns <- tolower(names(data))
#     
#     # Ensure 'geoid' is a character for joining
#     data$geoid <- as.character(data$geoid)
#     
#     # Join with the appropriate spatial boundary layer
#     if (resolution == "census_tract") {
#       data <- left_join(ct, data, by = c("CT_2010" = "geoid"))
#     } else if (resolution == "zcta") {
#       data <- left_join(zcta, data, by = c("ZCTA_2010" = "geoid"))
#     } else if (resolution == "nta") {
#       data <- left_join(nta, data, by = c("NTA_2010" = "geoid"))
#     }
#     
#     # Store in layers list with metadata
#     layers_list[[table_name]] <- data
#   }
#   
#   # Close database connection
#   dbDisconnect(conn)
#   
#   return(layers_list)
# }
# 
# 
# 
# all_data_layers <- retrieve_tables2(names(table_resolution_map), table_resolution_map)



