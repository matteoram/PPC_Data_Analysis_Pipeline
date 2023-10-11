library(robotoolbox)
library(dplyr)
library(dm)


asset_name <- "PPC/PACTO Tree Monitoring - Brazil only"

retrieve_kobo_data <- function(asset_name) {
  
  # Authenticate and set up KoboToolbox connection
  UN <- readline(prompt="Enter Kobo username: ")
  PW <- readline(prompt="Enter Kobo password: ")
  
  tkn <- kobo_token(username = UN,
                    password = PW,
                    url = "kf.kobotoolbox.org")
  
  kobo_settings()
  l <- kobo_asset_list()
  
  # Extract UID based on asset name
  uid <- l %>%
    filter(name == asset_name) %>%
    pull(uid) %>%
    first()
  
  # Get asset and data
  asset <- kobo_asset(uid)
  df <- kobo_data(asset, all_versions = T)
  
  # Convert to list for easier handling
  main_list <- as.list(df)
  
  # Extract main table and other tables
  main_table <- main_list$main
  tree_tables <- main_list[names(main_list)[grep(c("x"), x = names(main_list))]]
  DBH_tables <- main_list[names(main_list)[grep(c("group"), x = names(main_list))]]
  
  # Return the tables as a list
  return(list(main_table = main_table, tree_tables = tree_tables, DBH_tables = DBH_tables))
}


prep_main_table <- function(main_table){
  
  colnames(main_table)[colnames(main_table) == "_index"] <- "main_index"
  main_table <- main_table %>% select(Plot_Info_Plot_ID,
                                      Site_Info_Site_ID, 
                                      Site_Info_SiteType,
                                      General_Info_Country,
                                      General_Info_Organization_Name,
                                      Plot_Info_Plot_Permanence,
                                      Plot_Info_Strata,
                                      everything())
  
  main_table <- main_table %>% select(-General_Info_Note, 
                                      -General_Info_Note1, 
                                      -General_Info_Diagram, 
                                      General_Info_Note, 
                                      General_Info_Note1, 
                                      General_Info_Diagram)
  
}



clean_tree_tables <- function(tree_tables, main_table) {
  
  # Extract table names
  table_names <- names(tree_tables)
  tree_table_names <- table_names[grep(c("x"), x = table_names)]
  
  # Cleaning function
  tree_tables_modified <- lapply(tree_table_names, function(name) {
    # Extract the desired string from the name (plot dimensions)
    plot_dims <- strsplit(name, "_")[[1]][2]
    
    # Retrieve the dataframe from the tree_tables list
    df <- tree_tables[[name]]
    
    # Add  new columns to the dataframe
    df$Plot_Type <- plot_dims
    df$origin_table <- name
    
    
    # Check column names for the pattern "species" and rename to "Species"; check 
    # for "TreeType" and rename "Tree_Type"
    col_names <- names(df)
    col_names[grep("species|Species", col_names, ignore.case = TRUE)] <- "Species"
    col_names[grep("TreeType", col_names, ignore.case = TRUE)] <- "Tree_Type"
    col_names[grep("Count", col_names, ignore.case = TRUE)] <- "Tree_Count"
    col_names[col_names == "_index"] <- "tree_index"
    col_names[col_names == "_parent_index"] <- "main_index"
    names(df) <- col_names
    
    # Remove columns with the patterns "001" and "diagram"
    df <- df[, !grepl("001", names(df))]
    df <- df[, !grepl("diagram", names(df))]
    
    # If "Tree_Type" column doesn't exist, add it and populate with "planted." This
    # handles one of the tables which has planted in its title, but not in its columns
    if (!"Tree_Type" %in% names(df)) {
      df$Tree_Type <- "planted"
    }
    
    df <- df %>%
      left_join(select(main_table, 
                       main_index, 
                       Plot_Info_Plot_ID, 
                       Site_Info_Site_ID, 
                       Site_Info_SiteType), 
                by = "main_index")
    
    df <- df %>% select(Species, 
                        Tree_Type, 
                        Plot_Info_Plot_ID, 
                        Site_Info_Site_ID,
                        Site_Info_SiteType,
                        Plot_Type, 
                        everything())
    
    
    return(df)
  })
  names(tree_tables_modified) <- tree_table_names
  return(tree_tables_modified)
}



