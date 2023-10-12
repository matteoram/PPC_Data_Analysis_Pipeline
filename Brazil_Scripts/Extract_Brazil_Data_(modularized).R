library(robotoolbox)
library(dplyr)
library(dm)
library(askpass)


asset_name <- "PPC/PACTO Tree Monitoring - Brazil only"


retrieve_kobo_data <- function(asset_name) {
  
  # Authenticate and set up KoboToolbox connection
  UN <- askpass("Enter Kobo username: ")
  PW <- askpass("Enter Kobo password: ")
  
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
                                      everything(),
                                      -`_attachments`)
  
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



clean_DBH_tables <- function(DBH_tables, tree_tables) {
  
  # Extract table names
  DBH_table_names <- names(DBH_tables)
  
  # Cleaning function
  DBH_tables_modified <- lapply(DBH_table_names, function(name) {
    
    # Retrieve the dataframe from the DBH_table_names list
    df <- DBH_tables[[name]]
    df$origin_table <- name
    col_names <- names(df)
    
    col_names[col_names == "_index"] <- "trunk_index"
    col_names[col_names == "_parent_index"] <- "tree_index"
    names(df) <- col_names
    
    df <- df[, !grepl("button", names(df))]
    
    parent_table_name <- unique(df$`_parent_table_name`)
    parent_table <- tree_tables[[parent_table_name]]
    
    df <- df %>%
      left_join(select(parent_table, 
                       tree_index, 
                       Species, 
                       Tree_Type, 
                       Plot_Info_Plot_ID, 
                       Site_Info_Site_ID,
                       Site_Info_SiteType,
                       Plot_Type), 
                by = "tree_index")
    
    return(df)
  })
  
  # Rename tables and return
  names(DBH_tables_modified) <- DBH_table_names
  return(DBH_tables_modified)
}




adjust_DBH_tables <- function(DBH_tables, tree_tables) {
  
  # Extract specific data from tree_tables
  new_form_data <- tree_tables$`_30x30_Plot_Repeat`[!is.na(tree_tables$`_30x30_Plot_Repeat`$`_30x30_Plot_TreeIDNumber`),]
  
  # Merge and adjust the first table of DBH_tables
  DBH_tables[[1]] <- bind_rows(DBH_tables[[1]], new_form_data)
  DBH_tables[[1]]$main_index <- NULL
  DBH_tables[[1]]$trunk_index <- c(1:nrow(DBH_tables[[1]]))
  
  names(DBH_tables) <- c("DBH_30x30", "DBH_30x15")
  
  return(DBH_tables)
}


remove_NA_columns <- function(tables_list) {
  cleaned_tables <- lapply(tables_list, function(df) {
    df <- df %>% select_if(~ !all(is.na(.)))
    return(df)
  })
  return(cleaned_tables)
}



combine_tree_tables <- function(tree_tables_list) {
  combined_tree_tables <- bind_rows(tree_tables_list)
  
  # Remove specific columns and adjust Tree_Count
  combined_tree_tables <- combined_tree_tables %>% 
    select(-tree_index, -`_30x30_Plot_TreeIDNumber`, 
           -`_30X30_Plot_TreeTrunk`,  -`_30x30_Plot_TreeDBH`) %>% 
    mutate(Tree_Count = ifelse(grepl("_30x30_Plot_Repeat|_30x15_Plot_Repeat", origin_table) & is.na(Tree_Count), 1, Tree_Count))
  
  # Group by Plot_Info_Plot_ID and Species, then summarize
  combined_tree_tables <- combined_tree_tables %>%
    group_by(Plot_Info_Plot_ID, Species) %>%
    summarise(
      Tree_Count = sum(Tree_Count),
      # Retain the first value for all other columns
      across(everything(), first),
      .groups = 'drop'
    )
  
  return(combined_tree_tables)
}




write_to_csv <- function(data, prefix, date_stamp = TRUE) {
  if (date_stamp) {
    current_date <- format(Sys.Date(), "%Y-%m-%d") # e.g., "20231010"
    filename <- paste0(prefix, "_", current_date, ".csv")
  } else {
    filename <- paste0(prefix, ".csv")
  }
  
  write.csv(data, filename, row.names = FALSE)
  cat(paste("Data written to:", filename), "\n")
}



write_list_to_csv <- function(data_list, prefix_list, date_stamp = TRUE) {
  if (length(data_list) != length(prefix_list)) {
    stop("The number of data items does not match the number of prefixes.")
  }
  
  for (i in seq_along(data_list)) {
    write_to_csv(data_list[[i]], prefix_list[i], date_stamp)
  }
}



print("Retrieving data from KoboToolbox. This requires internet connection and may take a moment.")
all_data <- retrieve_kobo_data(asset_name) # This will prompt the user for username and password.
print("Data Retrieved successfully!")

print("Cleaning and transforming Data")
# 2. Prepare Main Table
prepared_main_table <- prep_main_table(all_data$main_table)

# 3. Clean Tree Tables
cleaned_tree_tables <- clean_tree_tables(all_data$tree_tables, prepared_main_table)

# 4. Clean DBH Tables
cleaned_DBH_tables <- clean_DBH_tables(all_data$DBH_tables, cleaned_tree_tables)

# 5. Adjust DBH Tables
adjusted_DBH_tables <- adjust_DBH_tables(cleaned_DBH_tables, cleaned_tree_tables)

# 6. Remove NA columns
final_DBH_tables <- remove_NA_columns(tables_list = adjusted_DBH_tables)
final_tree_tables <- remove_NA_columns(tables_list = cleaned_tree_tables)

# 7. Combine Tree Tables
final_combined_tree_tables <- combine_tree_tables(final_tree_tables)
print("Preprocessing complete!")

print("Writing data to disk.")
# 8. Write Data to Disk
write_to_csv(prepared_main_table, "Main_Data")
write_list_to_csv(final_DBH_tables, names(adjusted_DBH_tables))
write_list_to_csv(final_tree_tables, names(cleaned_tree_tables))
write_to_csv(final_combined_tree_tables, "Tree_Data_full")

# Optionally, print a message to let the user know the process is complete:
cat("Data processing and export complete!\n")
