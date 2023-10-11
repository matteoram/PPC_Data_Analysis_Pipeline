library(robotoolbox)
library(dplyr)
library(dm)



tkn=kobo_token(username = "johannes_nelson",
               password = "Timbnpfkt2295",
               url = "kf.kobotoolbox.org")

kobo_settings()
l <- kobo_asset_list()

uid <- l |>
  filter(name == "PPC/PACTO Tree Monitoring - Brazil only") |>
  pull(uid) |>
  first()

# uid <- l |>
#   filter(name == "Tree Monitoring") |>
#   pull(uid) |>
#   first()

asset <- kobo_asset(uid)

df <- kobo_data(asset, all_versions = T)
main_list <- as.list(df)

# table_names <- names(df)
# 
# 
# # Not necessary script, this is mostly for easy exploring
# for (name in table_names) {
#   tbl_x <- df[name][[1]]
#   assign(paste(name, "tbl", sep = "_"), value = tbl_x, envir = globalenv())
# }
# 
# Kobo extracts as a special object called "dm". To make it easier to handle, I 
# am converting to a list here. 


# Create a variable with all table names
table_names <- names(main_list)

# Pulling out the "tree" tables, by searching for the character 'x' (because it
# appears in all references to plot sizes)
tree_table_names <- table_names[grep(c("x"), x = table_names)]
DBH_table_names <- table_names[grep(c("group"), x = table_names)]
main_table_name <- "main" # This is just for consistency--a variable for each set

# Create separate variables for main data and tree data (since all the tree data
# refers to the main table by a _parent_index key.
main_table <- main_list$main
tree_tables <- main_list[tree_table_names]
DBH_tables <- main_list[DBH_table_names]



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



# The main purpose of this function is to add the plot_type column, which uses 
# the table name to extract the plot dimensions (30x15, 30x30, etc.), which it
# takes from the string split. On top of that it also adds a column for the
# table it originated from (just in case), while also cleaning up some labels
# so they are consistent in the new, larger table. 

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

tree_tables <- setNames(tree_tables_modified, tree_table_names)



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

DBH_tables <- setNames(DBH_tables_modified, DBH_table_names)
names(DBH_tables) <- c("DBH_30x30", "DBH_30x15")



new_form_data <- tree_tables$`_30x30_Plot_Repeat`[!is.na(tree_tables$`_30x30_Plot_Repeat`$`_30x30_Plot_TreeIDNumber`),]
DBH_tables[[1]] <- bind_rows(DBH_tables[[1]], new_form_data)
DBH_tables[[1]]$main_index <- NULL
DBH_tables[[1]]$trunk_index <- c(1:nrow(DBH_tables[[1]]))



DBH_tables <- lapply(DBH_tables, function(df) {
  df <- df %>% select_if(~ !all(is.na(.)))
  return(df)
})

tree_tables <- lapply(tree_tables, function(df) {
  df <- df %>% select_if(~ !all(is.na(.)))
  return(df)
})


combined_tree_tables <- bind_rows(tree_tables)

combined_tree_tables <- combined_tree_tables %>% 
  select(-tree_index, -`_30x30_Plot_TreeIDNumber`, 
         -`_30X30_Plot_TreeTrunk`,  -`_30x30_Plot_TreeDBH`) %>% 
  mutate(Tree_Count = ifelse(is.na(Tree_Count), 1, Tree_Count))

combined_tree_tables <- combined_tree_tables %>%
  group_by(Plot_Info_Plot_ID, Species) %>%
  summarise(
    Tree_Count = sum(Tree_Count),
    # Retain the first value for all other columns
    across(everything(), first),
    .groups = 'drop'
  )


current_date <- format(Sys.Date(), "%Y-%m-%d") # e.g., "20231010"

# Write each table in DBH_tables to a CSV
lapply(names(DBH_tables), function(name) {
  filename <- paste0(name, "_", current_date, ".csv")
  write.csv(DBH_tables[[name]], filename, row.names = FALSE)
})

# Write each table in tree_tables to a CSV
lapply(names(tree_tables), function(name) {
  filename <- paste0("Tree_Data",name, "_", current_date, ".csv")
  write.csv(tree_tables[[name]], filename, row.names = FALSE)
})


main_table_no_attachments <- main_table %>% select(-`_attachments`)
main_filename <- paste0("Main_Data", "_", current_date, ".csv")
write.csv(main_table_no_attachments, main_filename, row.names = FALSE)

tree_filename <- paste0("Tree_Data_full", "_", current_date, ".csv")
write.csv(combined_tree_tables, tree_filename, row.names = FALSE)


