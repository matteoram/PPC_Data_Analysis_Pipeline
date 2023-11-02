# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Author: Johannes Nelson
# Input: User will be prompted for Kobo username and password.
# Outputs: CSV files pertaining to different tables and variables.

# Description: This code forms the data extraction and preprocessing component 
# of PPC's data analysis pipeline. The primary outputs are the "Main_Data" 
# CSV and the combined "Tree_Data" CSV. The 'Main_Data' CSV contains information  
# about each submission/monitoring plot (the responses to the Kobo form questions 
# that are not records of tree species observed or planted), and the 'Tree_Data_' 
# CSV contains information about all the trees observed during monitoring surveys.
# Additional outputs include Geolocation_Data, Photo_Data, and a folder
# that separates out tree data by plot type.
#
# The tree data file contains the word "uncorrected", which refers to the fact that
# the species names have not been cleaned. This will be addressed in a later script.
#
# --------------------------------------------------------------------------------

# Check for and install missing packages; load them into session
necessary_packages <- c("robotoolbox", "dplyr", "dm", "askpass")

for (pkg in necessary_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(pkg, "not found. Installing now...\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


#' 1. Retrieve Kobo Data
#'
#' This function prompts the user for their username and password, and then
#' proceeds to extract data from Kobo with minor wrangling and renaming.
#'
#' @return List of data.frames that correspond to different data tables.
#' 
retrieve_kobo_data <- function(asset_name = "Tree Monitoring") {
  UN <- askpass("Enter Kobo username: ")
  PW <- askpass("Enter Kobo password: ")
  
  tkn <- kobo_token(
    username = UN,
    password = PW,
    url = "kf.kobotoolbox.org"
  )
  
  kobo_settings()
  
  l <- kobo_asset_list()
  
  uid <- l %>%
    filter(name == asset_name) %>%
    pull(uid) %>%
    first()
  
  asset <- kobo_asset(uid)
  
  df <- kobo_data(asset)
  
  main_list <- as.list(df)
  
  # Separate out the main submission data
  main_table <- main_list$main
  
  # This is a dataframe that maps the names in Kobo with more clear, informative
  # names designating what the data represents. If new tables appear, this will
  # need to be edited.
  table_name_mappings <- data.frame(
    original_name = c(
      "main", "begin_repeat_ztsNCjoPm", "begin_repeat_ELDOiv5Dr", "PlantedTrees3",
      "begin_repeat_YDvKUvA32", "begin_repeat_Wq3dUfnDG", "group_an2yk58", 
      "group_ka7vj63", "group_qr1fe53", "group_ai86m63", "group_oq8nt56", 
      "group_ql6qg69", "group_qs7yn71", "group_wj4lz16"
      ),
    clarified_name = c(
      "main", "Normal_30x30", "Nested_3x3_within_30x30", "Planted_30x30",
      "Control_10x10", "Nested_3x3_within_10x10_Control", "Normal_30x30_2", 
      "Nested_3x3_within_30x30_2", "Nested_1x1", "Small_3x3", "Control_10x10_2", 
      "Census_30x30", "Planted_30x30_2", "Nested_3x3_within_10x10_Control_2"
      )
    ) 
  for (i in seq_len(nrow(table_name_mappings))) {
    if (table_name_mappings$original_name[i] %in% names(main_list)) {
      names(main_list)[names(main_list) == table_name_mappings$original_name[i]] <- table_name_mappings$clarified_name[i]
    }
  }
  
  # Separate out tree tables
  tree_tables <- main_list[setdiff(names(main_list), "main")]
  
  # Print table info to check for data structure or naming changes. The number 
  # and table names in this check represent the expected values as of last update.
  # If forms change/new tables appear, this will need to be edited.
  cat(paste0("Number of Expected Tables: 14 \n", "Number of Retrieved Tables: ", length(df), "\n"))
  
  if (all(table_name_mappings$clarified_name %in% names(main_list))) {
    cat("Tables in Kobo data are named as expected.\n")
  } else {
    cat(paste0(
      "Table names are not as expected. This may cause data quality issues or unexpected outputs. \n",
      "Unexpected table name(s): ", names(main_list)[!names(main_list) %in% table_name_mappings$clarified_name], " \n"
    ))
  }
  
  return(list(main_table = main_table, tree_tables = tree_tables))
}



#' 2. Prepare Main Table
#'
#' This function pre-processes the main table by renaming columns, handling NAs
#' within the 'resample' column--the assumption here is that no response meant no
#' resampling occurred--and organizing the columns into a more helpful order. It
#' also prepares and separates out a table for geolocation data and photo 
#' attachment data.
#'
#' @param main_table The original main table extracted from Kobo.
#' @return List of dataframes--Main_Data, Geo_Data, Photo_Data.

process_main_table <- function(main_table) {
  
  # Rename generic _index column to distinct value for later joins.
  colnames(main_table)[colnames(main_table) == "_index"] <- "main_index"
  colnames(main_table)[colnames(main_table) == "Enter_a_date"] <- "Date"
  
  # Rename and process sampling columns; add plot size based on SiteSize answer
  main_table <- main_table %>%
  mutate(Resample_Main_Plot = ifelse(is.na(Resampling1), 0, Resampling1)) %>%
  mutate(Resample_3x3_Subplot = ifelse(is.na(Resampling2), 0, Resampling2)) %>%
  mutate(Monitoring_Plot_Size = ifelse(SiteSize == "Yes", "30x30", "3x3")) %>% 
    select(-Resampling1, -Resampling2)
  
  # Separate out geolocation data based on pattern in column names
  geo_columns <- names(main_table)[grep("Corner|Centroid", names(main_table), ignore.case = TRUE)]
  geo_data <- main_table %>% 
    select(Organization_Name, Site_ID, Plot_ID,
           SiteType, Plot_Type, Coordinate_System_Used, 
           all_of(geo_columns)) %>% 
    select(-names(.)[grep("Photo", names(.), ignore.case = TRUE)])
  
  
  # Separate out photo attachment data and join with relevant plot data
  photo_attachments <- main_table$`_attachments`
  full_attachments_df <- bind_rows(photo_attachments)
  final_attachments <- left_join(
    select(
      main_table, 
      Organization_Name,
      Site_ID, 
      Plot_ID,
      `_id`),
    full_attachments_df,
    by = c("_id" = "instance")
    )
    


  # Simple reordering for ease of viewing; drops all geolocation and photo columns
  main_table_shortened <- main_table %>%
  select(
    Plot_ID,
    Site_ID,
    SiteType,
    Country,
    Organization_Name,
    Plot_Permanence,
    Monitoring_Plot_Size,
    Strata_Number,
    Timeframe,
    Date,
    Resample_Main_Plot,
    Resample_3x3_Subplot,
    everything(),
    -all_of(geo_columns),
    -`_attachments`
  )
  

return(list(Main_Data = main_table_shortened, Geo_Data = geo_data, Photo_Data = final_attachments)) 
}



#' 3. Extract Misplaced Data
#'
#' For mysterious reasons that no mortal mind can comprehend, some of the tree
#' data was output into the main data table with submission data. The columns 
#' containing this data have patterns '1', '2', '0011', and '0012' in their names.
#' These patterns are used to find the misplaced data. This lengthy function's
#' sole purpose is to grab this data and put it where it belongs based on hard-coded, 
#' clunky rules. If there ever appear new data in the main table that do not have
#' the above mentioned patterns,it will need to be examined and code will need to 
#' be added to this function to accommodate it.
#'
#' @param main_table The main table after it has undergone the above preprocessing.
#' @param tree_tables The tree tables straight from extraction.
#' @return List with a dataframe called main_table and a list of dataframes called
#' tree tables.


extract_misplaced_data <- function(main_table, tree_tables) {

  tree_table_names <- names(tree_tables)
  
  # This renames all '_index' columns to 'tree_index', and all '_parent_index'
  # columns to 'main_index' to be able to match the entries in the main table.
  tree_tables <- lapply(tree_tables, function(df) {
    col_names <- names(df)
    col_names[col_names == "_index"] <- "tree_index"
    col_names[col_names == "_parent_index"] <- "main_index"
    names(df) <- col_names
    return(df)
  })
  
  # Select columns in main table where there is tree data based on known patterns, 
  # group by data type, and append to appropriate tree table. Destination tables
  # were chosen based on patterns in the 'Kobo_Key' document.
  misplaced_tree_data <- main_table %>% 
    filter(
      !is.na(Tree_Species1) | 
        !is.na(Tree_Species2) | 
        !is.na(Tree_Species_0011) | 
        !is.na(Tree_Species_0012)
    ) %>%
    select(
      main_index,
      contains("Number_of_Trees"),
      contains("Tree_Type"),
      contains("Tree_Species")
    ) %>% 
    mutate(
      origin_table = "main"
    )
  
  df_type_1 <- misplaced_tree_data %>%
    select(main_index, origin_table, ends_with("1")) %>%
    select(-ends_with("0011")) %>% 
    filter(
      !is.na(Tree_Species1) | 
        !is.na(Tree_Type1) | 
        !is.na(Number_of_Trees_of_this_Species1)) %>% 
    mutate(
      destination_table = "Control_10x10"
    )

  
  df_type_2 <- misplaced_tree_data %>%
    select(main_index, origin_table, ends_with("2")) %>%
    select(-ends_with("0012")) %>% 
    filter(
      !is.na(Tree_Species2) | 
        !is.na(Tree_Type2) | 
        !is.na(Number_of_Trees_of_this_Species2)) %>% 
    mutate(
      destination_table = "Normal_30x30"
    )
  
  
  df_type_0011 <- misplaced_tree_data %>%
    select(main_index, origin_table, ends_with("0011")) %>% 
    filter(
      !is.na(Tree_Species_0011) | 
        !is.na(Tree_Type_0011) | 
        !is.na(Number_of_Trees_of_this_Species_0011)) %>% 
    mutate(
      destination_table = "Nested_3x3_within_10x10_Control"
    )
  
  
  df_type_0012 <- misplaced_tree_data %>%
    select(main_index, origin_table, ends_with("0012")) %>% 
    filter(
      !is.na(Tree_Species_0012) | 
        !is.na(Tree_Type_0012) | 
        !is.na(Number_of_Trees_of_this_Species_0012)) %>% 
    mutate(
      destination_table = "Nested_3x3_within_30x30"
    )
  

  # Helper function to bind data to correct tree table
  bind_to_tree_table <- function(df, tree_tables) {
    destination <- unique(df$destination_table)

    corresponding_tree_table <- tree_tables[[destination]]
    
    updated_table <- bind_rows(corresponding_tree_table, df)

    return(updated_table)
  }
  
  # Call the binding function for each table, putting the data in correct place
  tree_tables$Control_10x10 <- bind_to_tree_table(df_type_1, tree_tables)
  tree_tables$Normal_30x30 <- bind_to_tree_table(df_type_2, tree_tables)
  tree_tables$Nested_3x3_within_10x10_Control <- bind_to_tree_table(df_type_0011, tree_tables)
  tree_tables$Nested_3x3_within_30x30 <- bind_to_tree_table(df_type_0012, tree_tables)
  
  # Remove data from main table after it has been put in place
  main_table <- main_table %>%
    select(
      -contains("Tree_Species"),
      -contains("Number_of_Trees"),
      -contains("Tree_Type")
    )
  return(list(main_table = main_table, tree_tables = tree_tables))
}



#' 4. Clean Tree Tables
#'
#' This function renames columns based on known naming conventions so that the 
#' combined data has common variable names, adds columns and values for Plot_Size 
#' and origin_table, and joins the tree data with the main data so that information
#' like Plot_ID, Site_ID, etc. becomes available in the tree data as well.
#'
#' @param tree_tables The tree_tables after they have been 'repaired' by the above
#' extract_misplaced_data function.
#' @param main_table The preprocessed and repaired main table, output by previous 
#' function
#'
#' @return List with: a dataframe of all tree data combined, as well as a nested 
#' list with tree data still separated by origin table. 

clean_tree_tables <- function(tree_tables, main_table) {
  # Extract table names to iterate over
  tree_table_names <- names(tree_tables)
  
  
  tree_tables_modified <- lapply(tree_table_names, function(name) {
    # Extract single table from list
    df <- tree_tables[[name]]
    
    # Check column names for certain patterns and standardize across tables
    names(df)[grep("number|numer", names(df), ignore.case = TRUE)] <- "Tree_Count"
    names(df)[grep("species", names(df), ignore.case = TRUE)] <- "Species"
    names(df)[grep("type", names(df), ignore.case = TRUE)] <- "Tree_Type"

    # Extract plot dimensions from the table name and add Plot_Size column. Assumes
    # table names do not change. Unexpected new tables may cause unwanted behavior.
    plot_dims <- strsplit(name, "_")[[1]][2]
    df$Plot_Size <- plot_dims
    
    # Record origin table for future reference
    df$origin_table <- name
    
    
    # Checks the tables with planted tree data. These need Tree_Type columns
    # that can automatically be populated with 'planted'
    if (grepl("planted", name, ignore.case = TRUE) && !"Tree_Type" %in% names(df)) {
      df$Tree_Type <- "planted"
    }
  

    # This joins the tree data with essential elements in the main data that will
    # be used in down stream analyses.
    df <- df %>%
      left_join(
        select(
          main_table,
          main_index,
          Plot_ID,
          Organization_Name,
          Country,
          Site_ID,
          SiteType,
          Plot_Permanence,
          Resample_Main_Plot,
          Resample_3x3_Subplot,
          PlantingPattern,
          Timeframe
        ),
        by = "main_index"
      )
    
    # Convert all plot and site IDs to character values.
    df$Plot_ID <- as.character(df$Plot_ID)
    df$Site_ID <- as.character(df$Site_ID)
    
    # Reordering columns -- relevant first. Can easily be customized.
    df <- df %>% select(
      Species,
      Tree_Type,
      Tree_Count,
      Site_ID,
      Plot_ID,
      Country,
      Organization_Name,
      SiteType,
      Plot_Size,
      everything()
    )


    return(df)
  })
  names(tree_tables_modified) <- tree_table_names
  
  combined_tree_tables <- bind_rows(tree_tables_modified)
  
  return(list(Tree_Tables = tree_tables_modified, Full_Tree_Data = combined_tree_tables))
}



#' 5. Remove Columns with Only NAs
#'
#' This function processes a list of tables and removes any columns within
#' these tables that contain only NA values. If this is not desired, exclude
#' this function from main script below. The assumption here is that if every
#' value is NA, it is likely not helpful.
#'
#' @param tables_list A list of tables (dataframes) from which columns containing
#' only NAs should be removed.
#'
#' @return A list of cleaned tables with columns containing only NAs removed.

remove_NA_columns <- function(tables_list) {
  cleaned_tables <- lapply(tables_list, function(df) {
    df <- df %>% select_if(~ !all(is.na(.)))
    return(df)
  })
  return(cleaned_tables)
}



#' 6. Write CSVs to disk
#'
#' These are some simple helper functions to make writing lists of dataframes and
#' single dataframes to the disk. They add date stamps and assume no 
#' sub directory by default. 

write_to_csv <- function(data, prefix, date_stamp = TRUE, sub_dir = NULL) {
  main_dir <- "Main_Raw_Data"
  
  # Check if the main directory exists, if not, create it
  if (!dir.exists(main_dir)) {
    dir.create(main_dir)
  }
  
  # If a subdirectory is provided, ensure it's created
  if (!is.null(sub_dir)) {
    sub_path <- file.path(main_dir, sub_dir)
    if (!dir.exists(sub_path)) {
      dir.create(sub_path)
    }
    path_prefix <- file.path(sub_path, prefix)
  } else {
    path_prefix <- file.path(main_dir, prefix)
  }
  
  # Determine filename with optional date stamp
  if (date_stamp) {
    current_date <- format(Sys.Date(), "%Y-%m-%d") # e.g., "2023-10-10"
    filename <- paste0(path_prefix, "_", current_date, ".csv")
  } else {
    filename <- paste0(path_prefix, ".csv")
  }
  
  # Write to file and print message
  write.csv(data, filename, row.names = FALSE)
  cat(paste("Data written to:", filename), "\n")
}


write_list_to_csv <- function(data_list, prefix_list, date_stamp = TRUE, sub_dir = NULL) {
  if (length(data_list) != length(prefix_list)) {
    stop("The number of data items does not match the number of prefixes.")
  }
  
  for (i in seq_along(data_list)) {
    write_to_csv(data_list[[i]], prefix_list[i], date_stamp, sub_dir)
  }
}


#---------------------------------------------------------------------------------
# The above script defines all these functions. The 'main' script below calls them
# each in turn. By having distinct modules, errors/bugs that might arise in the 
# future will be easier to diagnose. The print() statements output at each step
# in the console can help locate where things went wrong, and the relevant function
# above will be a good starting point for debugging. 
#---------------------------------------------------------------------------------

# 1. Retrieve Kobo Data
print("Retrieving data from KoboToolbox. This requires internet connection and may take a moment.")
all_data <- retrieve_kobo_data() # This will prompt the user for username and password.

# 2. Prepare Main Table
print("Processing Main Table, Extracting Geolocation and Photo Data")
main_geo_photo <- process_main_table(all_data$main_table)

# 3. Extract misplaced data
print("Extracting misplaced tree data from main data and putting it in correct place.")
all_data_fixed <- extract_misplaced_data(main_table = main_geo_photo$Main_Data, tree_tables = all_data$tree_tables)

# 4. Clean Tree Tables
print("Cleaning tree tables.")
cleaned_tree_tables <- clean_tree_tables(all_data_fixed$tree_tables, all_data_fixed$main_table)

# 5. Remove columns that are entirely NA (optional)
print("Removing columns that are entirely NA.")
final_tree_tables <- remove_NA_columns(cleaned_tree_tables$Tree_Tables)
final_full_tree_table <- remove_NA_columns(list(cleaned_tree_tables$Full_Tree_Data))[[1]]
final_main_table <- remove_NA_columns(list(all_data_fixed$main_table))[[1]]

# 8. Write Data to Disk
print("Writing data to disk.")
write_to_csv(final_main_table, "Main_Data")
write_list_to_csv(final_tree_tables, names(final_tree_tables), sub_dir = "Tree_Data_by_PlotType")
write_to_csv(final_full_tree_table, "Tree_Data_Uncorrected")
write_list_to_csv(main_geo_photo[2:3], names(main_geo_photo[2:3]))


cat("Data processing and export complete!\n")
