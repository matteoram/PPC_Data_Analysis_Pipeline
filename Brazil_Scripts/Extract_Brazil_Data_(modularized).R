# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Author: Johannes Nelson
# Date Updated: October 18, 2023
# Input: User will be prompted for Kobo username and password.
# Outputs: Eight .csv files pertaining to different tables and variables

# Description: This code forms the extraction part of PPC's data analysis pipeline
# for the Brazil data, which was collected differently than the remainder of the
# PPC projects and so required separate code to preprocess the raw Kobo data. The 
# two primary outputs are the "Main" table and the combined "Tree" table. The main
# table contains information about each plot, and the tree table contains information
# about all the trees monitored along with corresponding variables to identify plot,
# site, organization, timeframe, planting pattern, etc. 
# 
# The tree data file contains the word "uncorrected", which refers to the fact that
# the species names have not been cleaned. The next script addresses this.
# 
# --------------------------------------------------------------------------------

# Check and install missing packages
necessary_packages <- c("robotoolbox", "dplyr", "dm", "askpass")

for (pkg in necessary_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(pkg, "not found. Installing now...\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# This asset name is how the Brazil data is stored in Kobo.
asset_name <- "PPC/PACTO Tree Monitoring - Brazil only"


#' 1. Retrieve Kobo Data
#'
#' This function prompts the user for their username and password, and then 
#' proceeds to extract data from Kobo with minor wrangling and renaming.
#'
#' @return List of data.frames that correspond to different data tables.
retrieve_kobo_data <- function(asset_name) {
  
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

  main_table <- main_list$main
  
  # This renames tables based on patterns in the way data was labelled in Koobo
  tree_tables <- main_list[names(main_list)[grep(c("x"), x = names(main_list))]]
  DBH_tables <- main_list[names(main_list)[grep(c("group"), x = names(main_list))]]

  # Print table info to check for data structure or naming changes
  cat(paste0("Number of Expected Tables: 8 \n", "Number of Retrieved Tables: ", length(df), "\n"))
  expected_names <- c("main", "_30x30_Plot_Repeat", "group_un3bb19", 
                      "_3x3_Subplot_Repeat", "_30x30_Plot_Repeat_Planted_10cm", 
                      "_30x15_Plot_Repeat","group_dz1zn48", "Plot_Info_Repeat" )
  
  if (all(expected_names == names(df))) {
    cat("Tables in Kobo data are named as expected.\n")
  }else {
      cat(paste0("Table names are not as expected. This may cause data quality issues or unexpected outputs. \n", 
                   "Unexpected table name(s): ", names(df)[!names(df) %in% expected_names], " \n"))
    }
  
  return(list(main_table = main_table, tree_tables = tree_tables, DBH_tables = DBH_tables))
}



#' 2. Prepare Main Table
#'
#' This function pre-processes the main table by renaming columns, handling NAs,
#' and organizing the columns in a more logical order.
#'
#' @param main_table The original main table to be preprocessed.
#' @return Dataframe with cleaned and organized columns.

prep_main_table <- function(main_table) {
  
  if("Notes" %in% names(main_table)) {
    names(main_table)[names(main_table) == "Notes"] <- "Original_Notes"
  }
  # Remove lengthy prefixes
  rename_columns <- function(x) {
    x <- gsub("Plot_Info_", "", x)
    x <- gsub("Site_Info_", "", x)
    x <- gsub("General_Info_", "", x)
    return(x)
  }
  names(main_table) <- sapply(names(main_table), rename_columns)
  
  # Ensure "_index" is renamed to "main_index"
  colnames(main_table)[colnames(main_table) == "_index"] <- "main_index"
  
  # Address NAs for Resample values. This assumes an NA was a 0.
  main_table <- main_table %>% 
    mutate(Resample_Main_Plot = ifelse(is.na(Resampling), 0, Resampling)) %>% 
    mutate(Resample_3x3_Subplot = ifelse(is.na(Resample_3x3_Subplot), 0, Resample_3x3_Subplot))
  
  # Organize into more helpful order, remove 'attachments'. Note: these attachments
  # are links to photo downloads. If desired in output, script can be added.
  main_table <- main_table %>% 
    select(
      Plot_ID,
      Site_ID,
      SiteType,
      Country,
      Organization_Name,
      Plot_Permanence,
      Strata,
      Resample_Main_Plot,
      Resample_3x3_Subplot,
      everything(),
      -`_attachments`,
      Note,
      Note1,
      Diagram
    )
  
  return(main_table)  # Make sure to return the modified table
}

#' 3. Clean Tree Tables
#'
#' This function renames columns based on naming conventions so that the combined 
#' data has common variable names, adds columns and values for Plot_Type and 
#' origin_table, and joins the tree data with the main data so that information
#' like Plot_ID, Site_ID, etc. becomes available in the tree data as well. 
#'
#' @param tree_tables The unprocessed tree tables from the main data list
#' @param main_table The preprocessed main table.
#' 
#' @return a list of tree tables that have been preprocessed

clean_tree_tables <- function(tree_tables, main_table) {
  # Extract table names
  tree_table_names <- names(tree_tables)
  # tree_table_names <- table_names[grep(c("x"), x = table_names)]

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
    # handles one of the tables which has planted in its title, but not in its columns.
    if (!"Tree_Type" %in% names(df)) {
      df$Tree_Type <- "planted"
    }

    df <- df %>%
      left_join(
        select(
          main_table,
          main_index,
          Plot_ID,
          Organization_Name,
          Site_ID,
          SiteType,
          Plot_Permanence,
          Resample_Main_Plot,
          Resample_3x3_Subplot,
          Restoration_Technique,
          Timeframe
        ),
        by = "main_index"
      )

    df <- df %>% select(
      Species,
      Tree_Type,
      Plot_ID,
      Site_ID,
      SiteType,
      Plot_Type,
      everything()
    )


    return(df)
  })
  names(tree_tables_modified) <- tree_table_names
  return(tree_tables_modified)
}



#' 4. Clean Diameter at Breast Height (DBH) Tables
#'
#' This function processes the DBH tables by renaming specific columns for consistency, 
#' removing unnecessary columns, and merging the DBH data with the tree data. This 
#' ensures that the combined data includes critical information such as Species, Tree_Type, 
#' Plot_ID, Site_ID, and other essential attributes. The function also introduces an 
#' origin_table column to identify the source table for each entry.
#'
#' @param DBH_tables A list of unprocessed DBH tables.
#' @param tree_tables The preprocessed tree tables, used to provide context and 
#' additional information for the DBH tables.
#' 
#' @return A list of cleaned and merged DBH tables.


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
      left_join(
        select(
          parent_table,
          tree_index,
          main_index,
          Species,
          Tree_Type,
          Plot_ID,
          Site_ID,
          SiteType,
          Plot_Type,
          Organization_Name,
          Plot_Permanence,
          Resample_Main_Plot,
          Resample_3x3_Subplot,
          Restoration_Technique, 
          Timeframe
        ),
        by = "tree_index"
      )

    return(df)
  })

  names(DBH_tables_modified) <- DBH_table_names
  return(DBH_tables_modified)
}


#' 5. Adjust Diameter at Breast Height (DBH) Tables
#'
#' This function refines the DBH tables and handles an inconsistency in the data
#' coming from Kobo, where certain DBH measurements were not in the expected tables. 
#' It extracts these misplaced data from the tree tables and binds them to the 
#' relevant DBH table. Currently, this is a one-time issue, and the below function
#' assumes this will not continue to happen.
#'
#' @param DBH_tables A list of DBH tables that need adjustments.
#' @param tree_tables A list of tree tables, from which specific data will be extracted 
#' and appended to the DBH tables.
#'
#' @return A list of adjusted DBH tables.


adjust_DBH_tables <- function(DBH_tables, tree_tables) {
  # Extract specific trunk data from tree_tables that should not be there.
  new_form_data <- tree_tables$`_30x30_Plot_Repeat`[!is.na(tree_tables$`_30x30_Plot_Repeat`$`_30x30_Plot_TreeIDNumber`), ]

  # Merge and adjust tables
  DBH_tables[[1]] <- bind_rows(DBH_tables[[1]], new_form_data)
  DBH_tables[[1]]$trunk_index <- c(1:nrow(DBH_tables[[1]]))

  names(DBH_tables) <- c("DBH_30x30", "DBH_30x15")

  return(DBH_tables)
}



find_problem_rows_30x30 <- function(DBH_table) {
    df <- DBH_table
    problematic_tree_indices <- df %>%
      group_by(Plot_ID, Timeframe, `_30x30_Plot_TreeIDNumber`) %>%
      filter(sum(`_30X30_Plot_TreeTrunk` == 1) > 1) %>%
      pull(tree_index) %>%
      unique()
    
    # Filtering the problematic rows from the original table
    problematic_rows <- df %>%
      filter(tree_index %in% problematic_tree_indices)
    
    return(problematic_rows)
  }
  


find_problem_rows_30x15 <- function(DBH_table) {
  df <- DBH_table
  problematic_tree_indices <- df %>%
    group_by(Plot_ID, Timeframe, `_30x15_Plot_TreeIDNumber`) %>%
    filter(sum(`_30X15_Plot_TreeTrunk` == 1) > 1) %>%
    pull(tree_index) %>%
    unique()
  
  # Filtering the problematic rows from the original table
  problematic_rows <- df %>%
    filter(tree_index %in% problematic_tree_indices)
  
  return(problematic_rows)
}


#' 6. Remove Columns with Only NAs
#'
#' This function processes a list of tables and removes any columns within 
#' these tables that contain only NA values. If this is not desired, exclude
#' this function from main script below.
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




#' 7. Combine and Refine Tree Tables
#'
#' This function consolidates a list of tree tables into a single table. During the 
#' combination process, the function distinguishes rows based on the presence or 
#' absence of NA values in the "_30x30_Plot_TreeIDNumber" column. For rows without NA 
#' values in this column, duplicates are removed. The function then reintegrates these 
#' treated rows with those containing NA values. Additional refinements are made to 
#' the Tree_Count values and the data is grouped by Plot_ID and Species for summarization.
#'
#' @param tree_tables_list A list of tree tables that need to be combined and refined.
#'
#' @return A single consolidated and refined tree table.
combine_tree_tables <- function(tree_tables_list) {
  combined_tree_tables <- bind_rows(tree_tables_list)
  
  # Separate dataframes: one with non-NA values and one with NA values for the column
  df_non_na <- combined_tree_tables %>% filter(!is.na(`_30x30_Plot_TreeIDNumber`))
  df_na <- combined_tree_tables %>% filter(is.na(`_30x30_Plot_TreeIDNumber`))
  
  # Remove duplicates only from the non-NA dataframe
  df_non_na_fixed <- df_non_na %>% 
    distinct(`_30x30_Plot_TreeIDNumber`, .keep_all = TRUE)
  
  # Bind the rows back together
  combined_tree_tables_fixed <- bind_rows(df_na, df_non_na_fixed)
  
  # Your other operations here...
  combined_tree_tables_fixed <- combined_tree_tables_fixed %>%
    mutate(Tree_Count = ifelse(grepl("_30x30_Plot_Repeat|_30x15_Plot_Repeat", origin_table) & is.na(Tree_Count), 1, Tree_Count)) %>%
    group_by(Plot_ID, Species) %>%
    summarise(
      Tree_Count = sum(Tree_Count),
      # Retain the first value for all other columns
      across(everything(), first),
      .groups = "drop"
    )
  
  return(combined_tree_tables_fixed)
}




write_to_csv <- function(data, prefix, date_stamp = TRUE) {
  if (!dir.exists("Brazil_Raw_Data")) {
    dir.create("Brazil_Raw_Data")
  }

  if (date_stamp) {
    current_date <- format(Sys.Date(), "%Y-%m-%d") # e.g., "20231010"
    filename <- paste0("Brazil_Raw_Data/", prefix, "_", current_date, ".csv")
  } else {
    filename <- paste0("Brazil_Raw_Data/", prefix, ".csv")
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
# 1. Retrieve Kobo Data
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


# 5.5 This block and the associated functions would ideally be deleted. There
problematic_rows_30x30 <- find_problem_rows_30x30(DBH_table = adjusted_DBH_tables[[1]])
print(paste0("There are inconsistencies with trunk data for the following tree_index,",
             "values in the 30x30 DBH table: ",problematic_rows_30x30$tree_index))

problematic_rows_30x15 <- find_problem_rows_30x15(DBH_table = adjusted_DBH_tables[[2]])
print(paste0("There are inconsistencies with trunk data for the following tree_index,",
             "values in the 30x30 DBH table: ",problematic_rows_30x15$tree_index))

problem_entries <- bind_rows(problematic_rows_30x30, problematic_rows_30x15)


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
write_to_csv(final_combined_tree_tables, "Tree_Data_Uncorrected")

# Optionally, print a message to let the user know the process is complete:
cat("Data processing and export complete!\n")
