# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Author: Johannes Nelson
# Input: User will be prompted for Kobo username and password.
# Outputs: CSV files pertaining to different tables and variables.

# Description: This code forms the data extraction and preprocessing component
# of PPC's data analysis pipeline. This is the script specific to the Brazil Data,
# which was collected in a different manner and required special handling.

# The primary outputs are the "Main_Data_Brazil"  CSV and the combined
# "Tree_Data_Brazil" CSV. The former contains information  about each
# submission/monitoring plot (the responses to the Kobo form questions
# that are not records of tree species observed or planted), and the latter
# contains information about all the trees observed during monitoring surveys.
# In addition to the tree data, a folder with DBH data will contain trunk measurements
# for each of the recorded trees.

# Additional outputs include Geolocation_Data, Photo_Data, PACTO_data, and a folder
# that separates out tree data by plot type.
#
# The tree data file contains the word "uncorrected", which refers to the fact that
# the species names have not been cleaned. This will be addressed in a later script.
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


#' 1. Retrieve Kobo Data
#'
#' This function prompts the user for their username and password, and then
#' proceeds to extract data from Kobo with minor wrangling and renaming.
#'
#' @return List of data.frames that correspond to different data tables.

retrieve_kobo_data <- function(asset_name = "PPC/PACTO Tree Monitoring - Brazil only") {
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
  cat(paste0("Number of Expected Tables: 10 \n", "Number of Retrieved Tables: ", length(df), "\n"))
  expected_names <- c(
    "main", "_30x30_Plot_Repeat", "group_un3bb19",
    "_3x3_Subplot_Repeat", "_30x30_Plot_Repeat_Planted_10cm",
    "_30x15_Plot_Repeat", "group_dz1zn48", "Plot_Info_Repeat",
    "_30x30_Plot_Repeat_Census_10cm", "_30x15_Plot_Repeat_Planted_10c_001"
  )

  if (all(expected_names %in% names(df))) {
    cat("Tables in Kobo data are named as expected.\n")
  } else {
    cat(paste0(
      "Table names are not as expected. This may cause data quality issues or unexpected outputs. \n",
      "Unexpected table name(s): ", names(df)[!names(df) %in% expected_names], " \n"
    ))
  }

  return(list(main_table = main_table, tree_tables = tree_tables, DBH_tables = DBH_tables))
}



#' 2. Process Main Table
#'
#' This function pre-processes the main table by renaming columns, handling NAs,
#' and organizing the columns in a more logical order. It also separates out
#' geolocation data, photo attachment data, and PACTO data.
#'
#' @param main_table The original main table to be preprocessed.
#' @return List of dataframes--Main_Data, Geo_Data, Photo_Data, and PACTO_Data.

process_main_table <- function(main_table) {
  if ("Notes" %in% names(main_table)) {
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
    mutate(Resample_3x3_Subplot = ifelse(is.na(Resample_3x3_Subplot), 0, Resample_3x3_Subplot)) %>%
    mutate(Monitoring_Plot_Size = ifelse(SiteSize == "Yes", "30x30", "30x15"))



  geo_columns <- names(main_table)[grep("Corner|Centroid", names(main_table), ignore.case = TRUE)]
  geo_data <- main_table %>%
    select(
      Organization_Name, Site_ID, Plot_ID,
      SiteType,
      all_of(geo_columns)
    ) %>%
    select(-names(.)[grep("Photo|001", names(.), ignore.case = TRUE)])



  photo_attachments <- main_table$`_attachments`
  full_attachments_df <- bind_rows(photo_attachments)
  final_attachments <- left_join(
    select(
      main_table,
      Organization_Name,
      Site_ID,
      Plot_ID,
      `_id`
    ),
    full_attachments_df,
    by = c("_id" = "instance")
  )


  PACTO_cols <- names(main_table)[grep("pacto", names(main_table), ignore.case = TRUE)]
  PACTO_data <- main_table %>%
    select(
      Organization_Name, Site_ID, Plot_ID,
      SiteType,
      all_of(PACTO_cols)
    )


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

  return(list(
    Main_Data = main_table,
    Geo_Data = geo_data,
    Photo_Data = final_attachments,
    PACTO_data = PACTO_data
  ))
}

#' 3. Clean Tree Tables
#'
#' This function renames columns based on known naming conventions so that the
#' combined data has standardized variable names, adds columns and values for
#' Plot_Size and origin_table, and joins the tree data with the main data so that
#' information like Plot_ID, Site_ID, etc. becomes available in the tree data
#' as well.
#'
#' @param tree_tables The unprocessed tree tables from the main data list
#' @param main_table The preprocessed main table output by the above function.
#'
#' @return a list of dataframes of tree tables cleaned/processed tree data

clean_tree_tables <- function(tree_tables, main_table) {
  # Extract table names to iterate over them
  tree_table_names <- names(tree_tables)


  tree_tables_modified <- lapply(tree_table_names, function(name) {
    # Retrieve one dataframe from the tree_tables list
    df <- tree_tables[[name]]

    # Check column names for certain patterns and standardize across tables.
    col_names <- names(df)
    col_names[grep("species", col_names, ignore.case = TRUE)] <- "Species"
    col_names[grep("type", col_names, ignore.case = TRUE)] <- "Tree_Type"
    col_names[grep("count", col_names, ignore.case = TRUE)] <- "Tree_Count"

    # This handles one table that did not have the word 'Number' in its tree count column.
    col_names[grep("_30x30_Plot_Census_10cm",
      col_names,
      ignore.case = TRUE
    )] <- "Tree_Count"

    # Renaming indices in tree tables for unique, clear reference both parent and
    # child tables (main_index and trunk_index)
    col_names[col_names == "_index"] <- "tree_index"
    col_names[col_names == "_parent_index"] <- "main_index"
    names(df) <- col_names


    # Extract plot dimensions from the table name and add Plot_Size column. Assumes
    # table names do not change. Unexpected new tables may cause unwanted behavior.
    plot_dims <- strsplit(name, "_")[[1]][2]
    df$Plot_Size <- plot_dims

    # Record origin table for future reference
    df$origin_table <- name

    # Checks for two tables with planted tree data. These need Tree_Type columns
    # which can automatically be populated with 'planted'
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

    # Reordering columns -- relevant first.
    df <- df %>% select(
      Species,
      Tree_Type,
      Plot_ID,
      Site_ID,
      SiteType,
      Plot_Size,
      everything()
    )

    return(df)
  })

  names(tree_tables_modified) <- tree_table_names
  return(tree_tables_modified)
}



#' 4. Clean Diameter at Breast Height Tables
#'
#' This function processes the DBH tables by renaming specific columns for consistency,
#' removing unnecessary columns, and merging the DBH data with the tree data. This
#' ensures that the combined data includes critical information such as Species, Tree_Type,
#' Plot_ID, Site_ID, and other essential attributes. The function also introduces an
#' origin_table column to identify the source table for each entry.
#'
#' @param DBH_tables A list of unprocessed DBH tables.
#' @param tree_tables The preprocessed and cleaned tree tables, used to provide
#' context and additional information for the DBH tables.
#'
#' @return A list of cleaned and merged DBH tables.


clean_DBH_tables <- function(DBH_tables, tree_tables) {
  # Extract table names to iterate over them
  DBH_table_names <- names(DBH_tables)


  DBH_tables_modified <- lapply(DBH_table_names, function(name) {
    # Retrieve a dataframe from the DBH_table_names list
    df <- DBH_tables[[name]]

    # Record origin table for future reference
    df$origin_table <- name

    # Rename columns pertaining to reference indices for clear downstream joins
    col_names <- names(df)
    col_names[col_names == "_index"] <- "trunk_index"
    col_names[col_names == "_parent_index"] <- "tree_index"
    names(df) <- col_names

    # Removes an unwanted column
    df <- df[, !grepl("button", names(df))]

    # Identifies parent tree table for this data and retrieves it from tree tables
    parent_table_name <- unique(df$`_parent_table_name`)
    parent_table <- tree_tables[[parent_table_name]]

    # Joins with tree data by "tree_index" so that trunk measurements are tied
    # with tree data as well as the plot data (since that was already joined to
    # the tree tables)
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
          Plot_Size,
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

  names(DBH_tables_modified) <- c("DBH_30x30", "DBH_30x15")
  return(DBH_tables_modified)
}


#' 5. Adjust Diameter at Breast Height Tables
#'
#' This function handles an inconsistency in the data coming from Kobo, where
#' certain DBH measurements were not in the expected tables. It extracts these
#' misplaced data from the tree tables and binds them to the relevant DBH table.
#' Currently, this is a one-time issue, and the below function assumes this will
#' not continue to happen.
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
  DBH_tables$DBH_30x30 <- bind_rows(DBH_tables$DBH_30x30, new_form_data)
  DBH_tables$DBH_30x30$trunk_index <- c(1:nrow(DBH_tables$DBH_30x30))

  return(DBH_tables)
}


#' 6. Adjust Census Table
#'
#' This function handles a mistake in form creation, where only a single census
#' table is generated, wheter or not the monitoring plot is 30x30 or 30x15. This
#' function uses parent index values to edit the plot size and ensure that any
#' tree data coming from a 30x15 plot is labeled correctly. I left this separate
#' in case the form is ever fixed, in which case this could be easily deleted with
#' out impacting other functions.
#'
#' @param tree_tables The list of cleaned/preprocessed tree tables
#' @param main_table The preprocessed main_table
#' and appended to the DBH tables.
#'
#' @return List of tree tables

adjust_census_table <- function(tree_tables, main_table) {
  # Extracting the census table name
  census_table_name <- names(tree_tables)[grep(
    pattern = "census",
    x = names(tree_tables),
    ignore.case = TRUE
  )]

  # Extracting the dataframe from the list
  df <- tree_tables[[census_table_name]]

  df$Plot_Size <- NULL

  # Adjusting the dataframe by referencing the main_table for Monitoring_Plot_Size
  df_fixed <- df %>%
    left_join(select(main_table, main_index, Monitoring_Plot_Size), by = "main_index") %>%
    select(-main_index)

  # Updating the list with the modified dataframe
  tree_tables[[census_table_name]] <- df_fixed

  return(tree_tables)
}



#' 6.5 Find problematic rows
#'
#' The following two functions will not output new data. Rather, they identify
#' potentially problematic rows wihtin the dataset where Trunk/Tree labeling has
#' not gone as expected. Because there are a handful of ways this can be done, it
#' is best to clean it up on the data entry side. Since this input error would
#' directly impact tree counts, though, output is added here to help identify
#' problematic entries.

find_problem_rows_30x30 <- function(DBH_table) {
  df <- DBH_table
  problematic_tree_indices <- df %>%
    group_by(Plot_ID, Timeframe, `_30x30_Plot_TreeIDNumber`) %>%
    filter(sum(`_30X30_Plot_TreeTrunk` == 1) > 1) %>%
    pull(tree_index) %>%
    unique()

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

  problematic_rows <- df %>%
    filter(tree_index %in% problematic_tree_indices)

  return(problematic_rows)
}



#' 7. Remove Columns with Only NAs
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




#' 8. Combine and Refine Tree Tables
#'
#' This function consolidates a list of tree tables into a single table. During the
#' combination process, the function distinguishes rows based on the presence or
#' absence of NA values in the "_30x30_Plot_TreeIDNumber" column. This is because
#' if this column is populated, it is actually trunk data, not tree data--which
#' mysteriously appeared in the tree table, likely for the same reasons that Stonehenge
#' was built by early humans. For rows containing trunk information, duplicates are
#' removed to avoid multiple trunks mistakenly being counted as multiple trees.
#' Additional refinements are made to the Tree_Count values and the data is grouped
#' by Plot_ID and Species for summarization.
#'
#' @param tree_tables_list A list of tree tables that need to be combined and refined.
#'
#' @return A single consolidated and refined tree table.
combine_tree_tables <- function(tree_tables_list) {
  # First, combine all tables
  combined_tree_tables <- bind_rows(tree_tables_list)

  # Separate dataframes: one with non-NA values and one with NA values for the
  # column that points toward misplaced trunk data
  df_non_na <- combined_tree_tables %>% filter(!is.na(`_30x30_Plot_TreeIDNumber`))
  df_na <- combined_tree_tables %>% filter(is.na(`_30x30_Plot_TreeIDNumber`))

  # Remove duplicates only from the non-NA dataframe, effectively collapsing any
  # tree with multiple trunks back into a single entry
  df_non_na_fixed <- df_non_na %>%
    distinct(`_30x30_Plot_TreeIDNumber`, .keep_all = TRUE)

  # Bind the rows back together into a full table
  combined_tree_tables_fixed <- bind_rows(df_na, df_non_na_fixed)

  # Since there are no counts associated with trees for which there is trunk data,
  # this populates the tree_count column with '1' for those entries. Then it goes
  # by site-plot combos, and totals the counts.
  tree_data_with_single_counts <- combined_tree_tables_fixed %>%
    filter(origin_table %in% c("_30x30_Plot_Repeat", "_30x15_Plot_Repeat")) %>%
    mutate(Tree_Count = ifelse(
      origin_table %in% c("_30x30_Plot_Repeat", "_30x15_Plot_Repeat") & is.na(Tree_Count),
      1, Tree_Count
    )) %>%
    group_by(Site_ID, Plot_ID, Species) %>%
    summarise(
      Tree_Count = sum(Tree_Count),
      # Retain the first value for all other columns
      across(everything(), first),
      .groups = "drop"
    )

  # Takes the data for which true counts were input by users (Planted and 3x3 tables)
  # and binds it to the new data with total counts
  tree_data_with_true_counts <- combined_tree_tables_fixed %>%
    filter(!origin_table %in% c("_30x30_Plot_Repeat", "_30x15_Plot_Repeat"))
  tree_data_with_final_counts <- bind_rows(
    tree_data_with_single_counts,
    tree_data_with_true_counts
  )


  # This jettisons the unneeded trunk data which has been put in place already
  tree_data_with_final_counts <- tree_data_with_final_counts %>%
    select(
      -`_30x30_Plot_TreeIDNumber`,
      -`_30X30_Plot_TreeTrunk`,
      -`_30x30_Plot_TreeDBH`
    )

  return(tree_data_with_final_counts)
}



#' 8. Write CSVs to disk
#'
#' These are some simple helper functions to make writing lists of dataframes and
#' single dataframes to the disk. They add date stamps and assume no
#' sub directory by default.


write_to_csv <- function(data, prefix, date_stamp = TRUE, sub_dir = NULL) {
  main_dir <- "Brazil_Raw_Data"

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
print("Data Retrieved successfully!")


# 2. Prepare Main Table
print("Prepping main table, geolocation data, photo data, and PACTO data.")
main_geo_photo_pacto <- process_main_table(all_data$main_table)

# 3. Clean Tree Tables
print("Cleaning/processing tree tables.")
cleaned_tree_tables <- clean_tree_tables(all_data$tree_tables, main_geo_photo_pacto$Main_Data)

# 4. Clean DBH Tables
print("Cleaning/processing trunk/DBH tables.")
cleaned_DBH_tables <- clean_DBH_tables(all_data$DBH_tables, cleaned_tree_tables)

# 5. Adjust DBH Tables
print("Making adjustments to DBH tables.")
adjusted_DBH_tables <- adjust_DBH_tables(cleaned_DBH_tables, cleaned_tree_tables)

# 6. Adjust Census Table
print("Addressing census table issue.")
adjusted_tree_tables <- adjust_census_table(tree_tables = cleaned_tree_tables, main_table = main_geo_photo_pacto$Main_Data)


# 6.5 This block and the associated functions would ideally be deleted. There
problematic_rows_30x30 <- find_problem_rows_30x30(DBH_table = adjusted_DBH_tables[[1]])
print(paste0(
  "There are inconsistencies with trunk data for the following tree_index,",
  "values in the 30x30 DBH table: ", problematic_rows_30x30$tree_index
))

problematic_rows_30x15 <- find_problem_rows_30x15(DBH_table = adjusted_DBH_tables[[2]])
print(paste0(
  "There are inconsistencies with trunk data for the following tree_index,",
  "values in the 30x15 DBH table: ", problematic_rows_30x15$tree_index
))

problem_entries_30x30 <- problematic_rows_30x30
problem_entries_30x15 <- problematic_rows_30x15

# 7. Remove NA columns
print("Removing empty columns.")
final_DBH_tables <- remove_NA_columns(tables_list = adjusted_DBH_tables)
final_tree_tables <- remove_NA_columns(tables_list = cleaned_tree_tables)

# 8. Combine Tree Tables
print("Combining tree tables and processing tree counts.")
final_combined_tree_tables <- combine_tree_tables(final_tree_tables)
print("Preprocessing complete!")

# 9. Write Data to Disk
print("Writing data to disk.")
write_list_to_csv(main_geo_photo_pacto, names(main_geo_photo_pacto))
write_list_to_csv(final_DBH_tables, names(final_DBH_tables), sub_dir = "DBH_data")
write_list_to_csv(final_tree_tables, names(final_tree_tables), sub_dir = "Tree_Data_by_PlotType") # This may not even be a necessary output
write_to_csv(final_combined_tree_tables, "Tree_Data_Uncorrected_Brazil")

cat("Data processing and export complete!\n")
