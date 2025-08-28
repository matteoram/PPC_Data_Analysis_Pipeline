# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Authors: Johannes Nelson, Matteo Ramina
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

#region 0. Check and install missing packages
necessary_packages <- c(
  "here", "readxl", "robotoolbox", "dplyr", "dm", "askpass", "purrr", "tidyr",  
  "jsonlite"
)

for (pkg in necessary_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(pkg, "not found. Installing now...\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Specify file names
folder_name <- "Brazil_Data"
dataset_name <- "PPCPACTO_Tree_Monitoring_-_Brazil_only_-_all_versions_-_False_-_2025-08-27-07-39-44.xlsx"
survey_name <- "PPC/PACTO Tree Monitoring - Brazil only"
mapping_name <- "Forms_mapping.xlsx"
output_name <- "main_brazil_data.json"

# Load sheet mapping paper form columns to kobo fields
brazil_to_main_map <- read_excel(
    file.path(folder_name, mapping_name),
    sheet = "paper_to_kobo_map"
)

#region 1. Read Excel Data
#'
#' This function reads data from a specified Excel file.
#'
#' @param file_path The path to the Excel file.
#' @return A list of data.frames, where each data.frame is a sheet from Excel.
read_excel_data <- function(file_path, data_file) {
  sheet_names <- readxl::excel_sheets(file.path(file_path, data_file))
  all_sheets <- purrr::map(
    sheet_names, ~readxl::read_excel(
      file.path(file_path, data_file), sheet = .x)
    )
  names(all_sheets) <- sheet_names
  return(all_sheets)
}

#region 2. Process Main Table
#'
#' This function pre-processes the main table by renaming columns, handling NAs,
#' and organizing the columns in a more logical order. It also separates out
#' geolocation data, photo attachment data, and PACTO data.
#'
#' @param main_table The original main table to be preprocessed.
#' @return List of dataframes--Main_Data, Geo_Data, Photo_Data, and PACTO_Data.

process_main_table <- function(main_table, mapping_file) {

  ## Address issue with information in incorrect columns
  # Steps:
  # 1. Specify relevant columns
  # 2. Copy values from "duplicated" columns to correct columns where empty/NA

  # 1. Specify relevant columns
  dup_cols <- c(
    "Plot_Info_Plot_ID_001",
    "Plot_Info_Plot_Permanence_001",
    "Plot_Info_Strata_001",
    "Plot_Info_TreesPresent_001",
    "Plot_Info_Resampling_001",
    "Plot_Info_Restoration_Technique_001",
    "Plot_Info_SEPhoto_001",
    "Plot_Info_NEPhoto_001",
    "Plot_Info_NWPhoto_001",
    "Plot_Info_SWPhoto_001",
    "Plot_Info_SECorner_001",
    "Plot_Info_NECorner_001",
    "Plot_Info_NWCorner_001",
    "Plot_Info_SWCorner_001"
  )
  correct_cols <- sub("_001$", "", dup_cols)
  for (i in seq_along(dup_cols)) {
    col1 <- dup_cols[i]
    col2 <- correct_cols[i]
      
    # 2. Identify rows where the Set 2 column is empty (NA or empty string)
    empty_rows <- is.na(main_table[[col2]]) | trimws(main_table[[col2]]) == ""
    
    # Copy values from Set 1 to Set 2 for those empty rows
    main_table[empty_rows, col2] <- main_table[empty_rows, col1]
    }

  # Remove incorrect columns
  main_table <- main_table %>%
    select(-all_of(dup_cols))

  ## Keep only relevant columns
  main_table <- main_table[,
      colnames(main_table) %in% (mapping_file %>%
      filter(include == 1) %>%
      pull(brazilian_form_field_name))
  ]

  ## Rename columns based on the mapping file
  # Steps:
  # 1. Filter out columns that are clearly not field names
  # 2. Create a mapping of old to new names
  # 3. Rename columns in the main table based on this mapping

  # 1. Filter column names that are clearly associated with field names
  valid_mapping <- mapping_file[
      !grepl("^(\\?|.*na.*|.*`)", mapping_file$kobo_form_field_name,
      ignore.case = TRUE),
  ]

  # 2. Create an old-new rename name map
  rename_map <- setNames(
      valid_mapping$kobo_form_field_name,
      valid_mapping$brazilian_form_field_name
  )

  # 3. Rename only those columns

  # Find which columns are actually present in main_table
  columns_to_rename <- intersect(
      names(rename_map), colnames(main_table)
  )
  
  # Rename the columns in main_table
  colnames(main_table)[
      match(columns_to_rename, colnames(main_table))
      ] <- rename_map[columns_to_rename]

  ## Create Plot_Type column based on SiteType. Assumes SiteType = Plot_Type
  main_table <- main_table %>%
    mutate(Plot_Type = SiteType)

  ## Create ControlSize. Assumes ControlSize opposite than SiteSize
  main_table <- main_table %>%
    mutate(ControlSize = case_when(
      Plot_Type == "Control" & SiteSize == "Yes" ~ "No",
      Plot_Type == "Control" & SiteSize == "No" ~ "Yes",
      TRUE ~ NA_character_
    )
  )

  ## Fix Resampling1 with NAs. This assumes an NA was a 0
  main_table <- main_table %>%
    mutate(Resampling1 = ifelse(
      is.na(Resampling1) & Plot_Type == "Restoration", 0, Resampling1
      )
    )

  ## Fix Resampling1 for control plots (should be NA)
  main_table <- main_table %>%
    mutate(Resampling1 = ifelse(
      !is.na(Resampling1) & Plot_Type == "Control", NA, Resampling1
      )
    )

  ## Fix TreesPresent for control plots (should be NA)
  main_table <- main_table %>%
    mutate(TreesPresent = ifelse(
      !is.na(TreesPresent) & Plot_Type == "Control", NA, TreesPresent
      )
    )

  ## Fix TreesPresent == No for plots that actually have trees
  # Steps:
  # 1. Get indices from main_table where TreesPresent == "No"
  # 3. Check each tree table and obtain violating indices that list trees
  # 3. Replace "Yes" with "No" in for violating indices

  # 1. Get indices where TreesPresent == "No"
  no_trees_indices <- main_table$`_index`[main_table$TreesPresent == "No"]

  # 2. Check each tree table and find violating indices
  
  # Initialize an empty vector to store violating indices
  violating_indices <- c()

  # Check _30x30_Plot_Repeat
  violating_30x30 <- all_data$`_30x30_Plot_Repeat`$`_parent_index`[
    all_data$`_30x30_Plot_Repeat`$`_parent_index` %in% no_trees_indices &
    !is.na(all_data$`_30x30_Plot_Repeat`$`_30x30_Plot_TreeSpecies`) &
    all_data$`_30x30_Plot_Repeat`$`_30x30_Plot_TreeSpecies` != ""
  ]
  violating_indices <- c(violating_indices, unique(violating_30x30))

  # Check _30x15_Plot_Repeat
  violating_30x15 <- all_data$`_30x15_Plot_Repeat`$`_parent_index`[
    all_data$`_30x15_Plot_Repeat`$`_parent_index` %in% no_trees_indices &
    !is.na(all_data$`_30x15_Plot_Repeat`$`_30x15_Plot_TreeSpecies`) &
    all_data$`_30x15_Plot_Repeat`$`_30x15_Plot_TreeSpecies` != ""
  ]
  violating_indices <- c(violating_indices, unique(violating_30x15))

  # Remove duplicates and print the final result
  violating_indices <- unique(violating_indices)

  # 3. Replace "No" with "Yes" in for these indices
  main_table$TreesPresent[
    main_table$`_index` %in% violating_indices] <- "Yes"

  ## Fix TreesPresent == Yes for plots that actually have no trees
  # Steps:
  # 1. Get indices from main_table where TreesPresent == "Yes"
  # 3. Check each tree table and obtain violating indices that list trees
  # 3. Replace "No" with "Yes" in for violating indices

  # 1. Get indices where TreesPresent == "Yes"
  yes_trees_indices <- main_table$`_index`[main_table$TreesPresent == "Yes"]

  # 2. Check each tree table and find violating indices

  # Get all indices that have any tree species recorded in either table
  present_trees_indices <- c(
    all_data$`_30x30_Plot_Repeat`$`_parent_index`[
      !is.na(all_data$`_30x30_Plot_Repeat`$`_30x30_Plot_TreeSpecies`) & 
      all_data$`_30x30_Plot_Repeat`$`_30x30_Plot_TreeSpecies` != ""
    ],
    all_data$`_30x15_Plot_Repeat`$`_parent_index`[
      !is.na(all_data$`_30x15_Plot_Repeat`$`_30x15_Plot_TreeSpecies`) & 
      all_data$`_30x15_Plot_Repeat`$`_30x15_Plot_TreeSpecies` != ""
    ]
  )
  present_trees_indices <- unique(present_trees_indices)

  # Find the indices that should have trees but don't
  violating_indices <- yes_trees_indices[
    !yes_trees_indices %in% present_trees_indices]

  # 3. Replace "Yes" with "No" in for violating indices
  main_table$TreesPresent[
    main_table$`_index` %in% violating_indices] <- "No"

  ## Fix Resampling1 for plots with TreesPresent == "No" but Resampling < 2
  main_table <- main_table %>%
    mutate(Resampling1 = ifelse(
      TreesPresent == "No" & Resampling1 < 2, 2, Resampling1
      )
    )

  ## Combine resampling columns from 3x3 plots into a single column
  main_table <- main_table %>%
    mutate(Resampling2 = ifelse(
      is.na(Resample_3x3_Subplot) | Resample_3x3_Subplot == "",
      Resample_3x3_ControlSubplot,
      Resample_3x3_Subplot
    )
  ) %>%
  select(-Resample_3x3_Subplot, -Resample_3x3_ControlSubplot)

  ## Combine LittleTreesPresent columns from 3x3 plots into a single column
  main_table <- main_table %>%
    mutate(LittleTreesPresent = ifelse(
      is.na(TreesPresent_3x3_Subplot) | TreesPresent_3x3_Subplot == "",
      Trees_3x3_ControlSubplot,
      TreesPresent_3x3_Subplot
    )
  ) %>%
  select(-TreesPresent_3x3_Subplot, -Trees_3x3_ControlSubplot)

  ## Fix missing Resampling2 values for control plots (should not be NA)
  # Steps:
  # 1. Get control plots
  # 2. Find which of these control plots have entries in _3x3_Subplot_Repeat
  # 3. Update Resampling2 and LittleTreesPresent accordingly

  # 1. Get control plots
  control_plot_indices <- main_table$`_index`[main_table$Plot_Type == "Control"]

  # 2. Find which of these control plots have entries in _3x3_Subplot_Repeat
  is_present <- control_plot_indices %in% all_data$`_3x3_Subplot_Repeat`$`_parent_index`

  # 3. Update Resampling2 and LittleTreesPresent accordingly
  main_table$Resampling2[
    main_table$`_index` %in% control_plot_indices
    ] <- ifelse(is_present, 0, 2)
  main_table$LittleTreesPresent[
    main_table$`_index` %in% control_plot_indices
    ] <- ifelse(
      is_present, "Yes", "No"
      )

  ## Create Centroid_of_3m_x_3m_Subplot2 (Centroid_of_3m_x_3m_Subplot and 
  ## Centroid_of_3m_x_3m_Subplot1 are not applicable in Brazil form)
  main_table <- main_table %>%
    mutate(Centroid_of_3m_x_3m_Subplot2 = ifelse(
      is.na(`_3x3_Subplot_Centroid`) | `_3x3_Subplot_Centroid` == "",
      `_3x3_ControlSubplot_Centroid`,
      `_3x3_Subplot_Centroid`
    )
  ) %>%
  select(-`_3x3_Subplot_Centroid`, -`_3x3_ControlSubplot_Centroid`)

  ## Create Description_of_subpl_ithin_30m_x_30m_plot2
  main_table <- main_table %>%
    mutate(Description_of_subpl_ithin_30m_x_30m_plot2 = ifelse(
      is.na(`_3x3_Subplot_Description`) | `_3x3_Subplot_Description` == "",
      `_3x3_ControlSubplot_Descriptio`,
      `_3x3_Subplot_Description`
    )
  ) %>%
  select(-`_3x3_Subplot_Description`, -`_3x3_ControlSubplot_Descriptio`)

  ## Create Notes17
  main_table <- main_table %>%
    mutate(Notes17 = ifelse(
      is.na(`Notes_002`) | `Notes_002` == "",
      `ControlNotes_002`,
      `Notes_002`
    )
  ) %>%
  select(-`Notes_002`, -`ControlNotes_002`)

  ## Create Additional_Photos_Optional3
  main_table <- main_table %>%
    mutate(Additional_Photos_Optional3 = ifelse(
      is.na(`_3x3_Subplot_Photo`) | `_3x3_Subplot_Photo` == "",
      `_3x3_ControlSubplot_Photo`,
      `_3x3_Subplot_Photo`
    )
  ) %>%
  select(-`_3x3_Subplot_Photo`, -`_3x3_ControlSubplot_Photo`)  

  ## Create Notes21
  main_table <- main_table %>%
    mutate(Notes21 = ifelse(
      is.na(`Notes`) | `Notes` == "",
      `Notes_001`,
      `Notes`
    )
  ) %>%
  select(-`Notes`, -`Notes_001`)

  ## Create Note14
  main_table <- main_table %>%
    mutate(Note14 = Plot_Info_Notes) %>%
  select(-`Plot_Info_Notes`)

  ## Create Coordinate_System_Used
  main_table <- main_table %>%
    mutate(Coordinate_System_Used = NA)

  ## Rename General_Info_Organization_Name to Organization_Name
  main_table <- main_table %>%
    rename(Organization_Name = General_Info_Organization_Name)

  ## Return processed main table
  return(main_table)
}

#region 3. Prepare Tree Tables
#'
#' The following functions process the tree tables by extracting relevant
#' columns, grouping by species, and counting the number of trees of each 
#' species.
#' Each function corresponds to a specific plot type (30x30, 30x15, 3x3, and 
#' planted trees).
#' The functions also handle any necessary adjustments,
#' such as extrapolating data from 30x15 plots to 30x30 plots.

process_30x30 <- function(df, tree_monitoring_sheet) {
  df_30x30 <- df[["_30x30_Plot_Repeat"]]
  
  if (is.null(df_30x30)) {
    stop("'_30x30_Plot_Repeat' not found in the master data frame.")
  }
  
  df_30x30_processed <- df_30x30 %>%
    select(
      Tree_Species2 = `_30x30_Plot_TreeSpecies`,
      Tree_Type2 = `_30x30_Plot_TreeType`,
      Note111 = `_30x30_Plot_RepeatNote_001`,
      `_parent_index` = `_parent_index`
    ) %>%
    # Join to get the date and plot ID
    left_join(tree_monitoring_sheet, by = c("_parent_index" = "_index")) %>%
    # Perform the count on the joined data
    group_by(`_parent_index`, `Tree_Species2`, `Enter_a_date`, `Plot_ID`) %>%
    add_count(name = "Number_of_Trees_of_this_Species2") %>%
    ungroup()
  
  return(df_30x30_processed)
}

process_30x15 <- function(df, tree_monitoring_sheet) {
  df_30x15 <- df[["_30x15_Plot_Repeat"]]
  
  if (is.null(df_30x15)) {
    stop("'_30x15_Plot_Repeat' not found in the master data frame.")
  }
  
  df_30x15_processed <- df_30x15 %>%
    select(
      Tree_Species2 = `_30x15_Plot_TreeSpecies`,
      Tree_Type2 = `_30x15_Plot_TreeType`,
      `_parent_index` = `_parent_index`
    ) %>%
    # Join to get the date and plot ID
    left_join(tree_monitoring_sheet, by = c("_parent_index" = "_index")) %>%
    # Perform the count on the joined data
    group_by(`_parent_index`, `Tree_Species2`, `Enter_a_date`, `Plot_ID`) %>%
    add_count(name = "Number_of_Trees_of_this_Species2") %>%
    ungroup() %>%
    mutate(
      Note111 = "Extrapolated from 30x15 plot.",
      Number_of_Trees_of_this_Species2 = Number_of_Trees_of_this_Species2 * 2
    )
  return(df_30x15_processed)
}

process_3x3 <- function(df, tree_monitoring_sheet) {
  df_3x3 <- df[["_3x3_Subplot_Repeat"]]
  
  if (is.null(df_3x3)) {
    stop("'_3x3_Subplot_Repeat' not found in the master data frame.")
  }
  
  df_3x3_processed <- df_3x3 %>%
    select(
      Tree_Species_0012 = `_3x3_Subplot_TreeSpecies`,
      Tree_Type_0012 = `_3x3_Subplot_TreeType`,
      Number_of_Trees_of_this_Species_0012 = `_3x3_Subplot_TreeCount`,
      Note22 = `_3x3_Subplot_RepeatNote`,
      `_parent_index` = `_parent_index` # Retain this column
    ) %>%
    # Join to get the date and plot ID
    left_join(tree_monitoring_sheet, by = c("_parent_index" = "_index"))
    
  return(df_3x3_processed)
}

process_planted <- function(df, tree_monitoring_sheet) {
  df_planted <- df[["_30x30_Plot_Repeat_Planted_10cm"]]
  
  if (is.null(df_planted)) {
    stop("'_30x30_Plot_Repeat_Planted_10cm' not found in the master data frame.")
  }
  
  df_planted_processed <- df_planted %>%
    select(
      Tree_Species_Seedlings = `_30x30_Plot_TreeSpecies_Planted_10cm`,
      Numer_of_This_Species3 = `_30x30_Plot_Count_Planted_10cm`,
      Note3 = `_30x30_Plot_RepeatNote_Planted_10cm`,
      `_parent_index` = `_parent_index` # Retain this column
    ) %>%
    # Join to get the date and plot ID
    left_join(tree_monitoring_sheet, by = c("_parent_index" = "_index"))
    
  return(df_planted_processed)
}

#region 4. Create JSON Export Function
#'
#' This function processes the main table and tree tables, nests them, and
#' exports the final structured data as a JSON file.

process_and_export_to_json <- function(
    main_table_df,
    planted_trees_df,
    an2yk58_df,
    ka7vj63_df,
    output_filename = output_name
) {

  print("Nesting data frames...")
  # Specify nested data (only for the data frames you have)
  nested_PlantedTrees3 <- planted_trees_df %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(PlantedTrees3 = -c(Enter_a_date, Plot_ID))

  nested_an2yk58 <- an2yk58_df %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(group_an2yk58 = -c(Enter_a_date, Plot_ID))

  nested_ka7vj63 <- ka7vj63_df %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(group_ka7vj63 = -c(Enter_a_date, Plot_ID))

  # Create a list of all data frames to join
  # The main table is the starting point for the join
  dfs_to_join <- list(
    main_table_df,
    nested_PlantedTrees3,
    nested_an2yk58,
    nested_ka7vj63
  )

  print("Joining all nested data frames...")
  # Safely join all the nested data frames
  final_nested_data <- reduce(
    dfs_to_join,
    ~ full_join(.x, .y, by = c("Enter_a_date", "Plot_ID"))
  )

  print("Structuring data for JSON conversion...")
  # Structure and JSON Conversion
  structured_list <- final_nested_data %>%

      rowwise() %>%
      mutate(
          json_object = list(list(

          Enter_a_date = Enter_a_date,
          Country = Country,
          Organization_Name = Organization_Name,
          Site_ID = Site_ID,
          Timeframe = Timeframe,
          
          Plot = list(
              Plot_ID = Plot_ID,
              Plot_Type = Plot_Type,
              SiteSize = SiteSize,
              ControlSize = ControlSize,
              Plot_Permanence = Plot_Permanence,
              Resampling1 = Resampling1,
              TreesPresent = TreesPresent,
              PlantingPattern = PlantingPattern,
              Coordinate_System_Used = Coordinate_System_Used
          ),
          
          SmallSitesControl = list(
              Northeast_Corner_of_10m_x_10m_Plot = Northeast_Corner_of_10m_x_10m_Plot,
              Large1 = list(group_oq8nt56 = group_oq8nt56),
              Small1 = list(
              Centroid_of_3m_x_3m_Subplot1 = Centroid_of_3m_x_3m_Subplot1,
              group_wj4lz16 = group_wj4lz16
              )
          ),
          Large2 = list(group_an2yk58 = group_an2yk58),
          Small3 = list(
              Resampling2 = Resampling2,
              LittleTreesPresent = LittleTreesPresent,
              Centroid_of_3m_x_3m_Subplot2 = Centroid_of_3m_x_3m_Subplot2,
              group_ka7vj63 = group_ka7vj63
          ),
          Census = list(group_ql6qg69 = group_ql6qg69),
          PlantedTrees3 = PlantedTrees3,
          `__version__` = `__version__`,
          `_uuid` = `_uuid`
          ))
      ) %>%
      ungroup() %>%
      pull(json_object)

  print("Converting to JSON and saving...")
  # Convert to JSON
  json_output <- toJSON(
    structured_list,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "rows"
  )

  # Save the JSON to a file
  write(json_output, file.path("Brazil_Data", output_filename))
  
  message("JSON file successfully created and saved to ", file.path("Brazil_Data", output_filename))
}


#------------------------------------------------------------------------------
# The above script defines all these functions. The 'main' script below calls them
# each in turn. By having distinct modules, errors/bugs that might arise in the
# future will be easier to diagnose. The print() statements output at each step
# in the console can help locate where things went wrong, and the relevant function
# above will be a good starting point for debugging.
#------------------------------------------------------------------------------

# 1. Retrieve Kobo Data
print("Retrieving data from Excel.")
all_data <- read_excel_data(folder_name, dataset_name)
print("Data Retrieved successfully!")


# 2. Prepare Main Table
print("Prepping main table.")
tree_monitoring_sheet <- process_main_table(
  all_data[["PPC_PACTO Tree Monitoring - ..."]], brazil_to_main_map
)

# 3. Prepare Tree Tables
#print("Prepping tree tables.")
#group_an2yk58_30x30 <- process_30x30(df, tree_monitoring_sheet)
#group_an2yk58_30x15 <- process_30x15(df, tree_monitoring_sheet)
#group_an2yk58 <- bind_rows(group_an2yk58_30x30, group_an2yk58_30x15)
#group_an2yk58 <- group_an2yk58 %>%
#  select(-`_parent_index`) %>%
#  distinct()
#group_ka7vj63 <- process_3x3(df, tree_monitoring_sheet)
#PlantedTrees3 <- process_planted(df, tree_monitoring_sheet)

# 4. Create JSON Export
#print("Processing and exporting data to JSON.")
#process_and_export_to_json(
#    main_table_df = tree_monitoring_sheet,
#    planted_trees_df = PlantedTrees3,
#    an2yk58_df = group_an2yk58,
#    ka7vj63_df = group_ka7vj63,
#    output_filename = output_name
#)

#------------------------------------------------------------------------------
# END