library(dplyr)
library(stringr)
library(tidyverse)



load_data <- function() {
  # Determine which dataset is being corrected and set path to desired folder
  answer <- readline(prompt = cat("Which Dataset are analyzing? \n Enter '1' for the Primary Dataset or '2' for the Brazil Dataset:"))
  if (!answer %in% c("1", "2")) {
    print("Invalid response, please rerun script and be sure to enter either '1' or '2'")
  } else if (answer == "1") {
    raw_data_path <- "Main_Raw_Data"
  } else if (answer == "2") {
    raw_data_path <- "Brazil_Raw_Data"
  }
  
  # Get all files that match the pattern "Tree_Data_Uncorrected" in the "Brazil_Raw_Data" folder
  tree_files <- list.files(path = raw_data_path, pattern = "Final_Tree_Data", full.names = TRUE)
  
  # Sort files by modification date to get the most recent and read this into session
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]
  tree_data <- read.csv(latest_tree_file, check.names = FALSE)

  print(paste0("Latest tree data file: ", latest_tree_file))
  
  return(list(tree_data = tree_data, raw_data_path = raw_data_path))
}







scale_tree_count <- function(data) {
  scaled_data <- data %>%
    mutate(scaled_count = case_when(
      (Plot_Size == "30x30" & grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count,
      (Plot_Size == "30x30" & !grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count / (Resample_Main_Plot + 1),
      Plot_Size == "10x10" ~ (Tree_Count / (Resample_Main_Plot + 1))*9, # Change this if upscaling is not desired
      Plot_Size == "3x3" ~ Tree_Count * 100 / (Resample_3x3_Subplot + 1),
      Plot_Size == "1x1" ~ Tree_Count * 900,
      TRUE ~ Tree_Count
    )) %>% 
    mutate(scaled_count = ifelse(Tree_Type == "planted", Tree_Count, scaled_count)) %>% 
    select(Species, Tree_Count, 
           scaled_count, 
           size_class, 
           Country, 
           Organization_Name, 
           Site_ID, Plot_ID, 
           Plot_Size, 
           origin_table,
           everything())
return(scaled_data)
}



generate_baseline_reports <- function(data) {
  
  data_Y0 <- data %>% filter(Timeframe == 'Y0')
  
  results_by_species <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Plot_ID, size_class, Species, Tree_Type_Group) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  
  
  
  results_by_family <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Plot_ID, size_class, family, Tree_Type_Group) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  
  results_by_size_class <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Plot_ID,size_class, Tree_Type_Group) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  
  
  
  
  results_by_plot <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Plot_ID, Tree_Type_Group) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  
  
  
  
  
  results_by_site <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Tree_Type_Group) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  
  
  results_by_country<- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Country, Tree_Type_Group) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  
  
  return(list(
    Results_by_Species = results_by_species,
    Results_by_Family = results_by_family,
    Results_by_SizeClass = results_by_size_class,
    Results_by_Plot = results_by_plot,
    Results_by_Site = results_by_site,
    Results_by_Country = results_by_country))
  
}




# Trees planted, plus trees that are naturally regenerating
generate_trees_restored_reports <- function(scaled_data){
  
}




write_to_csv <- function(data, prefix, main_dir, date_stamp = TRUE, sub_dir = NULL) {

  
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


write_list_to_csv <- function(data_list, prefix_list, main_dir, date_stamp = TRUE, sub_dir = NULL) {
  if (length(data_list) != length(prefix_list)) {
    stop("The number of data items does not match the number of prefixes.")
  }
  
  for (i in seq_along(data_list)) {
    write_to_csv(data_list[[i]], prefix_list[i], main_dir, date_stamp, sub_dir)
  }
}



#### BEGIN MAIN

all_data <- load_data()

scaled_data <- scale_tree_count(all_data$tree_data)

BL_reports <- generate_baseline_reports(scaled_data)

write_list_to_csv(BL_reports, names(BL_reports),main_dir = all_data$raw_data_path, sub_dir = "Baseline_Reports")
