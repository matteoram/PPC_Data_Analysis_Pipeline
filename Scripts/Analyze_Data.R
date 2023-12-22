# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Author: Johannes Nelson
# Inputs: Final_Tree_Data_YYY-MM-DD_HHMM.csv (output from family names script)
# and site_size_data.csv, which is a csv containing the site size information that
# needs to be manually exported and placed in the parent directory.
# Outputs: Reports of baseline counts as well as reports for PPC Indicators

# Description: This script assumes the pipeline has been run up through the
# family names additions. It begins by scaling the tree counts up to the monitoring
# plot size based on logic about table names and the number of times plots were
# resampled. It then joins the site size data with the kobo data in order to
# calculate total site sizes and the sample ration between the area monitored and
# total area for extrapolation.
#
# --------------------------------------------------------------------------------

necessary_packages <- c("dplyr", "stringr", "tidyverse")

for (pkg in necessary_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(pkg, "not found. Installing now...\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}



#' 1. Load Data
#'
#' This function prompts the user to designate the desired dataset (Primary or
#' Brazil), and loads in the relevant data: Final_Tree_Data, Main_Data, and
#' site_size_data--the latter of which needs ot be manually placed in the parent
#' folder!
#' @return a list with the above mentioned dataframes

load_data <- function() {
  # Determine which dataset is being corrected and set path to desired folder
  answer <- readline(prompt = cat("Which Dataset are analyzing? \n Enter '1' for the Primary Dataset or '2' for the Brazil Dataset:"))
  if (!answer %in% c("1", "2")) {
    print("Invalid response, please rerun script and be sure to enter either '1' or '2'")
  } else if (answer == "1") {
    raw_data_path <- "Main_Data"
  } else if (answer == "2") {
    raw_data_path <- "Brazil_Data"
  }

  # Get all files that match the pattern "Tree_Data_Uncorrected"
  tree_files <- list.files(path = raw_data_path, pattern = "Final_Tree_Data", full.names = TRUE)

  if (length(tree_files) < 1) {
    cat(paste0("There is no Final_Tree_Data file in ", raw_data_path, ". Make sure you've run the Add_Family_Names.R script first!"))
  }

  # Sort files by modification date to get the most recent and read this into session
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]
  tree_data <- read.csv(latest_tree_file, check.names = FALSE)
  print(paste0("Latest tree data file: ", latest_tree_file))

  site_size_files <- list.files(pattern = "site_size", full.names = TRUE)

  if (length(site_size_files) < 1) {
    cat(paste0("There is no site_size file in ", raw_data_path, ". You need to download the site size data and save it as a CSV with the name 'site_size_data.csv'"))
  }

  # Sort files by modification date to get the most recent and read this into session
  latest_size_file <- site_size_files[order(file.info(site_size_files)$mtime, decreasing = TRUE)[1]]
  site_size_data <- read.csv(latest_size_file, check.names = FALSE)
  print(paste0("Latest site size data file: ", latest_size_file))





  main_data_files <- list.files(path = raw_data_path, pattern = "Main_Data", full.names = TRUE)

  if (length(main_data_files) < 1) {
    cat(paste0("There is no Main_Data file in ", raw_data_path, ". This should have been produced in the first stage of the pipeline."))
  }

  # Sort files by modification date to get the most recent and read this into session
  latest_main_data <- main_data_files[order(file.info(main_data_files)$mtime, decreasing = TRUE)[1]]
  main_data <- read.csv(latest_main_data, check.names = FALSE)
  print(paste0("Latest main data file: ", latest_main_data))

  return(list(
    tree_data = tree_data,
    site_size_data = site_size_data,
    main_data = main_data,
    raw_data_path = raw_data_path
  ))
}





#' 2. Scale Tree Count to Monitoring Plot Size
#'
#' This function section scales up the tree counts based on logic that takes into
#' consideration the table, the resample numbers, and so on.
#' @param tree_data The tree data
#' @return tree data with new fields for scaled_count


scale_tree_count_to_monitoring_plot <- function(tree_data) {
  scaled_data <- tree_data %>%
    # This block of code is important. Depending on the monitoring plot size, as
    # well as conditions like whether or not it is a census plot, the counts will
    # be scaled up (in the case of 1x1, 3x3, or 10x10) or down (in the case of
    # 30x30 plots where resampling was required). The amount of scaling depends
    # on the number of times the plot was resampled.
    mutate(scaled_count = case_when(
      # For 30x30 plots that are census plots of all middle sized tres, tree
      # count is simply returned as is.
      (Plot_Size == "30x30" & grepl("census", origin_table, ignore.case = TRUE)) ~ Tree_Count,

      # For 30x30 plots that are not census plots, tree count is scaled down by
      # the number of times it was resampled + 1 (i.e. if it was resampled zero
      # times, the count is divided by 1, if it was resampled once, the count is
      # halved, and if it was resampled twice, the count is cut by a third)
      (Plot_Size == "30x30" & !grepl("census", origin_table, ignore.case = TRUE)) ~ Tree_Count / (Resample_Main_Plot + 1),
      (Plot_Size == "30x15" & grepl("census", origin_table, ignore.case = TRUE)) ~ Tree_Count * 2,
      (Plot_Size == "30x15" & !grepl("census", origin_table, ignore.case = TRUE)) ~ Tree_Count / (Resample_Main_Plot + 1) * 2,

      # For 10x0 control plots, the logic is the same as above with regard to
      # resampling, but the number is ten scaled up by 9 (100 sq m scaled up to
      # 900 sq m monitoring plot size)
      Plot_Size == "10x10" ~ (Tree_Count / (Resample_Main_Plot + 1)) * 9, # Change this if upscaling is not desired

      # The same resampling logic here, final scaling by 100 (Note: the monitoring
      # framework PDF makes a mistake in the way it says this should be calculated,
      # using the wrong units)
      # Another note is that this code implicitly handles Small_3x3 plots as well,
      # since even though these do not have subplots, prior code automatically
      # gives these rwos the value of zero.
      Plot_Size == "3x3" ~ Tree_Count / (Resample_3x3_Subplot + 1) * 100,

      # The same resampling logic here, final scaling by 900
      Plot_Size == "1x1" ~ Tree_Count * 900,
      TRUE ~ Tree_Count
    )) %>%
    mutate(scaled_count = ifelse(Tree_Type == "planted", Tree_Count, scaled_count)) %>%
    select(
      Species, Tree_Count,
      scaled_count,
      size_class,
      Country,
      Organization_Name,
      Site_ID, Plot_ID,
      Plot_Size,
      origin_table,
      everything()
    )
  return(scaled_data)
}




#' 3. Preprocess site size data
#'
#' This function section scales up the tree counts based on logic that takes into
#' consideration the table, the resample numbers, and so on.
#' @param site_size_data the loaded data from the manually placed CSV for site size
#' information
#' @return a dataframe, all_sites_combined, with the site size information preprocessed

preprocess_site_size_data <- function(site_size_data) {
  # Removes commas from Site_ID numbers and rename hectare field
  preprocessed_site_size_data <- site_size_data %>%
    mutate(
      Site_ID = gsub(",", "", Site_ID),
      site_size_ha = `Site Area (ha)`
    )

  # Some sites are broken into separate shapefiles/areas, resulting in multiple
  # rows in the size dataset. This combines them, summing the total area.
  all_sites_combined <- preprocessed_site_size_data %>%
    group_by(Site_ID) %>%
    summarize(site_size_ha = sum(as.numeric(`Site Area (ha)`), na.rm = TRUE)) %>%
    ungroup()
  return(all_sites_combined)
}




#' 4. Extrapolate to whole site
#'
#' This uses the new information to extrapolate counts to the whole site. It takes
#' the total number of trees at each site and extrapolates this directly. More
#' robust ways--using the mean of all monitoring plots--can be developed, but the
#' Monitoring framework worded it this way so this is how I wrote it.
#'
#' @param scaled_data the data from prior scaling function
#' @param preprocessed_site_data the data about site size from the preprocessing function
#' @param main_data the main data loaded in at the onset of the script
#' @return a list with: full_tree_data, full_main_data--both of which have the
#' site size information and relevant calculations added--and site_info, which contains
#' important information for extrapolation down the line.


extrapolate_to_whole_site <- function(scaled_data, preprocessed_site_size_data, main_data) {
  # Label total square meters based on plot size
  main_data <- main_data %>%
    mutate(plot_size_m_sq = case_when(
      Monitoring_Plot_Size == "30x30" ~ 900,
      Monitoring_Plot_Size == "3x3" ~ 9,
      Monitoring_Plot_Size == "10x10" ~ 100
    )) %>%
    # Remove errant newline characters
    mutate(Site_ID = gsub("\n", "", Site_ID))

  # Join kobo data to the site size data, labelling the kobo data with hectare info
  main_data_with_site_size <- left_join(main_data, select(preprocessed_site_size_data, Site_ID, site_size_ha), by = "Site_ID")

  # Produce a dataframe of site information for later calculations
  site_info <- main_data_with_site_size %>%
    group_by(Site_ID) %>%
    summarise(
      # Total area monitored is the sum of all monitoring plot areas (labelled above)
      total_area_monitored = sum(plot_size_m_sq),
      # Change character to number
      site_size_ha = as.numeric(site_size_ha),
      # Metric conversion hectare to sq meter
      site_size_m_sq = site_size_ha * 10000,
      # Sample ration is the area monitored over the total site size
      sample_ratio = total_area_monitored / site_size_m_sq,
      # Total number of monitoring plots per site
      total_number_plots = n()
    ) %>%
    distinct()

  # Join new site info to the kobo submission data
  full_main_data <- main_data_with_site_size %>% left_join(select(site_info, -site_size_ha), by = "Site_ID")

  # Join site info to the tree data
  full_tree_data <- left_join(scaled_data, select(site_info, Site_ID, sample_ratio, total_number_plots), by = "Site_ID")

  # Calculate extrapolated count by dividing by the sample ration
  full_tree_data <- full_tree_data %>%
    mutate(extrapolated_count = scaled_count / sample_ratio)

  return(list(full_tree_data = full_tree_data, full_main_data = full_main_data, site_info = site_info))
}



#' 5. Generate Baseline Reports
#'
#' This generates baseline reports at the site and species level.
#'
#' @param extrapolated_data the data with extrapolated counts
#' @return a list with baseline reports at the site and species level

generate_baseline_reports <- function(extrapolated_data) {
  # Baseline: only Y0
  data_Y0 <- extrapolated_data %>% filter(Timeframe == "Y0")

  results_by_site <- data_Y0 %>%
    # Remove planted trees that aren't in the planted table (safe assumption at Y0)
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>%
    # Label NA tree type as unknown
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>%
    # Remove species labelled as 'None'
    filter(Species != "None") %>%
    # Remove data for which there is no sample ratio (sites that weren't included in
    # the site size info)
    filter(!is.na(sample_ratio)) %>%
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      # Assumes anything at Y0 labelled as naturally regenerating was already present
      Timeframe == "Y0" & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    # Group to site level and sum
    group_by(Organization_Name, Site_ID, Tree_Type_Group, size_class) %>%
    summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = extrapolated_count)

  results_by_site_and_species <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>%
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>%
    filter(Species != "None") %>%
    filter(!is.na(sample_ratio)) %>%
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == "Y0" & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Tree_Type_Group, size_class, Species) %>%
    summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = extrapolated_count)


  return(list(
    results_by_site = results_by_site,
    results_by_site_and_species = results_by_site_and_species
  ))
}






#' 6. Generate Indicator Reports
#'
#' This generates reports for the PPC indicators, including survival of planted
#' trees, trees restored, and trees naturally regenerated. At the time of creation,
#' the data does not have any entries from Y2.5 and Y5, so these outputs will either
#' be blank or very short.
#'
#' @param extrapolated_data the data with extrapolated counts
#' @return a list with indicator reports for survial, trees restored, and trees
#' naturally regenerated--all at both the site and species level
generate_indicator_reports <- function(extrapolated_data) {
  # Process data for survival calculation
  processed_data <- extrapolated_data %>%
    # Filter out Y0 trees that were 'planted' but not in planted tables
    filter(!(Timeframe == "Y0" & Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>%
    # Filter out <1cm trees, not wanted in analysis
    filter(!size_class == "<1cm") %>%
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>%
    filter(Species != "None") %>%
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == "Y0" & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    filter(Tree_Type_Group == "Planted")


  # Calculate counts of planted trees at each timeframe, calculate survival
  results_planted_survival <- processed_data %>%
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Timeframe, values_from = extrapolated_count)

  if (!"Y2.5" %in% names(results_planted_survival)) {
    results_planted_survival$Y2.5 <- NA
  }
  if (!"Y5" %in% names(results_planted_survival)) {
    results_planted_survival$Y5 <- NA
  }

  results_planted_survival <- results_planted_survival %>%
    mutate(
      Y2.5_Survival = Y2.5 / Y0,
      Y5_Survival = Y5 / Y0
    )


  # Repeat at species level
  results_planted_survival_species <- processed_data %>%
    group_by(Organization_Name, Site_ID, Species, Timeframe) %>%
    summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Timeframe, values_from = extrapolated_count)

  if (!"Y2.5" %in% names(results_planted_survival_species)) {
    results_planted_survival_species$Y2.5 <- NA
  }
  if (!"Y5" %in% names(results_planted_survival_species)) {
    results_planted_survival_species$Y5 <- NA
  }

  results_planted_survival_species <- results_planted_survival_species %>%
    mutate(
      Y2.5_Survival = Y2.5 / Y0,
      Y5_Survival = Y5 / Y0
    )




  # Get baseline counts of present trees (including natregen!), which will be
  # subtracted from later counts.
  baseline_trees <- extrapolated_data %>%
    filter(Timeframe == "Y0", Tree_Type %in% c("Present", "naturally_regenerating")) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID) %>%
    summarise(count_Y0 = sum(extrapolated_count))

  # Calculate trees at Y2.5 and Y5 (Planted and Naturally Regenerating). IGNORING
  # <1cm!
  trees_at_timeframe_2 <- extrapolated_data %>%
    filter(
      Timeframe == "Y2.5",
      Tree_Type %in% c("planted", "naturally_regenerating")
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>%
    select(-Timeframe)


  trees_at_timeframe_5 <- extrapolated_data %>%
    filter(
      Timeframe == "Y5",
      Tree_Type %in% c("planted", "naturally_regenerating")
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>%
    select(-Timeframe)


  trees_combined <- baseline_trees %>%
    full_join(trees_at_timeframe_2, by = c("Organization_Name", "Site_ID")) %>%
    full_join(trees_at_timeframe_5, by = c("Organization_Name", "Site_ID"))

  trees_restored <- trees_combined %>%
    mutate(
      restored_Y25 = count_Y25 - count_Y0,
      restored_Y5 = count_Y5 - count_Y0
    )

## BEGIN TEST
  baseline_trees_sc <- extrapolated_data %>%
    filter(Timeframe == "Y0", Tree_Type %in% c("Present", "naturally_regenerating")) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, size_class) %>%
    summarise(count_Y0 = sum(extrapolated_count))
  
  # Calculate trees at Y2.5 and Y5 (Planted and Naturally Regenerating). IGNORING
  # <1cm!
  trees_at_timeframe_2_sc <- extrapolated_data %>%
    filter(
      Timeframe == "Y2.5",
      Tree_Type %in% c("planted", "naturally_regenerating")
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name,  Site_ID, size_class, Timeframe) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>%
    select(-Timeframe)
  
  
  trees_at_timeframe_5_sc <- extrapolated_data %>%
    filter(
      Timeframe == "Y5",
      Tree_Type %in% c("planted", "naturally_regenerating")
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, size_class, Timeframe) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>%
    select(-Timeframe)
  
  
  trees_combined_sc <- baseline_trees_sc %>%
    full_join(trees_at_timeframe_2_sc, by = c("Organization_Name", "Site_ID")) %>%
    full_join(trees_at_timeframe_5_sc, by = c("Organization_Name", "Site_ID"))
  
  trees_restored_sc <- trees_combined_sc %>%
    mutate(
      restored_Y25 = count_Y25 - count_Y0,
      restored_Y5 = count_Y5 - count_Y0
    )
  
## END TEST

  # Count natregen at timeframe Y2.5 and Y5
  natregen_at_timeframe_2 <- extrapolated_data %>%
    filter(
      Timeframe == "Y2.5",
      Tree_Type == "naturally_regenerating"
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>%
    select(-Timeframe)

  natregen_at_timeframe_5 <- extrapolated_data %>%
    filter(
      Timeframe == "Y5",
      Tree_Type == "naturally_regenerating"
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>%
    select(-Timeframe)


  natregen_combined <- natregen_at_timeframe_2 %>%
    full_join(natregen_at_timeframe_5, by = c("Organization_Name", "Site_ID"))






  # Repeat all above, but at species level
  baseline_trees_species <- extrapolated_data %>%
    filter(Timeframe == "Y0", Tree_Type %in% c("Present", "naturally_regenerating")) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, Species) %>%
    summarise(count_Y0 = sum(extrapolated_count))


  trees_at_timeframe_2_species <- extrapolated_data %>%
    filter(
      Timeframe == "Y2.5",
      Tree_Type %in% c("planted", "naturally_regenerating")
    ) %>%
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>%
    ungroup() %>%
    select(-Timeframe)


  trees_at_timeframe_5_species <- extrapolated_data %>%
    filter(
      Timeframe == "Y5",
      Tree_Type %in% c("planted", "naturally_regenerating")
    ) %>%
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>%
    ungroup() %>%
    select(-Timeframe)


  trees_combined_species <- baseline_trees_species %>%
    full_join(trees_at_timeframe_2_species, by = c("Organization_Name", "Site_ID", "Species")) %>%
    full_join(trees_at_timeframe_5_species, by = c("Organization_Name", "Site_ID", "Species"))

  trees_restored_species <- trees_combined_species %>%
    mutate(
      restored_Y25 = count_Y25 - count_Y0,
      restored_Y5 = count_Y5 - count_Y0
    )


  natregen_timeframe_2_species <- extrapolated_data %>%
    filter(
      Timeframe == "Y2.5",
      Tree_Type == "naturally_regenerating"
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>%
    ungroup() %>%
    select(-Timeframe)

  natregen_at_timeframe_5_species <- extrapolated_data %>%
    filter(
      Timeframe == "Y5",
      Tree_Type == "naturally_regenerating"
    ) %>%
    filter(!size_class == "<1cm") %>%
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>%
    ungroup() %>%
    select(-Timeframe)


  natregen_combined_species <- natregen_timeframe_2_species %>%
    full_join(natregen_at_timeframe_5_species, by = c("Organization_Name", "Site_ID", "Species"))




  return(list(
    results_planted_survival = results_planted_survival,
    results_planted_survival_species = results_planted_survival_species,
    trees_restored = trees_restored,
    trees_restored_species = trees_restored_species,
    trees_natreggen = natregen_combined,
    trees_natregen_species = natregen_combined_species,
    trees_restored_sizeclass = trees_restored_sc
  ))
}

#' 7. CSV Writing Utility Scripts
#'
#' Same utility scripts from other parts of the pipeline. These just simplify the
#' process of writing and naming outputs.
#'

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

# 1. Load Data
message("Loading Data...")
all_data <- load_data()

# 2. Scale tree counts
message("Scaling tree counts to 30mx30m (900m^2)")
scaled_data <- scale_tree_count_to_monitoring_plot(all_data$tree_data)

# 3. Preprocess site size data
message("Preprocessing site size data...")
preprocessed_site_size_data <- preprocess_site_size_data(site_size_data = all_data$site_size_data)

# 4. Extrapolate to site size
message("Scaling tree counts to full site if size data was available...")
extrapolated_data <- extrapolate_to_whole_site(scaled_data, preprocessed_site_size_data, all_data$main_data)

# 5. Create baseline reports
message("Creating baseline reports of tree counts extrapolated to full site size...")
extrapolated_baseline_reports <- generate_baseline_reports(extrapolated_data$full_tree_data)

# 6. Create Indicator Reports
message("Creating indicator reports of tree counts extrapolated to full site size...")
indicator_reports <- generate_indicator_reports(extrapolated_data = extrapolated_data$full_tree_data)



# 7. Write reports to CSV (including master report)
message("Writing reports to disk...")
write_list_to_csv(extrapolated_baseline_reports, names(extrapolated_baseline_reports), main_dir = all_data$raw_data_path, sub_dir = "Baseline_Reports")
write_list_to_csv(indicator_reports, names(indicator_reports), main_dir = all_data$raw_data_path, sub_dir = "Indicator_Reports")
write_to_csv(extrapolated_data$full_tree_data, prefix = "Master_Report", main_dir = all_data$raw_data_path)










# This is some starter script if you ever wanted to calculate the counts with error
# For now, this script will execute and save something in the R environment, but
# it will not save anything to your disk.
results_with_error <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>%
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>%
  filter(Species != "None") %>%
  filter(!is.na(sample_ratio)) %>%
  group_by(Organization_Name, Site_ID, size_class) %>%
  summarise(
    mean_count = mean(scaled_count, na.rm = TRUE),
    sd_count = sd(scaled_count, na.rm = TRUE)
  ) %>%
  left_join(extrapolated_data$site_info, by = "Site_ID") %>%
  mutate(
    standard_error = sd_count / sqrt(total_number_plots),
    estimated_total = mean_count / sample_ratio,
    error_margin_site = standard_error / sample_ratio
  )
