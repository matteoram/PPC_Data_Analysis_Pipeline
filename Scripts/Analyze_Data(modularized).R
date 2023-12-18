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
  
  if(length(tree_files) < 1){
    cat(paste0("There is no Final_Tree_Data file in ", raw_data_path, ". Make sure you've run the Add_Family_Names.R script first!" ))
  }
  
  # Sort files by modification date to get the most recent and read this into session
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]
  tree_data <- read.csv(latest_tree_file, check.names = FALSE)
  print(paste0("Latest tree data file: ", latest_tree_file))
  
  site_size_files <- list.files(pattern = "site_size", full.names = TRUE)
  
  if(length(site_size_files) < 1){
    cat(paste0("There is no site_size file in ", raw_data_path, ". You need to download the site size data and save it as a CSV with the name 'site_size_data.csv'" ))
  }
  
  # Sort files by modification date to get the most recent and read this into session
  latest_size_file <- site_size_files[order(file.info(site_size_files)$mtime, decreasing = TRUE)[1]]
  site_size_data <- read.csv(latest_size_file, check.names = FALSE)
  print(paste0("Latest site size data file: ", latest_size_file))
  
  
  
  
  
  main_data_files <- list.files(path = raw_data_path, pattern = "Main_Data", full.names = TRUE)
  
  if(length(main_data_files) < 1){
    cat(paste0("There is no Main_Data file in ", raw_data_path, ". This should have been produced in the first stage of the pipeline." ))
  }
  
  # Sort files by modification date to get the most recent and read this into session
  latest_main_data <- main_data_files[order(file.info(main_data_files)$mtime, decreasing = TRUE)[1]]
  main_data <- read.csv(latest_main_data, check.names = FALSE)
  print(paste0("Latest main data file: ", latest_main_data))
  
  return(list(tree_data = tree_data, 
              site_size_data = site_size_data,
              main_data = main_data,
              raw_data_path = raw_data_path))
}





## !!! CHECK SMALL 3x3 stuff! it is not the same as a subplot and should not be impacted by the second resample number

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
      (Plot_Size == "30x30" & grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count,
      
      # For 30x30 plots that are not census plots, tree count is scaled down by
      # the number of times it was resampled + 1 (i.e. if it was resampled zero
      # times, the count is divided by 1, if it was resampled once, the count is 
      # halved, and if it was resampled twice, the count is cut by a third)
      (Plot_Size == "30x30" & !grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count / (Resample_Main_Plot + 1),
      
      (Plot_Size == "30x15" & grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count * 2,
      (Plot_Size == "30x15" & !grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count / (Resample_Main_Plot + 1) * 2,
      
      # For 10x0 control plots, the logic is the same as above with regard to
      # resampling, but the number is ten scaled up by 9 (100 sq m scaled up to
      # 900 sq m monitoring plot size)
      Plot_Size == "10x10" ~ (Tree_Count / (Resample_Main_Plot + 1)) * 9, # Change this if upscaling is not desired
      
      # The same resampling logic here, final scaling by 100 (Note: the monitoring
      # framework PDF makes a mistake in the way it says this should be calculated)
      Plot_Size == "3x3" ~ Tree_Count / (Resample_3x3_Subplot + 1) * 100,
      
      # The same resampling logic here, final scaling by 900
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



preprocess_site_size_data <- function(site_size_data){

  
  # # THIS SHOULD NOT HAVE TO EXIST!
  # distinct_site_data <- preprocessed_site_size_data %>%
  #   distinct(Site_ID, .keep_all = TRUE)
  
  org_name_mapping <- data.frame(
    names_from_size_dataset = c(
      "EMA", "GANB", "ISA", "CI-Cambodia","Faja Lobi", "ReForest Action (Chantilly)", 
      "FEDECOVERA", "Grow Trees India", "Greenbelt Movement", "CI Madagascar", 
      "Wells for Zoe (Malawi)", "COOPERTIVA AMBIO", "FONCET/ANP FRAILESCANA", 
      "ReForest Action (Proenca-a-Nova)", "ReForest Action (Palencia)",
      "Greening Australia (GSL)","CERT", "CI Philippines", "CI COLOMBIA", 
      "GANB - Flagship", "Martin County Legacy Coal Mine Restoration Project", 
      "MDPS - Flagship", "CICLOS - Flagship"),
    corrected_names = c("EMA", "GANB", "ISA", "CI-Cambodia", "Faja Lobi", 
                        "Reforest Action",  "Fedecovera","Grow Trees India",
                        "Greenbelt Movement", "CI Madagascar", "Wells for Zoe",
                        "AMBIO", "FONCET", "Reforest Action", "Reforest Action",
                        "Greening Australia (GSL)", "CERT", "CI Philippines",
                        "CI COLOMBIA", "GANB","Martin County Legacy Coal Mine Restoration Project",
                        "MDPS - Flagship", "CICLOS - Flagship")
      )
  updated_site_data <- site_size_data %>%
    left_join(org_name_mapping, by = c("Org_Name" = "names_from_size_dataset"))
  
  preprocessed_site_size_data <- updated_site_data %>% 
    mutate(Site_ID = gsub(",", "", Site_ID),
           site_size_ha = `Site Area (ha)`,
           org_site_ID = paste(corrected_names, Site_ID, sep = "_")) 
    # This should not have to exist
    # distinct(org_site_ID, .keep_all = TRUE)
  
  all_sites_combined <- preprocessed_site_size_data %>%
    group_by(Site_ID, corrected_names) %>%
    summarize(site_size_ha = sum(as.numeric(`Site Area (ha)`), na.rm = TRUE)) %>% 
    mutate(org_site_ID = paste(corrected_names, Site_ID, sep = "_")) %>% 
    ungroup()
  
}





preprocess_site_size_data_v2 <- function(site_size_data){
  

  
  preprocessed_site_size_data <- site_size_data %>% 
    mutate(Site_ID = gsub(",", "", Site_ID),
           site_size_ha = `Site Area (ha)`) 

  
  all_sites_combined <- preprocessed_site_size_data %>%
    group_by(Site_ID) %>%
    summarize(site_size_ha = sum(as.numeric(`Site Area (ha)`), na.rm = TRUE)) %>% 
    ungroup()
  
}




extrapolate_to_whole_site <- function(scaled_data, preprocessed_site_size_data, main_data){
  

  main_data <- main_data %>% 
  mutate(org_site_ID = paste(Organization_Name, Site_ID, sep = "_")) %>% 
  mutate(plot_size_m_sq = case_when(
    Monitoring_Plot_Size == '30x30' ~ 900,
    Monitoring_Plot_Size == '3x3' ~ 9,
    Monitoring_Plot_Size == '10x10'~100
    )
  ) %>% 
  mutate(Site_ID = gsub("\n", "", Site_ID))
 

main_data_with_site_size <- left_join(main_data, select(preprocessed_site_size_data, org_site_ID, site_size_ha), by = "org_site_ID")


site_info <- main_data_with_site_size %>% 
  group_by(org_site_ID) %>% 
  summarise(total_area_monitored = sum(plot_size_m_sq),
            site_size_ha  = as.numeric(site_size_ha),
            site_size_m_sq = site_size_ha*10000,
            sample_ratio = total_area_monitored/site_size_m_sq,
            total_number_plots = n()
            ) %>% 
  distinct()
# site_info <- site_info %>% 
#   left_join(select(main_data_with_site_size, 
#                    Country,
#                    Organization_Name, 
#                    Site_ID, 
#                    country_site_ID), 
#             by = "country_site_ID") %>% 
#   distinct()

full_main_data <- main_data_with_site_size %>% left_join(select(site_info, -site_size_ha), by = "org_site_ID")

scaled_data <- scaled_data %>% mutate(org_site_ID = paste(Organization_Name, Site_ID, sep = "_"))
full_tree_data <- left_join(scaled_data, select(site_info, org_site_ID, sample_ratio, total_number_plots), by = "org_site_ID")


full_tree_data <- full_tree_data %>% 
  mutate(extrapolated_count = scaled_count/sample_ratio) 

return(list(full_tree_data = full_tree_data, full_main_data = full_main_data, site_info = site_info))

}





extrapolate_to_whole_site_v2 <- function(scaled_data, preprocessed_site_size_data, main_data){
  
  
  main_data <- main_data %>% 
    mutate(plot_size_m_sq = case_when(
      Monitoring_Plot_Size == '30x30' ~ 900,
      Monitoring_Plot_Size == '3x3' ~ 9,
      Monitoring_Plot_Size == '10x10'~100
    )
    ) %>% 
    mutate(Site_ID = gsub("\n", "", Site_ID))
  
  
  main_data_with_site_size <- left_join(main_data, select(preprocessed_site_size_data, Site_ID, site_size_ha), by = "Site_ID")
  
  
  site_info <- main_data_with_site_size %>% 
    group_by(Site_ID) %>% 
    summarise(total_area_monitored = sum(plot_size_m_sq),
              site_size_ha  = as.numeric(site_size_ha),
              site_size_m_sq = site_size_ha*10000,
              sample_ratio = total_area_monitored/site_size_m_sq,
              total_number_plots = n()
    ) %>% 
    distinct()

  
  full_main_data <- main_data_with_site_size %>% left_join(select(site_info, -site_size_ha), by = "Site_ID")
  
  full_tree_data <- left_join(scaled_data, select(site_info, Site_ID, sample_ratio, total_number_plots), by = "Site_ID")
  
  
  full_tree_data <- full_tree_data %>% 
    mutate(extrapolated_count = scaled_count/sample_ratio) 
  
  return(list(full_tree_data = full_tree_data, full_main_data = full_main_data, site_info = site_info))
  
}





generate_baseline_reports_v2 <- function(data) {
  
  data_Y0 <- data %>% filter(Timeframe == 'Y0')
  
  results_by_site <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    filter(!is.na(sample_ratio)) %>%
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name,Site_ID, Tree_Type_Group, size_class) %>%
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
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name,Site_ID, Tree_Type_Group, size_class, Species) %>%
    summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
    
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = extrapolated_count) 
  
  
  return(list(
    results_by_site = results_by_site,
    results_by_site_and_species = results_by_site_and_species)
  )
  
}

















generate_monitoring_plot_baseline_reports <- function(data) {
  
  data_Y0 <- data %>% filter(Timeframe == 'Y0')
  
  results_by_species <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
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
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Plot_ID, Tree_Type_Group, size_class) %>%
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
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Site_ID, Tree_Type_Group, size_class) %>%
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
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Country, Tree_Type_Group, size_class) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  results_by_org <- data_Y0 %>%
    filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    group_by(Organization_Name, Tree_Type_Group, size_class) %>%
    summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
    replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
  
  
  return(list(
    Baseline_Results_by_Species = results_by_species,
    Baseline_Results_by_Family = results_by_family,
    Baseline_Results_by_SizeClass = results_by_size_class,
    Baseline_Results_by_Plot = results_by_plot,
    Baseline_Results_by_Site = results_by_site,
    Baseline_Results_by_Country = results_by_country,
    Baseline_Results_by_Org = results_by_org))
  
}


# Trees planted, plus trees that are naturally regenerating
generate_indicator_reports <- function(extrapolated_data){
  
  # Process data for survival calculation
  processed_data <- extrapolated_data %>%
    filter(!(Timeframe == 'Y0' & Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
    filter(!size_class == "<1cm") %>% 
    mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
    filter(Species != "None") %>% 
    mutate(Tree_Type_Group = case_when(
      Tree_Type == "planted" ~ "Planted",
      Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
      Tree_Type == "don_t_know" ~ "Unknown"
    )) %>%
    filter(Tree_Type_Group == "Planted")
  
  
  
  results_planted_survival <- processed_data %>% 
    group_by(Organization_Name,Site_ID, Timeframe) %>%
    summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Timeframe, values_from = extrapolated_count) 
  
  if(!'Y2.5' %in% names(results_planted_survival)){
    results_planted_survival$Y2.5 <- NA
  }
  if(!'Y5' %in% names(results_planted_survival)){
    results_planted_survival$Y5 <- NA
  }
  
  results_planted_survival <- results_planted_survival %>% 
    mutate(Y2.5_Survival = Y2.5/Y0,
           Y5_Survival = Y5/Y0)
  
  
  
  results_planted_survival_species <- processed_data %>%
    group_by(Organization_Name,Site_ID, Species, Timeframe) %>%
    summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = Timeframe, values_from = extrapolated_count) 
  
  if(!'Y2.5' %in% names(results_planted_survival_species)){
    results_planted_survival_species$Y2.5 <- NA
  }
  if(!'Y5' %in% names(results_planted_survival_species)){
    results_planted_survival_species$Y5 <- NA
  }
  
  results_planted_survival_species <- results_planted_survival_species %>% 
    mutate(Y2.5_Survival = Y2.5/Y0,
           Y5_Survival = Y5/Y0)
  

  
  
  
  baseline_trees <- extrapolated_data %>%
    filter(Timeframe == 'Y0', Tree_Type %in% c("Present", "naturally_regenerating")) %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID) %>%
    summarise(count_Y0 = sum(extrapolated_count))
  
  # Step 2: Calculate trees at Y2.5 and Y5 (Planted and Naturally Regenerating)
  trees_at_timeframe_2 <- extrapolated_data %>%
    filter(Timeframe == 'Y2.5', 
           Tree_Type %in% c("planted", "naturally_regenerating")) %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y25 = sum(extrapolated_count))%>% 
    select(-Timeframe)
  
  
  trees_at_timeframe_5 <- extrapolated_data %>%
    filter(Timeframe == 'Y5', 
           Tree_Type %in% c("planted", "naturally_regenerating")) %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>% 
    select(-Timeframe)

  
  trees_combined <- baseline_trees %>%
    full_join(trees_at_timeframe_2, by = c("Organization_Name", "Site_ID")) %>%
    full_join(trees_at_timeframe_5, by = c("Organization_Name", "Site_ID"))
  
  trees_restored <- trees_combined %>% 
    mutate(restored_Y25 = count_Y25 - count_Y0,
           restored_Y5 = count_Y5 - count_Y0)
  
  
  
  
  natregen_at_timeframe_2 <- extrapolated_data %>%
    filter(Timeframe == 'Y2.5', 
           Tree_Type == "naturally_regenerating") %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>% 
    select(-Timeframe)
  
  natregen_at_timeframe_5 <- extrapolated_data %>%
    filter(Timeframe == 'Y5', 
           Tree_Type == "naturally_regenerating") %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID, Timeframe) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>% 
    select(-Timeframe)
  
  
  natregen_combined <- natregen_at_timeframe_2 %>%
    full_join(natregen_at_timeframe_5, by = c("Organization_Name", "Site_ID"))
  

  
  
  
  
  
  baseline_trees_species <- extrapolated_data %>%
    filter(Timeframe == 'Y0', Tree_Type %in% c("Present", "naturally_regenerating")) %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID, Species) %>%
    summarise(count_Y0 = sum(extrapolated_count))
  
  # Step 2: Calculate trees at Y2.5 and Y5 (Planted and Naturally Regenerating)
  trees_at_timeframe_2_species <- extrapolated_data %>%
    filter(Timeframe == 'Y2.5', 
           Tree_Type %in% c("planted", "naturally_regenerating")) %>%
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>% 
    ungroup() %>% 
    select(-Timeframe)
  
  
  trees_at_timeframe_5_species <- extrapolated_data %>%
    filter(Timeframe == 'Y5', 
           Tree_Type %in% c("planted", "naturally_regenerating")) %>%
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>% 
    ungroup() %>% 
    select(-Timeframe)
  
  
  trees_combined_species <- baseline_trees_species %>%
    full_join(trees_at_timeframe_2_species, by = c("Organization_Name", "Site_ID", "Species")) %>%
    full_join(trees_at_timeframe_5_species, by = c("Organization_Name", "Site_ID", "Species"))
  
  trees_restored_species <- trees_combined_species %>% 
    mutate(restored_Y25 = count_Y25 - count_Y0,
           restored_Y5 = count_Y5 - count_Y0)
  
 
  natregen_timeframe_2_species <- extrapolated_data %>%
    filter(Timeframe == 'Y2.5', 
           Tree_Type == "naturally_regenerating") %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y25 = sum(extrapolated_count)) %>% 
    ungroup() %>% 
    select(-Timeframe)
  
  natregen_at_timeframe_5_species <- extrapolated_data %>%
    filter(Timeframe == 'Y5', 
           Tree_Type == "naturally_regenerating") %>%
    filter(!size_class == "<1cm") %>% 
    group_by(Organization_Name, Site_ID, Timeframe, Species) %>%
    summarise(count_Y5 = sum(extrapolated_count)) %>% 
    ungroup %>% 
    select(-Timeframe)
  
  
  natregen_combined_species <- natregen_timeframe_2_species %>%
    full_join(natregen_at_timeframe_5_species, by = c("Organization_Name", "Site_ID", "Species"))
  
  
  
  
  
  
  
  
  
  
  
  
  return(list(results_planted_survival = results_planted_survival,
              results_planted_survival_species = results_planted_survival_species,
              trees_restored = trees_restored))
  
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

scaled_data <- scale_tree_count_to_monitoring_plot(all_data$tree_data)

preprocessed_site_size_data <- preprocess_site_size_data_v2(site_size_data = all_data$site_size_data)

extrapolated_data <- extrapolate_to_whole_site_v2(scaled_data, preprocessed_site_size_data, all_data$main_data)

extrapolated_baseline_reports <- generate_baseline_reports_v2(extrapolated_data$full_tree_data)


monitoring_plot_baseline_reports <- generate_monitoring_plot_baseline_reports(scaled_data)



write_list_to_csv(BL_reports, names(BL_reports),main_dir = all_data$raw_data_path, sub_dir = "Baseline_Reports")
write_to_csv(scaled_data, prefix = "Master_Report", main_dir = all_data$raw_data_path, sub_dir = "Baseline_Reports")












# 
# generate_baseline_reports <- function(data) {
#   
#   data_Y0 <- data %>% filter(Timeframe == 'Y0')
#   
#   results_by_species <- data_Y0 %>%
#     filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
#     mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
#     filter(Species != "None") %>% 
#     mutate(Tree_Type_Group = case_when(
#       Tree_Type == "planted" ~ "Planted",
#       Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
#       Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
#       Tree_Type == "don_t_know" ~ "Unknown"
#     )) %>%
#     group_by(Organization_Name, Site_ID, Plot_ID, size_class, Species, Tree_Type_Group) %>%
#     summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
#     replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
#   
#   
#   
#   
#   results_by_family <- data_Y0 %>%
#     filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
#     mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
#     filter(Species != "None") %>% 
#     mutate(Tree_Type_Group = case_when(
#       Tree_Type == "planted" ~ "Planted",
#       Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
#       Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
#       Tree_Type == "don_t_know" ~ "Unknown"
#     )) %>%
#     group_by(Organization_Name, Site_ID, Plot_ID, size_class, family, Tree_Type_Group) %>%
#     summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
#     replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
#   
#   
#   results_by_size_class <- data_Y0 %>%
#     filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
#     mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
#     filter(Species != "None") %>% 
#     mutate(Tree_Type_Group = case_when(
#       Tree_Type == "planted" ~ "Planted",
#       Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
#       Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
#       Tree_Type == "don_t_know" ~ "Unknown"
#     )) %>%
#     group_by(Organization_Name, Site_ID, Plot_ID,size_class, Tree_Type_Group) %>%
#     summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
#     replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
#   
#   
#   
#   
#   
#   results_by_plot <- data_Y0 %>%
#     filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
#     mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
#     filter(Species != "None") %>% 
#     mutate(Tree_Type_Group = case_when(
#       Tree_Type == "planted" ~ "Planted",
#       Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
#       Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
#       Tree_Type == "don_t_know" ~ "Unknown"
#     )) %>%
#     group_by(Organization_Name, Site_ID, Plot_ID, Tree_Type_Group) %>%
#     summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
#     replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
#   
#   
#   
#   
#   
#   
#   results_by_site <- data_Y0 %>%
#     filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
#     mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
#     filter(Species != "None") %>% 
#     mutate(Tree_Type_Group = case_when(
#       Tree_Type == "planted" ~ "Planted",
#       Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
#       Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
#       Tree_Type == "don_t_know" ~ "Unknown"
#     )) %>%
#     group_by(Organization_Name, Site_ID, Tree_Type_Group) %>%
#     summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
#     replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
#   
#   
#   
#   results_by_country<- data_Y0 %>%
#     filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
#     mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
#     filter(Species != "None") %>% 
#     mutate(Tree_Type_Group = case_when(
#       Tree_Type == "planted" ~ "Planted",
#       Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
#       Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
#       Tree_Type == "don_t_know" ~ "Unknown"
#     )) %>%
#     group_by(Country, Tree_Type_Group) %>%
#     summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
#     replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
#   
#   results_by_org <- data_Y0 %>%
#     filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
#     mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
#     filter(Species != "None") %>% 
#     mutate(Tree_Type_Group = case_when(
#       Tree_Type == "planted" ~ "Planted",
#       Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
#       Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
#       Tree_Type == "don_t_know" ~ "Unknown"
#     )) %>%
#     group_by(Organization_Name, Tree_Type_Group) %>%
#     summarise(tree_count = sum(scaled_count, na.rm = TRUE)) %>%
#     ungroup() %>%
#     pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
#     replace_na(list(Planted = 0, `Already Present` = 0, Unknown = 0))
#   
#   
#   return(list(
#     Results_by_Species = results_by_species,
#     Results_by_Family = results_by_family,
#     Results_by_SizeClass = results_by_size_class,
#     Results_by_Plot = results_by_plot,
#     Results_by_Site = results_by_site,
#     Results_by_Country = results_by_country,
#     Results_by_Org = results_by_org))
#   
# }
# 



mada_baseline <- mada1795 %>% 
  filter(Timeframe == 'Y0' & !(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  mutate(extrapolated_count = scaled_count/sample_ratio)


results_by_org <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  group_by(Organization_Name, Tree_Type_Group, size_class) %>%
  summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%

  ungroup() %>%
  pivot_wider(names_from = Tree_Type_Group, values_from = extrapolated_count) 






results_by_site <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  group_by(Organization_Name,Site_ID, Tree_Type_Group, size_class) %>%
  summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
  
  ungroup() %>%
  pivot_wider(names_from = Tree_Type_Group, values_from = extrapolated_count) 






check <-extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>% 
  group_by(org_site_ID, size_class) %>% 
  summarise(mean_count = mean(scaled_count, na.rm = TRUE),
            sd_count = sd(scaled_count, na.rm = TRUE)) %>% 
  left_join(extrapolated_data$site_info, by = "org_site_ID") %>% 
  mutate(standard_error = sd_count/sqrt(total_number_plots),
         estimated_total = mean_count/sample_ratio,
         error_margin_site = standard_error / sample_ratio)


subsettest <- check %>% filter(size_class == ">10cm")
subsettest <- subsettest[1:20,]











results_by_org_short <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  group_by(Organization_Name, Tree_Type_Group, size_class) %>%
  summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE))




results_by_site_presentonly <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  filter(Tree_Type_Group == "Already Present") %>% 
  group_by(Organization_Name,Site_ID, size_class) %>%
  summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = size_class, values_from = extrapolated_count) 






results_by_site_plantedonly <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  filter(Tree_Type_Group == "Planted") %>% 
  group_by(Organization_Name,Site_ID, size_class) %>%
  summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = size_class, values_from = extrapolated_count) 






results_by_site_present_only_species <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  filter(Tree_Type_Group == "Already Present") %>% 
  group_by(Organization_Name,Site_ID, size_class, Species) %>%
  summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = size_class, values_from = extrapolated_count) 





results_planted_survival <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  # filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  filter(Tree_Type_Group == "Planted") %>% 
  group_by(Organization_Name,Site_ID, Timeframe) %>%
  summarise(scaled_count = sum(scaled_count, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = Timeframe, values_from = scaled_count) 


results_planted_survival %>% 
  mutate(Y2.5_Survival = Y2.5/Y0,
         Y5_Survival = Y5/Y0)




results_by_site_presentonly_timeframe <- extrapolated_data$full_tree_data %>%
  filter(!(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>% 
  mutate(Tree_Type = ifelse(is.na(Tree_Type), "Unknown", Tree_Type)) %>% 
  filter(Species != "None") %>% 
  filter(!is.na(sample_ratio)) %>%
  mutate(Tree_Type_Group = case_when(
    Tree_Type == "planted" ~ "Planted",
    Timeframe == 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Already Present",
    Timeframe != 'Y0' & (Tree_Type %in% c("Present", "naturally_regenerating")) ~ "Naturally Regenerating",
    Tree_Type == "don_t_know" ~ "Unknown"
  )) %>%
  filter(Tree_Type_Group == "Already Present") %>% 
  group_by(Organization_Name,Site_ID, size_class, Timeframe) %>%
  summarise(extrapolated_count = sum(extrapolated_count, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = size_class, values_from = extrapolated_count) 



