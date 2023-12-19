tree_data <- read.csv("Main_Raw_Data\\Tree_Data_Uncorrected_2023-12-01.csv", check.names = F)
main_data <- read.csv("Main_Raw_Data\\Main_Data_2023-12-01.csv", check.names = F)


Plots_30x30_tree <- main_data_unc %>% filter(Resample_Main_Plot %in% c(0, 1))
Plots_30x30_tree_2<- main_data_unc %>% filter(Resample_Main_Plot == 2 & TreesPresent == "Yes") 
plots_should_have_3030_trees <- rbind(Plots_30x30_tree1, Plots_30x30_tree_2)

tree_data_3030 <- tree_data_unc %>% filter(origin_table %in% c("Normal_30x30", "Normal_30x30_2")) 


plots_with_missing_data <- setdiff(plots_should_have_3030_trees$Plot_ID, tree_data_3030_plots$Plot_ID)

plots_with_missing_data_3030 <- anti_join(plots_should_have_3030_trees, tree_data_3030, by = c("Site_ID", "Plot_ID"))




Plots_3x3_tree <- main_data_unc %>% filter(Resample_3x3_Subplot %in% c(0, 1))
Plots_3x3_tree_2<- main_data_unc %>% filter(Resample_3x3_Subplot == 2 & LittleTreesPresent == "Yes") 
plots_should_have_3x3_trees <- rbind(Plots_3x3_tree, Plots_3x3_tree_2)

tree_data_3x3 <- tree_data_unc %>% filter(origin_table %in% c("Nested_3x3_within_30x30", "Nested_3x3_within_10x10_Control",
                                                              "Nested_3x3_within_30x30_2", "Nested_3x3_within_10x10_Control_2")) 


plots_with_missing_data_3x3 <- setdiff(plots_should_have_3x3_trees$Plot_ID, tree_data_3030_plots$Plot_ID)

plots_with_missing_data_3x3 <- anti_join(plots_should_have_3x3_trees, tree_data_3x3, by = c("Site_ID", "Plot_ID"))






Plots_10x10_tree <- main_data_unc %>% filter(ControlSize == "Yes")

tree_data_10x10 <- tree_data_unc %>% filter(origin_table %in% c("Control_10x10", "Control_10x10_2")) 


plots_with_missing_data_3x3 <- setdiff(plots_should_have_3x3_trees$Plot_ID, tree_data_3030_plots$Plot_ID)

plots_with_missing_data_1010 <- anti_join(Plots_10x10_tree, tree_data_10x10, by = c("Site_ID", "Plot_ID"))






Plots_3x3_small <- main_data_unc %>% filter(SiteSize == "No")

tree_data_3x3_small <- tree_data_unc %>% filter(origin_table %in% c("Small_3x3")) 

plots_with_missing_data_3x3_small <- anti_join(tree_data_3x3_small, tree_data_3x3_small, by = c("Site_ID", "Plot_ID"))









census_plots <- main_data_unc %>% filter(Resample_3x3_Subplot == 2 & LittleTreesPresent == "No") 


tree_data_census <- tree_data_unc %>% filter(origin_table %in% c("Census_30x30")) 


plots_with_missing_data_census <- anti_join(census_plots, tree_data_census, by = c("Site_ID", "Plot_ID"))





find_missing_data_plots <- function(tree_data, main_data){
  # Filter for 30x30 plots that should have tree data--i.e. those that were not
  # resampled at all, those that were resampled once, and those that were resampled
  # twice AND had trees present within the final resampled plot.
  Plots_30x30_tree <- main_data %>% 
    filter((Resample_Main_Plot %in% c(0, 1)) | (Resample_Main_Plot == 2 & TreesPresent == "Yes"))
  
  # Filter tree data for only 30x30, >10cm plots
  tree_data_3030 <- tree_data %>% filter(origin_table %in% c("Normal_30x30", "Normal_30x30_2")) 
  
  # Identify which Site-Plot combinations that should have trees do not have trees.
  # Returns all rows in the first dataframe that do not have Site-Plot matches in the 
  # second dataframe.
  plots_with_missing_data_30x30 <- anti_join(Plots_30x30_tree, tree_data_3030, by = c("Site_ID", "Plot_ID"))
  
  
  # Same process as above, applied to 3x3 tables
  Plots_3x3_tree <- main_data %>% 
    filter((Resample_3x3_Subplot %in% c(0, 1)) | (Resample_3x3_Subplot == 2 & LittleTreesPresent == "Yes"))
  
  tree_data_3x3 <- tree_data %>% filter(origin_table %in% c("Nested_3x3_within_30x30", "Nested_3x3_within_10x10_Control",
                                                                "Nested_3x3_within_30x30_2", "Nested_3x3_within_10x10_Control_2")) 
  
  plots_with_missing_data_3x3 <- anti_join(Plots_3x3_tree, tree_data_3x3, by = c("Site_ID", "Plot_ID"))
  
  
  # Same process as above, applied to the 10x0 control table
  Plots_10x10_tree <- main_data %>% filter(ControlSize == "Yes")
  tree_data_10x10 <- tree_data %>% filter(origin_table %in% c("Control_10x10", "Control_10x10_2")) 
  plots_with_missing_data_10x10 <- anti_join(Plots_10x10_tree, tree_data_10x10, by = c("Site_ID", "Plot_ID"))
  
  # Same process as above, applied to plots too small for a 30x30 plot
  Plots_3x3_small <- main_data %>% filter(SiteSize == "No")
  tree_data_3x3_small <- tree_data %>% filter(origin_table %in% c("Small_3x3")) 
  plots_with_missing_data_3x3_small <- anti_join(tree_data_3x3_small, tree_data_3x3_small, by = c("Site_ID", "Plot_ID"))
  
  
  # Same process as above, but for census tables (Twice resampled 3x3 and still no trees)
  census_plots <- main_data %>% filter(Resample_3x3_Subplot == 2 & LittleTreesPresent == "No") 
  tree_data_census <- tree_data %>% filter(origin_table %in% c("Census_30x30")) 
  plots_with_missing_data_census <- anti_join(census_plots, tree_data_census, by = c("Site_ID", "Plot_ID"))
  
  return(list(Missing_Data_30x30 = plots_with_missing_data_30x30,
              Missing_Data_3x3 = plots_with_missing_data_3x3,
              Missing_Data_10x10 = plots_with_missing_data_10x10,
              Missing_Data_3x3_small = plots_with_missing_data_3x3_small,
              Missing_Data_census = plots_with_missing_data_census))
  
}



find_misplaced_tree_data <- function(tree_data) {
  misplaced_tree_data <- tree_data %>% 
    filter(Timeframe == 'Y0'& (Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2")))
  
  return(misplaced_tree_data)
}

