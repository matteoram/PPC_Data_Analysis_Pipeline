library(dplyr)
library(stringr)
library(tidyverse)

tree_data <- read.csv("Main_Raw_Data\\Tree_Data_Uncorrected_2023-10-24.csv", check.names = FALSE)
main_data <- read.csv("Main_Raw_Data\\Main_Data_2023-10-24.csv", check.names = FALSE)



tree_data_with_sizeclass <- tree_data %>%
  mutate(size_class = ifelse(Plot_Type %in% c("30x30", "30x15") & !grepl("census", origin_table, ignore.case = TRUE), ">10cm",
                             ifelse(Plot_Type == "3x3", "1 - 9.9cm", 
                                    ifelse(grepl("census", origin_table, ignore.case= TRUE),"1 - 9.9cm", "<1cm"))))





scale_tree_count <- function(data) {
  data %>%
    mutate(scaled_count = case_when(
      Plot_Type == "30x30" ~ Tree_Count / (Resample_Main_Plot + 1),
      Plot_Type == "10x10" ~ Tree_Count / (Resample_Main_Plot + 1),
      Plot_Type == "3x3" ~ Tree_Count * 100 / (Resample_3x3_Subplot + 1),
      Plot_Type == "1x1" ~ Tree_Count * 900,
      TRUE ~ Tree_Count
    ))
}



prepped_data <- scale_tree_count(tree_data_with_sizeclass)

prepped_data_with_ID <- prepped_data %>%  left_join(select(main_data, `_id`, main_index), by = "main_index")



check <- prepped_data_with_ID %>%
  filter(Tree_Type == "planted", Timeframe == "Y0") %>% 
  group_by(Organization_Name, Site_ID, Plot_ID, Species) %>%
  summarise(total_count = sum(scaled_count, na.rm = TRUE))

















baseline_tree_count_plot <- prepped_data_with_ID %>%
  filter(Timeframe == "Y0") %>% 
  group_by(Organization_Name, Site_ID, Plot_ID, Species, Tree_Type) %>%
  summarise(total_count = sum(scaled_count), .groups = 'drop') %>%
  spread(Tree_Type, total_count, fill = 0) %>%
  mutate(existing_trees = naturally_regenerating + Present,
         planted_trees = planted) %>% 
  left_join(select(main_data, Organization_Name, Site_ID, Plot_ID, `_id`), by = "_id") %>% 
  mutate(unique_ID = paste(Organization_Name, Site_ID, Plot_ID, sep = "_"))




# prepped_data_with_planting_pattern <- prepped_data %>% 
#   left_join(select(main_data_with_pattern, 
#                    Simplified_Restoration_Technique,
#                    Planting_Area, main_index), by = "main_index") %>% 
#   mutate(unique_ID = paste(Organization_Name, Site_ID, Plot_ID, sep = "_"))
# 


baseline_tree_count_plot <- prepped_data %>%
  filter(Timeframe == "Y0") %>% 
  group_by(Organization_Name, Site_ID, Plot_ID, SiteType, Tree_Type) %>%
  summarise(total_count = sum(scaled_count), .groups = 'drop') %>%
  spread(Tree_Type, total_count, fill = 0) %>%
  mutate(existing_trees = naturally_regenerating + Present,
         planted_trees = planted,
         unique_ID = paste(Organization_Name, Site_ID, Plot_ID, sep = "_")) %>% 
  distinct(unique_ID, .keep_all = TRUE) #should not have to exist



main_data_with_pattern <- main_data_with_pattern %>% 
  mutate(unique_ID = paste(Organization_Name, Site_ID, Plot_ID, sep = "_")) %>% 
  distinct(unique_ID, .keep_all = TRUE) #should not have to exist


# Because of non-unique IDs

baseline_with_pattern_plot <- baseline_tree_count_plot %>% 
  left_join(select(main_data_with_pattern, 
                   Planting_Area, 
                   Simplified_Restoration_Technique,
                   unique_ID), by = "unique_ID") %>% 
  mutate(expected_planting_count = 900/Planting_Area)
            








baseline_tree_count_site <- prepped_data %>%
  filter(Timeframe == "Y0") %>% 
  group_by(Organization_Name, Site_ID, SiteType, Tree_Type) %>%
  summarise(total_count = sum(scaled_count), .groups = 'drop') %>%
  spread(Tree_Type, total_count, fill = 0) %>%
  mutate(existing_trees = naturally_regenerating + Present,
         planted_trees = planted,
         unique_ID = paste(Organization_Name, Site_ID, sep = "_")) 






###############
tree_count_by_site <- prepped_data %>%
  group_by(Site_ID, SiteType, Tree_Type, Timeframe) %>%
  summarise(total_count = sum(Tree_Count)) %>%
  pivot_wider(names_from = Tree_Type, values_from = total_count, values_fill = 0)



tree_count_by_site_andTF <- prepped_data %>%
  group_by(Site_ID, SiteType, Tree_Type, Timeframe) %>%
  summarise(total_count = sum(Tree_Count)) %>%
  mutate(combined_id = paste(Tree_Type, Timeframe, sep="_")) %>%
  pivot_wider(names_from = combined_id, values_from = total_count, values_fill = 0)







tree_count_by_plot <-  prepped_data %>%
  mutate(unique_plot_ID = paste(Site_ID, Plot_ID, sep = "_")) %>% 
  group_by(unique_plot_ID, SiteType, Tree_Type) %>%
  summarise(total_count = sum(Tree_Count)) %>%
  spread(Tree_Type, total_count, fill = 0) 


tree_count_by_site_over_time <- testscaled %>%
  group_by(Site_ID, SiteType, Tree_Type, Timeframe) %>%
  summarise(total_count = sum(Tree_Count)) %>%
  unite("Tree_Time", Tree_Type, Timeframe, sep = "_") %>%
  spread(Tree_Time, total_count, fill = 0)






user_submitted_dims <- unique(tree_data$Restoration_Technique)




manually_add_dimensions <- function(user_submitted_dims) {
  true_dimensions <- data.frame(user_submitted_dims = user_submitted_dims,
                                planting_dimensions = NA)
  
  for (i in 1:length(user_submitted_dims)) {
    print(user_submitted_dims[i])
    value <- readline(prompt = paste0("Enter two numbers separated by * that match ",
                                      "the described planting pattern above (or press Enter to skip): "))
    
    if (nchar(value) > 0) {  # Check if input is not empty
      value <- eval(parse(text = value))
      true_dimensions$planting_dimensions[i] <- value
    }
  }
  return(true_dimensions)
}


tree_data_with_planting_dims <- tree_data %>% 
  left_join(true_dimensions, join_by(Plot_Info_Restoration_Technique == user_submitted_dims)) %>% 
  mutate(estimated_planting_density = 900/planting_dimensions)



#####



# Sample data

# Process data
main_data_with_pattern <- main_data %>% 
  mutate(
    # Extract numbers using regular expression
    nums = str_extract_all(PlantingPattern, "\\d*\\.?\\d+"),
    
    # Check the length of the extracted numbers. If it's not 2 or Restoration_Technique is NA, 
    # set Planting_Area and Simplified_Restoration_Technique to NA
    Planting_Area = mapply(function(x, y) {
      if (is.na(y) || length(x) != 2) return(NA)
      as.numeric(x)[1] * as.numeric(x)[2]
    }, nums, PlantingPattern),
    
    Simplified_Restoration_Technique = mapply(function(x, y) {
      if (is.na(y) || length(x) != 2) return(NA)
      paste0(x, collapse = "x")
    }, nums, PlantingPattern)
  ) %>%
  select(-nums)


planting_pattern_corrections <- main_data_with_pattern %>% 
  select(PlantingPattern, Simplified_Restoration_Technique, Planting_Area) %>% 
  distinct()



for (i in 1:nrow(planting_pattern_corrections)) {
  if(!is.na(planting_pattern_corrections$Simplified_Restoration_Technique[i])){
    cat(paste0(planting_pattern_corrections$Restoration_Technique[i], "(original)", "-->", 
               planting_pattern_corrections$Simplified_Restoration_Technique[i],"(simplified)", "-->",
               planting_pattern_corrections$Planting_Area[i], "(calculated area)", "\n"))
    response <- readline(prompt = "Does the above conversion look correct? Hit enter if yes, otherwise, type in your correction.")
    if (!nchar(response) == 0) {
      planting_pattern_corrections$Simplified_Restoration_Technique[i] <- response
      nums = str_extract_all(response, "\\d+")
      area = as.numeric(nums[[1]][1]) * as.numeric(nums[[1]][2])
      planting_pattern_corrections$Planting_Area[i] <- area
      
    }
  }else if(is.na(planting_pattern_corrections$Simplified_Restoration_Technique[i])) {
    cat(planting_pattern_corrections$Restoration_Technique[i])
    response <- readline(prompt = paste0("Type in your interpretation of the, ", 
                                         "description (e.g. 3x2). Press 'Enter'",
                                         " if pattern is not discernible:"))
    if (!nchar(response) == 0) {
      planting_pattern_corrections$Simplified_Restoration_Technique[i] <- response
      nums = str_extract_all(response, "\\d+")
      area = as.numeric(nums[[1]][1]) * as.numeric(nums[[1]][2])
      planting_pattern_corrections$Planting_Area[i] <- area
    }
  }
}


corrections_with_estimated_density <- planting_pattern_corrections %>% 
  mutate(Est_Planting_Density = 900/Planting_Area)



















# Separate the data based on Plot_Type
data_3x3 <- prepped_data_with_ID %>% filter(Plot_Type == "3x3", Tree_Type == "planted")
data_30x30 <- prepped_data_with_ID %>% filter(Plot_Type == "30x30", Tree_Type == "planted")

# Aggregate data for each Plot_Type
agg_3x3 <- data_3x3 %>% 
  group_by(`_id`, Species) %>%
  summarise(total_planted_3x3 = sum(scaled_count, na.rm = TRUE), .groups = 'drop')

agg_30x30 <- data_30x30 %>% 
  group_by(`_id`, Species) %>%
  summarise(total_planted_30x30 = sum(scaled_count, na.rm = TRUE), .groups = 'drop')

# Combine the aggregated data
combined_data <- left_join(agg_30x30, agg_3x3, by = c("_id", "Species"))

# Compute the total planted for each _id and Species combination
combined_data <- combined_data %>% 
  mutate(total_planted = total_planted_3x3 + total_planted_30x30)
