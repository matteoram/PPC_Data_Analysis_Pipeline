library(dplyr)
library(stringr)
library(tidyverse)

tree_data <- read.csv("Brazil_Raw_Data\\Corrected_Tree_Data_2023-10-19.csv", check.names = FALSE)
main_data <- read.csv("Brazil_Raw_Data\\Main_Data_2023-10-19.csv", check.names = FALSE)



tree_data_with_sizeclass <- tree_data %>%
  mutate(size_class = ifelse(Plot_Type %in% c("30x30", "30x15") & !grepl("census", origin_table, ignore.case = TRUE), ">10cm",
                             ifelse(Plot_Type == "3x3", "1 - 9.9cm", 
                                    ifelse(grepl("census", origin_table, ignore.case= TRUE),"1 - 9.9cm", "<1cm"))))


big_tree_data_with_scaled_count <- tree_data_with_sizeclass %>% 
  filter(Plot_Type == "30x30") %>% 
  mutate(scaled_count = case_when(
    Resample_Main_Plot == 0 ~ Tree_Count, 
    Resample_Main_Plot == 1 ~ Tree_Count/2, 
    Resample_Main_Plot == 2 ~ Tree_Count/3
  ))

big_tree_data_with_scaled_count <- tree_data_with_sizeclass %>% 
  filter(Plot_Type == "30x30") %>% 
  mutate(scaled_count = case_when(
    Resample_Main_Plot == 0 ~ Tree_Count, 
    Resample_Main_Plot == 1 ~ Tree_Count/2, 
    Resample_Main_Plot == 2 ~ Tree_Count/3
  ))





small_tree_data_with_scaled_count <- tree_data_with_sizeclass %>% 
  filter(Plot_Type == "3x3") %>% 
  mutate(scaled_count = case_when(
    Resample_3x3_Subplot == 0 ~ Tree_Count*100, 
    Resample_3x3_Subplot == 1 ~ Tree_Count*100/2, 
    Resample_3x3_Subplot == 2 ~ Tree_Count*100/3
  ))



tiny_tree_data_with_scaled_count <- tree_data_with_sizeclass %>% 
  filter(Plot_Type == "1x1") %>% 
  mutate(scaled_count = case_when(
    Resample_Main_Plot == 0 ~ Tree_Count*900, 
    Resample_Main_Plot == 1 ~ Tree_Count*900/2, 
    Resample_Main_Plot == 2 ~ Tree_Count*900/3
  ))





tree_count_by_site <- tree_data_with_scaled_count %>%
  group_by(Site_ID, SiteType,Tree_Type) %>%
  summarise(total_count = sum(Tree_Count)) %>%
  spread(Tree_Type, total_count, fill = 0)


tree_count_by_plot <-  tree_data_with_scaled_count %>%
  group_by(Plot_ID, SiteType, Tree_Type) %>%
  summarise(total_count = sum(Tree_Count)) %>%
  spread(Tree_Type, total_count, fill = 0)








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
    nums = str_extract_all(Restoration_Technique, "\\d+"),
    
    # Check the length of the extracted numbers. If it's not 2 or Restoration_Technique is NA, 
    # set Planting_Area and Simplified_Restoration_Technique to NA
    Planting_Area = mapply(function(x, y) {
      if (is.na(y) || length(x) != 2) return(NA)
      as.numeric(x)[1] * as.numeric(x)[2]
    }, nums, Restoration_Technique),
    
    Simplified_Restoration_Technique = mapply(function(x, y) {
      if (is.na(y) || length(x) != 2) return(NA)
      paste0(x, collapse = "x")
    }, nums, Restoration_Technique)
  ) %>%
  select(-nums)



planting_pattern_corrections <- main_data_with_pattern %>% 
  select(Restoration_Technique, Simplified_Restoration_Technique, Planting_Area) %>% 
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
