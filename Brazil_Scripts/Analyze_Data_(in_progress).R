

tree_data <- read.csv("Brazil_Raw_Data\\Corrected_Tree_Data_2023-10-16.csv")
main_data <- read.csv("Brazil_Raw_Data\\Main_Data_2023-10-16.csv")



tree_data_with_sizeclass <- tree_data %>%
  mutate(size_class = ifelse(Plot_Type %in% c("30x30", "30x15"), ">10cm",
                             ifelse(Plot_Type == "3x3", "1 - 9.9cm", "<1cm")))


tree_data_with_scaled_count <- tree_data_with_sizeclass %>% 
  filter(Plot_Type == "30x30") %>% 
  mutate(scaled_count = case_when(
    Resampling == 0 ~ Tree_Count, 
    Resampling == 1 ~ Tree_Count/2, 
    Resampling == 2 ~ Tree_Count/3
  ))



user_submitted_dims <- unique(tree_data$Plot_Info_Restoration_Technique)






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



