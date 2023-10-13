library(taxize)
library(dplyr)

tree_data <- read.csv("Tree_Data_Full_2023-10-12.csv", check.names = FALSE)
corrected_names <- read.csv("Taxonomic_Corrections_2023-10-13.csv")

unique_sp_names <- unique(tree_data$Species)

resolved_df <- gnr_resolve(sci = unique_sp_names, data_source_ids = c(165, 167), canonical = TRUE)

resolver_dataframe_short <- resolved_df %>%
  group_by(user_supplied_name) %>%
  arrange(desc(score)) %>%
  slice(1) %>%
  ungroup()
  

complete_df <- tree_data %>%
  left_join(resolver_dataframe_short, 
            by = c("Species" = "user_supplied_name")) %>% 
  select(Species, matched_name2, score, data_source_title)
  


manual_validation <- function(df) {
  for (i in 1:nrow(df)) {
    if (is.na(df$matched_name2[i])) {
      cat(paste("Unable to resolve:", df$Species[i], "\n"))
      new_name <- readline(prompt = "Please provide the correct name (or press Enter to skip): ")
      if (new_name != "") {
        df$matched_name2[i] <- new_name
        df$data_source_title[i] <- "Manual validation"
      }
    }
  }
  return(df)
}


final_df <- manual_validation(complete_df)

# Extract distinct corrections
distinct_corrections <- final_df %>%
  filter(!is.na(matched_name2)) %>%
  distinct()

# Save to CSV

date_info <- Sys.Date()
write.csv(distinct_corrections, paste0("Taxonomic_Corrections_", date_info, ".csv"), row.names = FALSE)


updated_data <- tree_data %>%
  mutate(submitted_species_name = Species) %>% 
  left_join(corrected_names, by = "Species") %>%
  mutate(Species = ifelse(is.na(matched_name2), Species, matched_name2)) %>%
  mutate(name_validation = ifelse(is.na(matched_name2), "Needs review", "Resolved")) %>% 
  select(-matched_name2)





