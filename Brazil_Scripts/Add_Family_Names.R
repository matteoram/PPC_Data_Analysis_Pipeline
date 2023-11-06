# Add family name information 
library(dplyr)
library(taxize)
library(taxizedb)
library(tidyr)

tree_data <- read.csv("Main_Raw_Data\\Corrected_Tree_Data_2023-11-03_1527.csv")



# Example species names
species_names <- unique(tree_data$Species)

# Get taxonomic information from TPL
example_df <- tax_name(species_names[1:2], get = 'family', db = "itis")



tree_data_with_family <- tree_data %>% left_join(select(example_df, query, family), by = c("Species" = "query"))


tree_data_with_family <- tree_data_with_family %>%
  separate(Species, into = c("genus", "specific_epithet"), sep = " ", remove = FALSE)



##########


load_data <- function() {
  # Determine which dataset is being corrected and set path to desired folder
  answer <- readline(prompt = cat("Which Dataset are you correcting for? \n Enter '1' for the Primary Dataset or '2' for the Brazil Dataset:"))
  if (!answer %in% c("1", "2")) {
    print("Invalid response, please rerun script and be sure to enter either '1' or '2'")
  } else if (answer == "1") {
    raw_data_path <- "Main_Raw_Data"
  } else if (answer == "2") {
    raw_data_path <- "Brazil_Raw_Data"
  }
  
  # Get all files that match the pattern "Tree_Data_Uncorrected" in the "Brazil_Raw_Data" folder
  tree_files <- list.files(path = raw_data_path, pattern = "Corrected_Tree_Data", full.names = TRUE)
  
  # Sort files by modification date to get the most recent and read this into session
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]
  tree_data <- read.csv(latest_tree_file, check.names = FALSE)
  
  tree_data <- tree_data[1:30, ]
  
  # Find most recent Taxonomic_Ranks
  corrections_path <- "Taxonomic_Corrections"
  family_names_files <- list.files(path = corrections_path, pattern = "Family_Names", full.names = TRUE)
  
  # Conditional that handles the unlikely case that you are starting from scratch
  if (length(family_names_files) > 0) {
    latest_fam_name_file <- family_names_files[order(file.info(family_names_files)$mtime, decreasing = TRUE)[1]]
    existing_family_names <- read.csv(latest_fam_name_file)
    print(paste0("Latest taxonomic rank file: ", latest_fam_name_file))
    
  } else {
    existing_family_names <- NULL
    print(paste0("No taxonomic rank file found. Starting from scratch."))
    
  }
  print(paste0("Latest tree data file: ", latest_tree_file))
  
  return(list(tree_data = tree_data, existing_family_names = existing_family_names, raw_data_path = raw_data_path))
}


add_existing_family_names <- function(tree_data, existing_family_names){
  if(!is.null(existing_family_names)) {
    updated_tree_data <- left_join(tree_data, 
                                   existing_family_names, 
                                   by = c("Species" = "query"))
  } else{
    updated_tree_data <- tree_data
    
  }
  return(updated_tree_data)
}


# add_family_names <- function(tree_data){
#   species_names <- unique(tree_data$Species)
#   family_names_results <- tax_name(species_names, get = 'family', db = "ncbi")
#   
#   tree_data_with_family <- tree_data %>% 
#     left_join(select(family_names_results, query, family), by = c("Species" = "query"))
#   return(list(tree_data_with_family = tree_data_with_family, family_results = family_names_results))
# }


get_family_names <- function(updated_tree_data, existing_family_names) {
  
  # Check if 'family' column exists
  if("family" %in% names(updated_tree_data)) {
    species_to_lookup <- updated_tree_data %>%
      filter(is.na(family)) %>%
      distinct(Species) %>%
      pull(Species)
    
  } else {
    # If the 'family' column does not exist, use all Species
    species_to_lookup <- updated_tree_data %>%
      distinct(Species) %>%
      pull(Species)
  }
  
  if(!is.null(existing_family_names)){
    species_to_skip <- existing_family_names %>% 
      filter(is.na(family)) %>% 
      distinct(query) %>% 
      pull(query)
    
    species_to_lookup <- setdiff(species_to_lookup, species_to_skip)
  }
  
  # Retrieve family names from taxonomic database
  new_family_names <- tax_name(species_to_lookup, get = 'family', db = "ncbi")
  new_family_names <- new_family_names %>%
    select(-db)
  
  return(new_family_names)
}

add_new_family_names <- function(tree_data, new_family_names, existing_family_names) {
  if(is.null(existing_family_names)){
    tree_data_with_family <- left_join(tree_data, 
                                   select(new_family_names, query, family), 
                                   by = c("Species" = "query"))
  } else {
    tree_data_with_family <- left_join(tree_data, 
                                   select(new_family_names, query, family), 
                                   by = c("Species" = "query"),
                                   suffix = c(".old", ".new")) %>% 
      mutate(family = coalesce(family.new, family.old)) %>% 
      select(-family.old, -family.new)
  }

  return(tree_data_with_family)
}





add_genus_species_cols <- function(updated_tree_data) {
  tree_data_with_family <- updated_tree_data %>%
    separate(Species, into = c("genus", "specific_epithet"), sep = " ", remove = FALSE) %>% 
    select(Site_ID, Plot_ID, Species, family, genus, specific_epithet, everything())
  
}



save_family_names <- function(existing_family_names, new_family_names) {
  # new_resolved_names <- new_family_names %>% filter(!is.na(family))
  all_family_names <- rbind(new_family_names, existing_family_names) %>% 
    distinct(query)
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  corrections_path <- "Taxonomic_Corrections"
  
  if (!dir.exists(corrections_path)) {
    dir.create(corrections_path, recursive = TRUE)
  }
  
  
  file_name <- paste0(corrections_path, "/Family_Names_", date_info, ".csv")
  write.csv(all_family_names, file_name, row.names = FALSE)
  print(paste0("Updated family corrections saved to: ", file_name))
  return(all_family_names)

}


save_updated_tree_data <- function(tree_data_with_ranks, raw_data_path){
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  write.csv(tree_data_with_ranks, paste0(raw_data_path, "/Final_Tree_Data_", date_info, ".csv"), row.names = FALSE)
  print(paste0("Tree data with family names saved to: ", raw_data_path, "/Final_Tree_Data_", date_info, ".csv"))
  
}




all_data <- load_data()
updated_tree_data <- add_existing_family_names(all_data$tree_data, all_data$existing_family_names)
new_family_names <- get_family_names(updated_tree_data, all_data$existing_family_names)
tree_data_with_family <- add_new_family_names(updated_tree_data, new_family_names, all_data$existing_family_names)
tree_data_with_ranks <- add_genus_species_cols(tree_data_with_family)
all_family_names <- save_family_names(all_data$existing_family_names, new_family_names)
save_updated_tree_data(tree_data_with_ranks, all_data$raw_data_path)
