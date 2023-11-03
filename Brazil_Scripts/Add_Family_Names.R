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
  
  tree_data <- tree_data[1:15, ]
  
  # Find most recent Taxonomic_Ranks
  corrections_path <- "Taxonomic_Corrections"
  tax_rank_files <- list.files(path = corrections_path, pattern = "Taxonomic_Ranks", full.names = TRUE)
  
  # Conditional that handles the unlikely case that you are starting from scratch
  if (length(tax_rank_files) > 0) {
    latest_tax_rank_file <- tax_rank_files[order(file.info(tax_rank_files)$mtime, decreasing = TRUE)[1]]
    tax_rank_data <- read.csv(latest_tax_rank_file)
    print(paste0("Latest taxonomic rank file: ", latest_tax_rank_file))
    
  } else {
    tax_rank_data <- NULL
    print(paste0("No taxonomic rank file found. Starting from scratch."))
    
  }
  print(paste0("Latest tree data file: ", latest_tree_file))
  
  return(list(tree_data = tree_data, tax_rank_data = tax_rank_data, raw_data_path = raw_data_path))
}





add_family_names <- function(tree_data){
  species_names <- unique(tree_data$Species)
  family_names_results <- tax_name(species_names, get = 'family', db = "ncbi")
  
  tree_data_with_family <- tree_data %>% 
    left_join(select(family_names_results, query, family), by = c("Species" = "query"))
  return(list(tree_data_with_family = tree_data_with_family, family_results = family_names_results))
}


add_genus_species_cols <- function(updated_tree_data) {
  tree_data_with_family <- updated_tree_data %>%
    separate(Species, into = c("genus", "specific_epithet"), sep = " ", remove = FALSE) %>% 
    select(Site_ID, Plot_ID, Species, family, genus, specific_epithet, everything())
  
}



all_data <- load_data()
updated_tree_data <- add_family_names(all_data$tree_data)
tree_data_with_ranks <- add_genus_species_cols(updated_tree_data)
