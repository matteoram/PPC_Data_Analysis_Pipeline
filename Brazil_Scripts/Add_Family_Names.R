# Add family name information 

library(taxize)
library(taxizedb)

tree_data <- read.csv("test_corrections_sample.csv")



# Example species names
species_names <- unique(tree_data$Species)

# Get taxonomic information from TPL
example_df <- tax_name(species_names, get = 'family', db = "itis")



tree_data_with_family <- tree_data %>% left_join(select(example_df, query, family), by = c("Species" = "query"))


tree_data_with_family <- tree_data_with_family %>%
  separate(Species, into = c("genus", "specific_epithet"), sep = " ", remove = FALSE)
