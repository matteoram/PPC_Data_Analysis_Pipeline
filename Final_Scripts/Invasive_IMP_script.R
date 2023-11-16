
library(jsonlite)
library(xml2)
library(dplyr)


IMP_data <- read.csv("IMPDATA.csv")
names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][2] <- "Tree_Species"
names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][3] <- "Seed_Species"

spec_names <- c(IMP_data$Tree_Species, IMP_data$Seed_Species)

spec_count_pairs <- strsplit(spec_names, split = "\\|")


extract_species_names <- function(species_count_str) {
  # Replace the ":count" part with an empty string
  gsub(":\\d+", "", species_count_str)
}

# Apply the function to each element in the list and remove the names
species_names <- lapply(spec_count_pairs, function(pairs) {
  unname(sapply(pairs, extract_species_names))
})

all_species_names <- unlist(species_names)
unique_species <- unique(all_species_names)


invasive_results <- check_invasive_status(unique(all_species_names))

flagged_species <- invasive_results %>% filter(status == 'Invasive') %>% pull(species)



pattern <- paste(flagged_species, collapse="|")

# Function to return matched flagged species in a species string
find_matched_species <- function(species_string) {
  # Find all matches
  matches <- regmatches(species_string, gregexpr(pattern, species_string))[[1]]
  
  # Return the matches as a comma-separated string, or NA if no matches
  if (length(matches) == 0) {
    return(NA)
  } else {
    return(paste(matches, collapse=", "))
  }
}

# Apply the function to each row in the Species column
IMP_data$MatchedTreeSpecies <- sapply(IMP_data$Tree_Species, find_matched_species)

IMP_data$MatchedSeedSpecies <- sapply(IMP_data$Seed_Species, find_matched_species)


IMP_flagged_data <- IMP_data %>% filter(!is.na(MatchedTreeSpecies) | !is.na(MatchedSeedSpecies)) %>% 
  select(Project.Name, Project.Country, Organization.Name, MatchedTreeSpecies, MatchedSeedSpecies)

invasive_report <- invasive_results %>% filter(status == 'Invasive')








gbif_find <- function (x, ...) 
{
  args <- list(datasetKey = "b351a324-77c4-41c9-a909-f30f77268bc4", 
               name = x)
  cli <- crul::HttpClient$new(url = "https://api.gbif.org", 
                              opts = list(...))
  out <- cli$get("v1/species", query = args)
  out$raise_for_status()
  fromJSON(out$parse("UTF-8"))$results
}




check_invasive_status <- function (x, simplify = FALSE, ...) 
{
  outlist <- list()
  for (i in seq_along(x)) {
    message(paste("Checking", x[i]))
    out <- gbif_find(x[i], ...)
    if (length(out) == 0) {
      outlist[[i]] <- list(species = x[i], status = "Not in GISD")
    }
    else {
      doc <- xml2::read_html(paste0("http://www.iucngisd.org/gisd/species.php?sc=", out$taxonID))
      if (!simplify) {
        alien <- gsub("^\\s+|\\s+$", "", gsub("\\[|\\]|[[:digit:]]", 
                                              "", xml_text(xml_find_all(doc, "//div[@id=\"ar-col\"]//ul/li"))))
        native <- gsub("^\\s+|\\s+$", "", xml_text(xml_find_all(doc, 
                                                                "//div[@id=\"nr-col\"]//ul/li")))
        outlist[[i]] <- list(species = x[i], alien_range = alien, 
                             native_range = native)
      }
      else {
        outlist[[i]] <- list(species = x[i], status = "Invasive")
      }
    }
  }
  names(outlist) <- x
  
  
  not_invasive <- list()
  invasive <- list()
  
  for (species in outlist) {
    if (length(species) == 2){
      not_invasive <- c(not_invasive, list(species))
    } else if (length(species) == 3){
      invasive <- c(invasive, list(species))
    }
  }
  
  not_invasive_df <- bind_rows(not_invasive)
  invasive_dfs <- lapply(invasive, function(x) {
    data.frame(
      species = x$species,
      alien_range = paste(x$alien_range, collapse = ", "),
      native_range = paste(x$native_range, collapse = ", ")
    )
  })
  all_invasives_df <- do.call(rbind, invasive_dfs)
  
  final_df <- bind_rows(all_invasives_df, not_invasive_df)
  final_df <- final_df %>% mutate(status = ifelse(is.na(status), "Invasive", status))
}



spec_names <- IMP_data$X..of.Trees.Planted.by.Species...Count

spec_sample <- spec_names[1:15]

spec_count_pairs <- strsplit(spec_names, split = "\\|")


extract_species_names <- function(species_count_str) {
  # Replace the ":count" part with an empty string
  gsub(":\\d+", "", species_count_str)
}

# Apply the function to each element in the list and remove the names
species_names <- lapply(spec_count_pairs, function(pairs) {
  unname(sapply(pairs, extract_species_names))
})

all_species_names <- unlist(species_names)


invasive_results <- check_invasive_status(unique(all_species_names))

flagged_species <- invasive_results %>% filter(status == 'Invasive') %>% pull(species)

pattern <- paste(flagged_species, collapse="|")
pattern <- gsub(" ", "\\s", pattern) # Replace spaces with regex space matcher



contains_flagged_species <- function(species_string) {
  # Use grepl to check if the string contains any of the flagged species
  grepl(pattern, species_string)
}


IMP_data$Flagged <- sapply(IMP_data$Species, contains_flagged_species)









# Assuming 'flagged_species' is your vector of flagged species names
# and 'df' is your dataframe with the 'Species' column

# Create a regular expression pattern from the flagged species
pattern <- paste(flagged_species, collapse="|")

# Function to return matched flagged species in a species string
find_matched_species <- function(species_string) {
  # Find all matches
  matches <- regmatches(species_string, gregexpr(pattern, species_string))[[1]]
  
  # Return the matches as a comma-separated string, or NA if no matches
  if (length(matches) == 0) {
    return(NA)
  } else {
    return(paste(matches, collapse=", "))
  }
}

# Apply the function to each row in the Species column
IMP_data$MatchedTreeSpecies <- sapply(IMP_data$Tree_Species, find_matched_species)

IMP_data$MatchedSeedSpecies <- sapply(IMP_data$Seed_Species, find_matched_species)










# Function to return matched flagged species in two species strings
find_matched_species <- function(tree_species, seed_species) {
  # Find matches in both columns
  matches_tree <- regmatches(tree_species, gregexpr(pattern, tree_species))[[1]]
  matches_seed <- regmatches(seed_species, gregexpr(pattern, seed_species))[[1]]
  matches <- unique(c(matches_tree, matches_seed))
  
  # Return the matches as a comma-separated string, or NA if no matches
  if (length(matches) == 0) {
    return(NA)
  } else {
    return(paste(matches, collapse=", "))
  }
}

# Apply the function to each row in the dataframe
IMP_data$MatchedSpecies <- mapply(find_matched_species, IMP_data$Tree_Species, IMP_data$Seed_Species)



