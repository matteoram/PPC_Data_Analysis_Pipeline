
library(jsonlite)
library(xml2)
library(dplyr)
library(tidyr)



load_data <- function() {

  # Get all files that match the pattern "Tree_Data_Uncorrected" in the "Brazil_Raw_Data" folder
  IMP_files <- list.files(pattern = "ppc_export", full.names = TRUE)
  
  # Sort files by modification date to get the most recent and read this into session
  latest_IMP_file <- IMP_files[order(file.info(IMP_files)$mtime, decreasing = TRUE)[1]]
  IMP_data <- read.csv(latest_IMP_file, check.names = FALSE)
  
  species_data_path <- "Species_Data"
  invasives_files <- list.files(path = species_data_path, pattern = "Invasive_Species_Data", full.names = TRUE)
  invasives_report_files <- list.files(path = species_data_path, pattern = "Invasive_Species_Report", full.names = TRUE)
  
  # Conditional that handles the unlikely case that you are starting from scratch
  if (length(invasives_files) > 0) {
    latest_invasives_file <- invasives_files[order(file.info(invasives_files)$mtime, decreasing = TRUE)[1]]
    invasive_species_data <- read.csv(latest_invasives_file, check.names = FALSE)
    print(paste0("Latest Invasive Species file: ", latest_invasives_file))
  } else {
    invasive_species_data <- NULL
    print("No invasive species data found. Starting from scratch.")
  }
  if (length(invasives_report_files) > 0) {
    latest_report_file <- invasives_report_files[order(file.info(invasives_report_files)$mtime, decreasing = TRUE)[1]]
    latest_invasives_report <- read.csv(latest_report_file, check.names = FALSE)
    print(paste0("Latest Invasive Species file: ", latest_report_file))
  } else {
    latest_invasives_report <- NULL
    print("No prior invasive species report found. Starting from scratch.")
  }
  
  
  
  
  print(paste0("Latest IMP data file: ", latest_IMP_file))
  
  return(list(IMP_data = IMP_data, Invasive_Species_Data = invasive_species_data, Invasives_Report = latest_invasives_report))
}







preprocess_IMP_data <- function(IMP_data){
  
  IMP_data <- clean_names(IMP_data)
  names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][2] <- "tree_species"
  names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][3] <- "seed_species"
  
  extract_tree_species_names <- function(species_count_str) {
    # Replace the ":count" part with an empty string and "|" with ", "
    species_names = gsub(":\\d+", "", species_count_str)
    gsub("\\|", ", ", species_names)
  }
  
  extract_seed_species_names <- function(species_count_str) {
    # Replace the "\\d+:" part with an empty string and "|" with ", "
    species_names = gsub("\\d+:", "", species_count_str)
    gsub("\\|", ", ", species_names)
  }
  
  IMP_simple <- IMP_data %>% select(project_name, project_country, organization_name, tree_species, seed_species)
  IMP_modified <- IMP_simple %>% mutate(tree_species_names = extract_tree_species_names(IMP_simple$tree_species),
                                        seed_species_names = extract_seed_species_names(IMP_simple$seed_species))
  
  
  return(IMP_modified)
  
}





create_species_list <- function(processed_IMP_data, invasive_species_data){
  all_species <- processed_IMP_data %>% 
    select(tree_species_names, seed_species_names) %>% 
    pivot_longer(cols = everything(), values_to = "Species_Names") %>%
    pull(Species_Names) %>% 
    unlist() %>% 
    strsplit(", ") %>% 
    unlist() 
  unique_species_names <- unique(all_species)
  
  if(!is.null(invasive_species_data)){
    species_to_check <- unique_species_names[!unique_species_names %in% invasive_species_data$species]
    
  }else {
    species_to_check <- unique_species_names
  }
  return(species_to_check)
}



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





check_invasive_status <- function (species_to_check, simplify = FALSE, ...) 
{
  if (length(species_to_check) == 0) {
    print("No new species to check.")
    return(NULL)
  }else{
    outlist <- list()
    for (i in seq_along(species_to_check)) {
      message(paste("Checking", species_to_check[i]))
      out <- gbif_find(species_to_check[i], ...)
      if (length(out) == 0) {
        outlist[[i]] <- list(species = species_to_check[i], status = "Not in GISD")
      }
      else {
        doc <- xml2::read_html(paste0("http://www.iucngisd.org/gisd/species.php?sc=", out$taxonID))
        if (!simplify) {
          alien <- gsub("^\\s+|\\s+$", "", gsub("\\[|\\]|[[:digit:]]", 
                                                "", xml_text(xml_find_all(doc, "//div[@id=\"ar-col\"]//ul/li"))))
          native <- gsub("^\\s+|\\s+$", "", xml_text(xml_find_all(doc, 
                                                                  "//div[@id=\"nr-col\"]//ul/li")))
          outlist[[i]] <- list(species = species_to_check[i], alien_range = alien, 
                               native_range = native)
        }
        else {
          outlist[[i]] <- list(species = species_to_check[i], status = "Invasive")
        }
      }
    }
    names(outlist) <- species_to_check
    
    
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
}



create_invasives_report <- function(invasives_results, processed_IMP_data, old_invasives_report){
  
  find_organizations_for_species <- function(species, planting_dataframe) {
    orgs <- planting_dataframe %>% 
      filter(grepl(species, tree_species_names) | grepl(species, seed_species_names)) %>% 
      pull(organization_name) %>%
      unique()
    
    # Combine the organization names into a single character string
    orgs_list <- paste(orgs, collapse = ", ")
    return(orgs_list)
  }
  if(sum(invasives_results$status == "Invasive") >0){
    new_invasives_report <- invasives_results %>% 
      filter(status == 'Invasive') %>%
      rowwise() %>% 
      mutate(orgs_list = find_organizations_for_species(species, processed_IMP_data))
  }else {
    new_invasives_report <- NULL
    print("No new invasive species data found.")
  }
  
  full_invasives_report <- rbind(new_invasives_report, old_invasives_report)
  return(full_invasives_report)
}




save_invasives_data <- function(old_invasive_data, new_invasive_data){
  if(!is.null(old_invasive_data)){
    all_invasive_data <- rbind(new_invasive_data, old_invasive_data)
  }else {
    all_invasive_data <- new_invasive_data
  }
  
  species_data_path <- "Species_Data"
  
  # Check if the "Species_Data" directory exists, if not, create it
  if (!dir.exists(species_data_path)) {
    dir.create(species_data_path, recursive = TRUE)
  }
  
  # Define the filename with the current date and time
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  file_name <- paste0(species_data_path, "/Invasive_Species_Data_", date_info, ".csv")
  
  # Write the corrections to the file
  write.csv(all_invasive_data, file_name, row.names = FALSE)
  print(paste0("Updated invasive species data saved to: ", file_name))
  return(all_invasive_data)
  
}


save_invasives_report <- function(full_invasives_report){

  
  species_data_path <- "Species_Data"
  
  # Check if the "Species_Data" directory exists, if not, create it
  if (!dir.exists(species_data_path)) {
    dir.create(species_data_path, recursive = TRUE)
  }
  
  # Define the filename with the current date and time
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  file_name <- paste0(species_data_path, "/Invasive_Species_Report_", date_info, ".csv")
  
  # Write the corrections to the file
  write.csv(full_invasives_report, file_name, row.names = FALSE)
  print(paste0("Updated invasive species report saved to: ", file_name))

}






all_data <- load_data()
all_data$IMP_data <- all_data$IMP_data[1:300,]
processed_IMP_data <- preprocess_IMP_data(all_data$IMP_data)
species_list <- create_species_list(processed_IMP_data = processed_IMP_data, all_data$Invasive_Species_Data)
invasives_results <- check_invasive_status(species_list)
invasives_report <- create_invasives_report(invasives_results, processed_IMP_data, all_data$Invasives_Report)
all_invasives_data <- save_invasives_data(old_invasive_data = all_data$Invasive_Species_Data, new_invasive_data = invasives_results)
save_invasives_report(invasives_report)









find_organizations_for_species <- function(species, processed_IMP_data) {
  orgs <- processed_IMP_data %>% 
    filter(grepl(species, tree_species_names) | grepl(species, seed_species_names)) %>% 
    pull(organization_name) %>%
    unique()
  
  # Combine the organization names into a single character string
  orgs_list <- paste(orgs, collapse = ", ")
  return(orgs_list)
}




invasive_report <- invasives_results %>% 
  filter(status == 'Invasive') %>%
  rowwise() %>% 
  mutate(orgs_list = find_organizations_for_species(species, processed_IMP_data))









all_species <- IMP_modified %>% 
  select(Tree_Species_Names, Seed_Species_Names) %>% 
  pivot_longer(cols = everything(), values_to = "Species_Names") %>%
  pull(Species_Names) %>% 
  unlist() %>% 
  strsplit(", ") %>% 
  unlist() 

unique_species_names <- unique(all_species)

species_to_check <- unique_species_names[!unique_species_names %in% invasive_data$species]

invasive_results <- check_invasive_status(species_to_check)


flagged_species <- invasive_results %>% 
  filter(status == 'Invasive') %>% 
  pull(species)



find_organizations_for_species <- function(invasive_species_name, dataframe) {
  orgs <- dataframe %>% 
    filter(grepl(invasive_species_name, tree_species_names) | grepl(invasive_species_name, seed_species_names)) %>% 
    pull(organization_name) %>%
    unique()
  
  # Combine the organization names into a single character string
  orgs_list <- paste(orgs, collapse = ", ")
  return(orgs_list)
}


invasive_report <- invasive_report %>% 
  filter(status == 'Invasive') %>% 
  rowwise() %>% 
  mutate(orgs_list = find_organizations_for_species(species, processed_IMP_data))








check_invasive_status <- function (x, simplify = FALSE, ...) 
{
  if (length(x) == 0) {
    print("No new species to check.")
    return(NULL)
  }else{
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
}









spec_names <- c(IMP_data$Tree_Species, IMP_data$Seed_Species)

spec_count_pairs <- strsplit(spec_names, split = "\\|")



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
  if (length(x) == 0) {
    print("No new species to check.")
    return(NULL)
    }else{
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










names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][2] <- "Tree_Species"
names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][3] <- "Seed_Species"



extract_tree_species_names <- function(species_count_str) {
  # Replace the ":count" part with an empty string and "|" with ", "
  species_names = gsub(":\\d+", "", species_count_str)
  gsub("\\|", ", ", species_names)
}

extract_seed_species_names <- function(species_count_str) {
  # Replace the "\\d+:" part with an empty string and "|" with ", "
  species_names = gsub("\\d+:", "", species_count_str)
  gsub("\\|", ", ", species_names)
}



IMP_simple <- IMP_data %>% select(Project.Name, Project.Country, Organization.Name, Tree_Species, Seed_Species)
IMP_modified <- IMP_simple %>% mutate(Tree_Species_Names = extract_tree_species_names(IMP_simple$Tree_Species),
                                      Seed_Species_Names = extract_seed_species_names(IMP_simple$Seed_Species))



