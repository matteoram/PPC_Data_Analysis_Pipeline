
library(jsonlite)
library(rvest)
library(dplyr)
library(tidyr)
library(janitor)
library(taxize)
library(stringr)



load_data <- function() {

  # Get all files that match the pattern "Tree_Data_Uncorrected" in the "Brazil_Raw_Data" folder
  IMP_files <- list.files(pattern = "ppc_export", full.names = TRUE)
  
  # Sort files by modification date to get the most recent and read this into session
  latest_IMP_file <- IMP_files[order(file.info(IMP_files)$mtime, decreasing = TRUE)[1]]
  IMP_data <- read.csv(latest_IMP_file, check.names = FALSE)
  
  species_data_path <- "Species_Data"
  invasives_files <- list.files(path = species_data_path, pattern = "Invasive_Species_Data", full.names = TRUE)
  invasives_report_files <- list.files(path = species_data_path, pattern = "Invasive_Species_Report", full.names = TRUE)
  
  
  
  corrections_files <- list.files(path = species_data_path, pattern = "Taxonomic_Corrections", full.names = TRUE)
  
  # Conditional that handles the unlikely case that you are starting from scratch
  if (length(corrections_files) > 0) {
    latest_corrections_file <- corrections_files[order(file.info(corrections_files)$mtime, decreasing = TRUE)[1]]
    corrected_names <- read.csv(latest_corrections_file)
    print(paste0("Latest correction file: ", latest_corrections_file))
  } else {
    corrected_names <- NULL
    print("No corrections found. Starting from scratch.")
  }
  
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
  
  return(list(IMP_data = IMP_data, Invasive_Species_Data = invasive_species_data, Invasives_Report = latest_invasives_report, Species_Corrections = corrected_names))
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

preprocess_IMP_data_v2 <- function(IMP_data){
  
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
  
  IMP_simple <- IMP_data %>% select(project_name, project_country, organization_name, site_id, site_name, tree_species, seed_species)
  IMP_modified <- IMP_simple %>% mutate(tree_species_names = extract_tree_species_names(IMP_simple$tree_species),
                                        seed_species_names = extract_seed_species_names(IMP_simple$seed_species))
  
  IMP_data_long <- IMP_modified %>%
    mutate(row_id = row_number()) %>%
    separate_rows(tree_species_names, sep = ", ") %>%
    separate_rows(seed_species_names, sep = ", ") %>% 
    mutate(original_tree_names = tree_species_names) %>% 
    mutate(original_seed_names = seed_species_names) %>% 
    mutate(tree_species_names = trimws(gsub("[\"']", "", tree_species_names))) %>%
    mutate(seed_species_names = trimws(gsub("[\"']", "", seed_species_names))) %>%
    mutate(tree_species_names = gsub("\n", "", tree_species_names)) %>% 
    mutate(seed_species_names = gsub("\n", "", seed_species_names))
  
  
  
  return(IMP_data_long)
  
}






update_IMP_data_existing_corrections_v2 <- function(species_df, species_corrections){
  
  updated_species_list <- species_df %>% 
    left_join(select(species_corrections, Species, matched_name2), by =c("tree_species_names" = "Species")) %>% 
    mutate(tree_species_names = ifelse(!is.na(matched_name2), matched_name2, tree_species_names),
           new_tree_name = matched_name2) %>% 
    select(-matched_name2) %>% 
    
    
    left_join(select(species_corrections, Species, matched_name2), by =c("seed_species_names" = "Species")) %>% 
    mutate(seed_species_names = ifelse(!is.na(matched_name2), matched_name2, seed_species_names),
           new_seed_name = matched_name2) %>% 
      select(-matched_name2)
  
  
  
  return(updated_species_list)
}



get_unresolved_names <- function(updated_IMP_data){
 unresolved_tree_names <-  updated_IMP_data %>% 
   select(original_tree_names, new_tree_name) %>% 
   filter(is.na(new_tree_name)) %>% 
   pull(original_tree_names)
 
 unresolved_tree_names <- unique(unresolved_tree_names)
 
 unresolved_seed_names <-  updated_IMP_data %>% 
   select(original_seed_names, new_seed_name) %>% 
   filter(is.na(new_seed_name)) %>% 
   pull(original_seed_names)
 
 unresolved_seed_names <- unique(unresolved_seed_names)
 
 all_unresolved_species <- unique(c(unresolved_tree_names, unresolved_seed_names))
 
 return(all_unresolved_species)
 
}


create_species_list <- function(updated_IMP_data, invasive_species_data){
  all_species <- processed_IMP_data %>% 
    select(tree_species_names, seed_species_names) %>% 
    pivot_longer(cols = everything(), values_to = "Species_Names") %>%
    pull(Species_Names) %>% 
    unlist() %>% 
    strsplit(", ") %>% 
    unlist() 
  unique_species_names <- unique(all_species)
  
  if(!is.null(invasive_species_data)){
    species_to_check <- data.frame(Species = unique_species_names[!unique_species_names %in% invasive_species_data$species])
    
  }else {
    species_to_check <- data.frame(Species = unique_species_names)
  }
  return(species_to_check)
}





create_species_list_v2 <- function(updated_IMP_data, invasive_species_data){
  unique_species_names <- unique(c(updated_IMP_data$tree_species_names, updated_IMP_data$seed_species_names))
  
  
  if(!is.null(invasive_species_data)){
    species_to_check <- data.frame(Species = unique_species_names[!unique_species_names %in% invasive_species_data$species])
    
  }else {
    species_to_check <- data.frame(Species = unique_species_names)
  }
  
  species_to_check <- species_to_check %>% filter(!Species == "")
  return(species_to_check)
}



update_species_list_with_existing_corrections <- function(species_df, species_corrections){
  cleaned_species_df <- species_df %>%
    mutate(submitted_species_name = Species) %>%
    mutate(Species = trimws(gsub("[\"']", "", Species))) %>%
    mutate(Species = gsub("\n", "", Species))
  
  
  updated_species_list <- species_df %>% 
    left_join(species_corrections, by = "Species") %>% 
    filter(is.na(matched_name2)) %>% 
    pull(Species)
  
  return(updated_species_list)
}






resolve_species_names <- function(unresolved_names){
  
  # Pass names needing review to resolver
  resolved_df <- gnr_resolve(sci = unresolved_names, data_source_ids = c(165, 167), canonical = TRUE, best_match_only = TRUE)
  
  # If no names can be resolved (which will happen if you've resolved everything),
  # this returns an empty dataframe with the correct structure for next steps
  if (nrow(resolved_df) == 0) {
    return(data.frame(
      user_supplied_name = character(0),
      matched_name2 = character(0),
      score = numeric(0),
      data_source_title = character(0)
    ))
  }
  
  unresolved_df <- data.frame(user_supplied_name = unresolved_names[!unresolved_names %in% resolved_df$user_supplied_name])
  full_df <- bind_rows(unresolved_df, resolved_df)
  
  full_df <- full_df %>% 
    rename(Species = user_supplied_name) %>% 
    select(-submitted_name)
    
  
  return(full_df)
}



manual_validation <- function(full_df) {
  # Extract unique unresolved species names
  unresolved_unique <- full_df %>%
    filter(is.na(matched_name2)) %>%
    distinct(Species) %>%
    pull(Species)
  
  # Show the user the total number of names that need correction
  total_to_correct <- length(unresolved_unique)
  cat(paste("You have", total_to_correct, "unique unresolved species names to correct...\n"))
  
  # Initialize a count to update user progress
  count_processed <- 0
  
  for (species in unresolved_unique) {
    # Increment count
    count_processed <- count_processed + 1
    # Show user unresolved name, prompt for correction
    cat(paste("Unable to resolve:", species, "\n"))
    new_name <- readline(prompt = "Please provide the correct name(or press Enter to skip): ")
    
    # If the user types "save", save the current state of df and continue
    if (new_name == "save") {
      cat("Progress saved. You can resume from where you left off.\n")
      return(full_df)
      
      # If user does not leave input blank, update all entries with matching name
      # with user correction, and record data_source_title as 'Manual validation'
    } else if (new_name != "") {
      full_df$matched_name2[full_df$Species == species] <- new_name
      full_df$data_source_title[full_df$Species == species] <- "Manual validation"
    }
    # Every 25 species, report on how many are finished and remaining
    if (count_processed %% 25 == 0) {
      cat(paste(count_processed, "species processed. You have", total_to_correct - count_processed, "remaining...\n"))
    }
  }
  
  return(full_df)
}



update_corrections_file <- function(old_corrections, new_corrections){
  new_corrections_filtered <- new_corrections %>% 
    filter(!is.na(matched_name2))
  
  all_corrections <- rbind(new_corrections_filtered, old_corrections) %>% 
    distinct()

  species_data_path <- "Species_Data"
  
  # Check if the "Species_Data" directory exists, if not, create it
  if (!dir.exists(species_data_path)) {
    dir.create(species_data_path, recursive = TRUE)
  }
  
  # Define the filename with the current date and time
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  file_name <- paste0(species_data_path, "/Taxonomic_Corrections_", date_info, ".csv")
  
  # Write the corrections to the file
  write.csv(all_corrections, file_name, row.names = FALSE)
  print(paste0("Updated species corrections saved to: ", file_name))
  return(all_corrections)
}




gbif_GISD_find <- function (x, ...) 
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
      out <- gbif_GISD_find(species_to_check[i], ...)
      if (length(out) == 0) {
        outlist[[i]] <- list(species = species_to_check[i], status = "Not in GISD")
      }
      else {
        doc <- rvest::read_html(paste0("http://www.iucngisd.org/gisd/species.php?sc=", out$taxonID))
        if (!simplify) {
          alien <- doc %>%
            html_elements("#ar-col li") %>%
            html_text() %>%
            str_replace_all("\\[\\d+\\]\\s*", "") 
          
          native <- doc %>%
            html_elements("#nr-col li") %>%
            html_text()
          
          summary <- doc %>% 
            html_elements("#summary") %>% 
            html_text() %>% 
            str_replace_all("[\r\n\t]|Summary", " ") %>%
            str_trim()
          
          outlist[[i]] <- list(species = species_to_check[i], alien_range = alien, 
                               native_range = native, summary = summary)
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
      } else if (length(species) >2){
        invasive <- c(invasive, list(species))
      }
    }
    
    not_invasive_df <- bind_rows(not_invasive)
    invasive_dfs <- lapply(invasive, function(x) {
      data.frame(
        species = x$species,
        alien_range = paste(x$alien_range, collapse = ", "),
        native_range = paste(x$native_range, collapse = ", "),
        summary = x$summary
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
processed_IMP_data <- preprocess_IMP_data_v2(all_data$IMP_data)
updated_IMP_data <- update_IMP_data_existing_corrections_v2(processed_IMP_data, all_data$Species_Corrections)
unresolved_names <- get_unresolved_names(updated_IMP_data)
resolved_names <- resolve_species_names(unresolved_names)
finished_names <- manual_validation(resolved_names)
updated_corrections <- update_corrections_file(all_data$Species_Corrections, finished_names)
updated_IMP_data_2 <- update_IMP_data_existing_corrections_v2(processed_IMP_data, updated_corrections)
# Script that will make the update
testresults <- updated_IMP_data_2 %>% filter(tree_species_names %in% invasives_only$species | seed_species_names%in% invasives_only$species)

species_list <- create_species_list_v2(updated_IMP_data = updated_IMP_data, all_data$Invasive_Species_Data)
invasives_results <- check_invasive_status(species_list$Species)


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



