# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Author: Johannes Nelson
# Input: The exported IMP data that has information about species of planted 
# seeds and trees. 
# Output: Invasives Report, Invasives Data

# This script relies on the Global Invasive Species Database (GISD) to flag 
# potential invasive species. To pull information about invasive species, the 
# script relies on the 'rvest' package to perform webscraping that targets 
# information about the native and alien ranges of the plants in question, as 
# well as a summary of its invasiveness, if one exists. Then, a report is 
# generated that identifies any organizations/countries planting these plants
# for further follow-up. In a majority of cases, the species flagged as invasive
# will be species that are being planted in their native ranges. This tool is 
# designed as a first step, but follow-up to analyze ranges and potential impact
# will be necessary.

# Since species names need to be well formed in order for this to work, this 
# script also runs through a slightly modified species corrections loop. All 
# corrections data from this script will be combined and saved with the corrections
# data from the primary species corrections script.

# The script outputs two files: an Invasives Report, which will contain the 
# species and organizations that have been flagged, as well as an Invasives Data
# file which serves as a record of results for all species that have been processed
# by this script so that subsequent passes do not need to reprocess names that
# have already been checked (since there are nearly 1,000 species, this would 
# take a while)
# -------------------------------------------------------------------------------

necessary_packages <- c("jsonlite", "rvest", "dplyr", "tidyr", 
                        "janitor", "taxize", "stringr")

for (pkg in necessary_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(pkg, "not found. Installing now...\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


#' 1. Load Data
#'
#' This function loads the IMP data, the prior corrections data, the prior 
#' invasives data, and the prior invasives report. It loads each of these using 
#' character patterns in the file names which were applied in previous scripts.
#' The only pattern not applied in script is the IMP data file. This function
#' looks for a file with the pattern 'ppc_export' within the filename. This file
#' will need to be exported from the IMP database and placed in the working 
#' directory. 
#'
#' @return List with two dataframes: tree data and corrections data--as well as 
#' a filepath for later reference.

load_data <- function() {

  # Retrieve files based on character patterns and select most recently modified
  # version to read into R session
  IMP_files <- list.files(pattern = "ppc_export", full.names = TRUE)
  latest_IMP_file <- IMP_files[order(file.info(IMP_files)$mtime, 
                                     decreasing = TRUE)[1]]
  IMP_data <- read.csv(latest_IMP_file, check.names = FALSE)
  
  species_data_path <- "Species_Data"
  invasives_files <- list.files(path = species_data_path, 
                                pattern = "Invasive_Species_Data", 
                                full.names = TRUE)
  invasives_report_files <- list.files(path = species_data_path, 
                                       pattern = "Invasive_Species_Report", 
                                       full.names = TRUE)
  
  
  
  corrections_files <- list.files(path = species_data_path, 
                                  pattern = "Taxonomic_Corrections", 
                                  full.names = TRUE)
  
  # Series of conditionals that handle the unlikely case that you are starting 
  # from scratch. 
  if (length(corrections_files) > 0) {
    latest_corrections_file <- corrections_files[order(file.info(corrections_files)$mtime, 
                                                       decreasing = TRUE)[1]]
    corrected_names <- read.csv(latest_corrections_file)
    print(paste0("Latest correction file: ", latest_corrections_file))
  } else {
    corrected_names <- NULL
    print("No corrections found. Starting from scratch.")
  }
  
  if (length(invasives_files) > 0) {
    latest_invasives_file <- invasives_files[order(file.info(invasives_files)$mtime, 
                                                   decreasing = TRUE)[1]]
    invasive_species_data <- read.csv(latest_invasives_file, check.names = FALSE)
    print(paste0("Latest Invasive Species file: ", latest_invasives_file))
  } else {
    invasive_species_data <- NULL
    print("No invasive species data found. Starting from scratch.")
  }
  if (length(invasives_report_files) > 0) {
    latest_report_file <- invasives_report_files[order(file.info(invasives_report_files)$mtime, 
                                                       decreasing = TRUE)[1]]
    latest_invasives_report <- read.csv(latest_report_file, check.names = FALSE)
    print(paste0("Latest Invasive Species file: ", latest_report_file))
  } else {
    latest_invasives_report <- NULL
    print("No prior invasive species report found. Starting from scratch.")
  }
  
  print(paste0("Latest IMP data file: ", latest_IMP_file))
  
  return(list(IMP_data = IMP_data, Invasive_Species_Data = invasive_species_data, Invasives_Report = latest_invasives_report, Species_Corrections = corrected_names))
}


#' 2. Preprocess IMP Data
#'
#' This function handles the way species data is stored in the IMP database and 
#' makes it workable. It first separates out the long strings of species data
#' @return List with two dataframes: tree data and corrections data--as well as 
#' a filepath for later reference.


# 
# preprocess_IMP_data <- function(IMP_data){
#   
#   IMP_data <- clean_names(IMP_data)
# 
#   names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][2] <- "tree_species"
#   names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][3] <- "seed_species"
#   
#   extract_tree_species_names <- function(species_count_str) {
#     # Replace the ":count" part with an empty string and "|" with ", "
#     species_names = gsub(":\\d+", "", species_count_str)
#     gsub("\\|", ", ", species_names)
#   }
#   
#   extract_seed_species_names <- function(species_count_str) {
#     # Replace the "\\d+:" part with an empty string and "|" with ", "
#     species_names = gsub("\\d+:", "", species_count_str)
#     gsub("\\|", ", ", species_names)
#   }
#   
#   IMP_simple <- IMP_data %>% select(project_name, project_country, organization_name, site_id, site_name, tree_species, seed_species)
#   IMP_modified <- IMP_simple %>% mutate(tree_species_names = extract_tree_species_names(IMP_simple$tree_species),
#                                         seed_species_names = extract_seed_species_names(IMP_simple$seed_species))
#   
#   IMP_data_long <- IMP_modified %>%
#     mutate(row_id = row_number()) %>%
#     separate_rows(tree_species_names, sep = ", ") %>%
#     separate_rows(seed_species_names, sep = ", ") %>% 
#     mutate(original_tree_names = tree_species_names) %>% 
#     mutate(original_seed_names = seed_species_names) %>% 
#     mutate(tree_species_names = trimws(gsub("[\"']", "", tree_species_names))) %>%
#     mutate(seed_species_names = trimws(gsub("[\"']", "", seed_species_names))) %>%
#     mutate(tree_species_names = gsub("\n", "", tree_species_names)) %>% 
#     mutate(seed_species_names = gsub("\n", "", seed_species_names))
#   
#   
#   
#   return(IMP_data_long)
#   
# }


preprocess_IMP_data <- function(IMP_data) {
  IMP_data <- clean_names(IMP_data)
  
  # Rename columns
  names(IMP_data)[grep("species", names(IMP_data), ignore.case = TRUE)][2] <- "tree_species"
  names(IMP_data)[grep("species", names(IMP_data), ignore.case = TRUE)][3] <- "seed_species"
  
  # Extract combined species and count data
  
  extract_combined_tree_data <- function(species_count_str) {
    species_pairs <- strsplit(species_count_str, "\\|")[[1]]
    combined_data <- sapply(species_pairs, function(pair) {
      parts <- strsplit(pair, ":")[[1]]
      paste(parts[1], parts[2], sep = ";") # Combine species name and count with ";"
    })
    paste(combined_data, collapse = ", ")
  }
  
  
  extract_combined_seed_data <- function(species_count_str) {
    species_pairs <- strsplit(species_count_str, "\\|")[[1]]
    combined_data <- sapply(species_pairs, function(pair) {
      parts <- strsplit(pair, ":")[[1]]
      paste(parts[2], parts[1], sep = ";") # Combine species name and count with ";", note the order is reversed
    })
    paste(combined_data, collapse = ", ")
  }
  
  IMP_modified <- IMP_data %>%
    mutate(tree_species_combined = sapply(tree_species, extract_combined_tree_data),
           seed_species_combined = sapply(seed_species, extract_combined_seed_data))
  
  # Separate rows and then split combined data
  IMP_data_long <- IMP_modified %>%
    mutate(row_id = row_number()) %>%
    separate_rows(tree_species_combined, sep = ", ") %>%
    separate_rows(seed_species_combined, sep = ", ") %>%
    mutate(tree_species_names = sapply(strsplit(tree_species_combined, ";"), `[`, 1),
           tree_species_count = sapply(strsplit(tree_species_combined, ";"), `[`, 2),
           seed_species_names = sapply(strsplit(seed_species_combined, ";"), `[`, 1),
           seed_species_count = sapply(strsplit(seed_species_combined, ";"), `[`, 2)) %>%
    select(-tree_species_combined, -seed_species_combined) %>% 
  
    mutate(original_tree_names = tree_species_names) %>% 
    mutate(original_seed_names = seed_species_names) %>% 
    mutate(tree_species_names = trimws(gsub("[\"']", "", tree_species_names))) %>%
    mutate(seed_species_names = trimws(gsub("[\"']", "", seed_species_names))) %>%
    mutate(tree_species_names = gsub("\n", "", tree_species_names)) %>% 
    mutate(seed_species_names = gsub("\n", "", seed_species_names))

  return(IMP_data_long)
}







update_IMP_data_existing_corrections <- function(processed_IMP_data, species_corrections){
  
  updated_species_list <- processed_IMP_data %>% 
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


# START HERE! original names are NOT the ones I want. 
get_unresolved_names <- function(updated_IMP_data){
 unresolved_tree_names <-  updated_IMP_data %>% 
   select(tree_species_names, new_tree_name) %>% 
   filter(is.na(new_tree_name)) %>% 
   pull(tree_species_names)
 
 unresolved_tree_names <- unique(unresolved_tree_names)
 
 unresolved_seed_names <-  updated_IMP_data %>% 
   select(seed_species_names, new_seed_name) %>% 
   filter(is.na(new_seed_name)) %>% 
   pull(seed_species_names)
 
 unresolved_seed_names <- unique(unresolved_seed_names)
 
 all_unresolved_species <- unique(c(unresolved_tree_names, unresolved_seed_names))
 
 return(all_unresolved_species)
 
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





resolve_species_names <- function(unresolved_names){
  
  # Pass names needing review to resolver
  resolved_df <- gnr_resolve(sci = unresolved_names, data_source_ids = c(165, 167), canonical = TRUE, best_match_only = TRUE)
  
  # If no names can be resolved (which will happen if you've resolved everything),
  # this returns an empty dataframe with the correct structure for next steps
  if (nrow(resolved_df) == 0) {
    resolved_df <- data.frame(
      user_supplied_name = character(0),
      matched_name2 = character(0),
      score = numeric(0),
      data_source_title = character(0),
      submitted_name = character(0)
    )
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







check_invasive_status <- function (species_to_check, prior_invasives_data, simplify = FALSE, ...) 
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
    
    all_invasives_data <- bind_rows(final_df, prior_invasives_data)
    return(all_invasives_data)
    
  }
}


create_invasives_report_v2 <- function(invasives_results, updated_IMP_data){
  if (is.null(invasives_results)){
    print("Nothing new to add to previous report.")
    invasives_report <- all_data$Invasives_Report
  }else{
    invasives_only <- invasives_results %>% 
      filter(status == 'Invasive')
    IMP_data_invasives <- updated_IMP_data %>% 
      filter(tree_species_names %in% invasives_only$species | seed_species_names %in% invasives_only$species)
    orgs_df <- IMP_data_invasives %>%
      group_by(tree_species_names) %>% 
      summarise(org_list = toString(unique(organization_name)),
                country_list = toString(unique(project_country)))
    invasives_report <- invasives_only %>% 
      left_join(orgs_df, by = c('species' ='tree_species_names'))
  }
  return(invasives_report)
}



save_invasives_data <- function(old_invasives_data, new_invasives_data){
  if(is.null(new_invasives_data)){
    print("No new data to save. Refer to previous file.")
    return(NULL)
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
  write.csv(new_invasives_data, file_name, row.names = FALSE)
  print(paste0("Updated invasive species data saved to: ", file_name))

}



save_invasives_report <- function(full_invasives_report, new_invasives_data){

  if(is.null(new_invasives_data)){
    print("No new data to report. Refer to previous report")
    return(NULL)
  }
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

save_IMP_data <- function(updated_IMP_data_2){
  
  tree_IMP_data <- updated_IMP_data_2 %>% 
    select(everything(), 
           -seed_species, 
           -seed_species_names, 
           -original_seed_names, 
           -new_seed_name,
           -seed_species_count) %>% 
    filter(tree_species_names != "")
  
  
  
  seed_IMP_data <- updated_IMP_data_2 %>% 
    select(everything(), 
           -tree_species, 
           -tree_species_names, 
           -original_tree_names, 
           -new_tree_name,
           -tree_species_count) %>% 
    filter(seed_species_names != "")
  
  species_data_path <- "Species_Data"
  
  # Check if the "Species_Data" directory exists, if not, create it
  if (!dir.exists(species_data_path)) {
    dir.create(species_data_path, recursive = TRUE)
  }
  
  # Define the filename with the current date and time
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  tree_file_name <- paste0(species_data_path, "/IMP_planted_trees_", date_info, ".csv")
  seed_file_name <- paste0(species_data_path, "/IMP_planted_seeds_", date_info, ".csv")
  
  # Write the corrections to the file
  write.csv(tree_IMP_data, tree_file_name, row.names = FALSE)
  print(paste0("IMP planted tree data saved to: ", tree_file_name))
  write.csv(seed_IMP_data, seed_file_name, row.names = FALSE)
  print(paste0("IMP planted seed data saved to: ", seed_file_name))
  
  return(list(IMP_tree_data = tree_IMP_data, IMP_seed_data = seed_IMP_data))
}




all_data <- load_data()
processed_IMP_data <- preprocess_IMP_data(all_data$IMP_data)
updated_IMP_data <- update_IMP_data_existing_corrections(processed_IMP_data, all_data$Species_Corrections)
unresolved_names <- get_unresolved_names(updated_IMP_data)
resolved_names <- resolve_species_names(unresolved_names)
finished_names <- manual_validation(resolved_names)

updated_corrections <- update_corrections_file(all_data$Species_Corrections, finished_names)
updated_IMP_data_2 <- update_IMP_data_existing_corrections(updated_IMP_data, updated_corrections)

species_list <- create_species_list_v2(updated_IMP_data = updated_IMP_data_2, all_data$Invasive_Species_Data)
invasives_results <- check_invasive_status(species_list$Species, prior_invasives_data = all_data$Invasive_Species_Data)
invasives_report <- create_invasives_report_v2(invasives_results, updated_IMP_data_2)
save_invasives_data(old_invasives_data = all_data$Invasive_Species_Data, new_invasives_data = invasives_results)
save_invasives_report(invasives_report, invasives_results)
IMP_data <- save_IMP_data(updated_IMP_data_2)




# 
# 
# 
# 
# 
# 
# preprocess_IMP_data <- function(IMP_data){
#   
#   IMP_data <- clean_names(IMP_data)
#   
#   names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][2] <- "tree_species"
#   names(IMP_data)[grep("species", names(IMP_data), ignore.case = T)][3] <- "seed_species"
#   
#   extract_tree_species_names <- function(species_count_str) {
#     # Replace the ":count" part with an empty string and "|" with ", "
#     species_names = gsub(":\\d+", "", species_count_str)
#     gsub("\\|", ", ", species_names)
#   }
#   
#   
#   
#   extract_seed_species_names <- function(species_count_str) {
#     # Replace the "\\d+:" part with an empty string and "|" with ", "
#     species_names = gsub("\\d+:", "", species_count_str)
#     gsub("\\|", ", ", species_names)
#   }
#   
#   extract_tree_species_count <- function(species_count_str) {
#     species_pairs <- strsplit(species_count_str, "\\|")[[1]]
#     species_counts <- sapply(species_pairs, function(pair) {
#       parts <- strsplit(pair, ":")[[1]]
#       return(parts[2]) # parts[2] is the count part
#     })
#     paste(species_counts, collapse = ", ")
#   }
#   
#   extract_seed_species_count <- function(species_count_str) {
#     species_pairs <- strsplit(species_count_str, "\\|")[[1]]
#     species_counts <- sapply(species_pairs, function(pair) {
#       parts <- strsplit(pair, ":")[[1]]
#       return(parts[2]) # parts[2] is the count part
#     })
#     paste(species_counts, collapse = ", ")
#   }
#   
#   
#   
#   IMP_simple <- IMP_data %>% select(project_name, project_country, organization_name, site_id, site_name, tree_species, seed_species)
#   IMP_modified <- IMP_simple %>% mutate(tree_species_names = extract_tree_species_names(IMP_simple$tree_species),
#                                         tree_species_count = extract_tree_species_count(IMP_simple$tree_species),
#                                         seed_species_names = extract_seed_species_names(IMP_simple$seed_species),
#                                         seed_species_count = extract_seed_species_count(IMP_simple$seed_species))
#   
#   IMP_data_long <- IMP_modified %>%
#     mutate(row_id = row_number()) %>%
#     separate_rows(tree_species_names, sep = ", ") %>%
#     separate_rows(seed_species_names, sep = ", ") %>% 
#     mutate(original_tree_names = tree_species_names) %>% 
#     mutate(original_seed_names = seed_species_names) %>% 
#     mutate(tree_species_names = trimws(gsub("[\"']", "", tree_species_names))) %>%
#     mutate(seed_species_names = trimws(gsub("[\"']", "", seed_species_names))) %>%
#     mutate(tree_species_names = gsub("\n", "", tree_species_names)) %>% 
#     mutate(seed_species_names = gsub("\n", "", seed_species_names))
#   
#   
#   
#   return(IMP_data_long)
#   
# }
# 










