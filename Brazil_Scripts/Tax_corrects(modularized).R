library(taxize)
library(dplyr)


choose_dataset <- function(){
  answer <- readline(prompt = "Which Dataset are you correcting for? Enter '1' for the Primary Dataset or '2' for the Brazil Dataset:")
  if (!answer %in% c('1', '2')) {print("Invalid response, please enter either '1' or '2'")}
  else if (answer == '1') {
    raw_data_path <- "Main_Raw_Data"
  }
  else if (answer == '2'){
    raw_data_path <- "Brazil_Raw_Data"
  }
  return(raw_data_path)
}

load_data <- function() {
  # Define the path to the "Brazil_Raw_Data" folder

  # Get all files that match the pattern "Tree_Data_Full" in the "Brazil_Raw_Data" folder
  tree_files <- list.files(path = raw_data_path, pattern = "Tree_Data_Uncorrected", full.names = TRUE)

  # Sort files by modification date to get the most recent
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]

  tree_data <- read.csv(latest_tree_file, check.names = FALSE)

  # tree_data <- tree_data[1:300,]
  
  # Do the same for "Taxonomic_Corrections" in the "Raw_Data" folder
  corrections_files <- list.files(path = raw_data_path, pattern = "Taxonomic_Corrections", full.names = TRUE)

  if (length(corrections_files) > 0) {
    latest_corrections_file <- corrections_files[order(file.info(corrections_files)$mtime, decreasing = TRUE)[1]]
    corrected_names <- read.csv(latest_corrections_file)
  } else {
    corrected_names <- NULL
  }

  return(list(tree_data = tree_data, corrected_names = corrected_names))
}



preprocess_species_names <- function(tree_data) {
  cleaned_tree_data <- tree_data %>% 
    mutate(submitted_species_name = Species) %>%
    mutate(Species = trimws(gsub("[\"']", "", Species))) %>% 
    mutate(Species = gsub("\n", "", Species))  # This line removes newline characters

}




update_tree_data_with_existing_corrections <- function(cleaned_tree_data, corrected_names) {
  if (!is.null(corrected_names)) {
    updated_data <- cleaned_tree_data %>%
      left_join(corrected_names, by = "Species") %>%
      mutate(Species = ifelse(is.na(matched_name2), Species, matched_name2)) %>%
      mutate(name_validation = ifelse(is.na(matched_name2), "Needs review", "Resolved"))
  } else {
    updated_data <- cleaned_tree_data %>%
      mutate(
        submitted_species_name = Species,
        name_validation = "Needs review"
      )
  }
  return(updated_data)
}




get_unresolved_names <- function(updated_data) {
  unresolved_names <- updated_data %>%
    filter(name_validation == "Needs review") %>%
    pull(Species) %>%
    unique()
  return(unresolved_names)
}



resolve_names <- function(unresolved_names) {
  resolved_df <- gnr_resolve(sci = unresolved_names, data_source_ids = c(165, 167), canonical = TRUE, best_match_only = TRUE)
  
  # Check if the dataframe is empty
  if (nrow(resolved_df) == 0) {
    # Return a dataframe with the expected structure but no rows
    return(data.frame(
      user_supplied_name = character(0),
      matched_name2 = character(0),
      score = numeric(0),
      data_source_title = character(0)
    ))
  }
  

  return(resolved_dataframe_short)
}




# 
# resolve_names <- function(unresolved_names) {
#   resolved_df <- gnr_resolve(sci = unresolved_names, data_source_ids = c(165, 167), canonical = TRUE, best_match_only = TRUE)
# 
#   # Check if the dataframe is empty
#   if (nrow(resolved_df) == 0) {
#     # Return a dataframe with the expected structure but no rows
#     return(data.frame(
#       user_supplied_name = character(0),
#       matched_name2 = character(0),
#       score = numeric(0),
#       data_source_title = character(0)
#     ))
#   }
# 
#   resolved_dataframe_short <- resolved_df %>%
#     group_by(user_supplied_name) %>%
#     arrange(desc(score)) %>%
#     slice(1) %>%
#     ungroup()
# 
#   return(resolved_dataframe_short)
# }




# manual_validation <- function(df) {
#   total_to_correct <- sum(is.na(df$matched_name2))
#   cat(paste("You have", total_to_correct, "corrections to make...\n"))
#   
#   for (i in 1:nrow(df)) {
#     if (is.na(df$matched_name2[i])) {
#       
#       cat(paste("Unable to resolve:", df$Species[i], "\n"))
#       new_name <- readline(prompt = "Please provide the correct name (or press Enter to skip): ")
#       
#       # If the user types "save", save the current state of df and continue
#       if (new_name == "save") {
#         cat("Progress saved. You can resume from where you left off.\n")
#         return(df)
#       } else if (new_name != "") {
#         df$matched_name2[i] <- new_name
#         df$data_source_title[i] <- "Manual validation"
#       }
#     }
#   }
#   return(df)
# }


create_complete_df <- function(updated_data, resolver_dataframe_short, corrected_names) {
  if (is.null(corrected_names)) {
    # If no corrections file was present
    complete_df <- updated_data %>%
      left_join(resolver_dataframe_short, by = c("Species" = "user_supplied_name")) %>%
      select(Species, matched_name2, score, data_source_title)
  } else {
    # If a corrections file was present
    complete_df <- updated_data %>%
      left_join(resolver_dataframe_short, by = c("Species" = "user_supplied_name"), suffix = c(".old", ".new")) %>%
      mutate(
        matched_name2 = coalesce(matched_name2.new, matched_name2.old),
        score = coalesce(score.new, score.old),
        data_source_title = coalesce(data_source_title.new, data_source_title.old)
      ) %>%
      select(Species, matched_name2, score, data_source_title)
  }
  return(complete_df)
}




manual_validation <- function(df) {
  
  # Extract unique unresolved species names
  unresolved_unique <- df %>%
    filter(is.na(matched_name2)) %>%
    distinct(Species) %>%
    pull(Species)
  
  total_to_correct <- length(unresolved_unique)
  cat(paste("You have", total_to_correct, "unique unresolved species names to correct...\n"))
  
  for (species in unresolved_unique) {
    cat(paste("Unable to resolve:", species, "\n"))
    new_name <- readline(prompt = "Please provide the correct name (or press Enter to skip): ")
    
    # If the user types "save", save the current state of df and continue
    if (new_name == "save") {
      cat("Progress saved. You can resume from where you left off.\n")
      return(df)
    } else if (new_name != "") {
      # Update all occurrences of that name in the dataframe
      df$matched_name2[df$Species == species] <- new_name
      df$data_source_title[df$Species == species] <- "Manual validation"
    }
  }
  
  return(df)
}









save_updated_corrections <- function(final_df, corrected_names) {
  # Combine old and new corrections, and remove duplicates
  all_corrections <- bind_rows(final_df, corrected_names) %>%
    filter(!is.na(matched_name2)) %>%
    distinct(Species, .keep_all = TRUE)

  date_info <- Sys.Date()

  write.csv(all_corrections, paste0(raw_data_path,"/Taxonomic_Corrections_", date_info, ".csv"), row.names = FALSE)
  print(paste0("Updated species corrections saved to: ", raw_data_path, "/Taxonomic_Corrections_", date_info, ".csv"))
  return(all_corrections)
}


save_corrected_tree_data <- function(tree_data, all_corrections) {
  # Update tree_data with all corrections
  updated_tree_data <- tree_data %>%
    # mutate(submitted_species_name = Species) %>%
    # mutate(Species = trimws(gsub("[\"']", "", Species))) %>%
    left_join(all_corrections, by = "Species") %>%
    mutate(Species = ifelse(is.na(matched_name2), Species, matched_name2)) %>%
    select(-matched_name2) # Remove the matched_name2 column

  date_info <- Sys.Date()
  write.csv(updated_tree_data, paste0(raw_data_path, "/Corrected_Tree_Data_", date_info, ".csv"), row.names = FALSE)
  print(paste0("Corrected tree data saved to: ", raw_data_path, "/Corrected_Tree_Data_", date_info, ".csv"))
  
  return(updated_tree_data)
}



raw_data_path <- choose_dataset()
data <- load_data()
preprocessed_tree_data <- preprocess_species_names(data$tree_data)
updated_data <- update_tree_data_with_existing_corrections(preprocessed_tree_data, data$corrected_names)
unresolved_names <- get_unresolved_names(updated_data)
resolved_dataframe_short <- resolve_names(unresolved_names)
complete_df <- create_complete_df(updated_data, resolved_dataframe_short, data$corrected_names)
final_df <- manual_validation(complete_df)

# Save updated corrections
all_corrections <- save_updated_corrections(final_df, data$corrected_names)

# Save updated tree_data
updated_tree_data <- save_corrected_tree_data(preprocessed_tree_data, all_corrections)


