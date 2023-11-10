# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Author: Johannes Nelson
# Input: The uncorrected tree data CSV extracted using data extraction script and
# the most recent taxonomic corrections CSV (either the master copy created by me,
# or a more recent update created from running this script)
# Outputs: A CSV with the tree data and corrected species names, as well as an
# updated corrections CSV with any new updates.

# This script is built around the gnr_resolve() function from the taxize package.
# This function takes a list of scientific names candidates and checks databases
# for close matches. Before passing the list of species names to this function, the
# script first preprocesses the species entries to remove white space and unwanted
# formatting that can seriously slow down gnr_resolve and the manual validation
# process. It then makes all the corrections it knows to make based on the most recent
# corrections file. After this, it only passes entries to the function for which there
# is no clear match, and then asks the user to weigh in on these entries.
#
# During this manual validation part of the script, the user will be shown the
# species name that was originally submitted and prompted to manually enter the
# correct name, if possible.
#
# The more there is to resolve, the longer it takes the gnr_resolve() function to
# run (can take >10 minutes if there are thousands of new entries), and the longer
# it will take to manually validate. Since I performed manual validation for a bulk
# of the dataset, subsequent runs should only be for any new entries since the last
# time it was run.
#
# The script outputs two files: an updated Taxonomic_Corrections file and a
# Corrected_Tree_Data file. The updated corrections file will become the new
# auto-updater the next time this script is run, correcting all known errors rapidly.
# The corrected tree data is what should ultimately be passed to the analysis steps,
# though it should be noted that any names that could not be resolved by either
# gnr_resolve or manual validation will also be passed on for analysis.
# -------------------------------------------------------------------------------



# Check and install missing packages
necessary_packages <- c("taxize", "dplyr")

for (pkg in necessary_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(pkg, "not found. Installing now...\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


#' 1. Load Data
#'
#' This function prompts the user to designate the desired dataset (Primary or
#' Brazil), and loads in the relevant tree data. It also checks the corrections
#' folder for the most recent corrections data. 
#'
#' @return List with two dataframes: tree data and corrections data--as well as 
#' a filepath for later reference.
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
  tree_files <- list.files(path = raw_data_path, pattern = "Tree_Data_Uncorrected", full.names = TRUE)

  # Sort files by modification date to get the most recent and read this into session
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]
  tree_data <- read.csv(latest_tree_file, check.names = FALSE)

  # tree_data <- tree_data[1:300, ]

  # Find most recent taxonomic corrections, if any exists (at the very least the
  # "_MASTER" copy should still exist from my initial corrections).
  corrections_path <- "Taxonomic_Corrections"
  corrections_files <- list.files(path = corrections_path, pattern = "Taxonomic_Corrections", full.names = TRUE)

  # Conditional that handles the unlikely case that you are starting from scratch
  if (length(corrections_files) > 0) {
    latest_corrections_file <- corrections_files[order(file.info(corrections_files)$mtime, decreasing = TRUE)[1]]
    corrected_names <- read.csv(latest_corrections_file)
    print(paste0("Latest correction file: ", latest_corrections_file))
  } else {
    corrected_names <- NULL
    print("No corrections found. Starting from scratch.")
  }
  print(paste0("Latest tree data file: ", latest_tree_file))

  return(list(tree_data = tree_data, corrected_names = corrected_names, raw_data_path = raw_data_path))
}






#' 2. Update Tree Data with Existing Corrections
#'
#' The first part of this function cleans up the species names before moving
#' forward, which helps dramatically decrease the runtime and manual
#' validation effort required. It trims leading or trailing white space and
#' removes any quotation marks at the beginning or end of entries, which many
#' users included. It also removes the 'newline' character that gets introduced
#' when users entered multiple species on multiple lines in the same box (besides
#' being an unwanted way to collect data this also completely messes up the
#' resulting corrections file). After cleaning, it makes corrections based on any
#' corrections made during prior runs of this script.
#'
#' Note: if ever wanting only to make updates based on prior corrections without
#' running through the whole script, this function can do so on its own.
#'
#' @param  tree_data The uncorrected tree data dataframe
#' @param corrected_names The most recent corrections dataframe
#' @return Dataframe of tree data with cleaned and updated species names
update_tree_data_with_existing_corrections <- function(tree_data, corrected_names) {
  # Preprocess names by clearing unwatned formatting
  cleaned_tree_data <- tree_data %>%
    mutate(submitted_species_name = Species) %>%
    mutate(Species = trimws(gsub("[\"']", "", Species))) %>%
    mutate(Species = gsub("\n", "", Species))

  # Joins the uncorrected data with the corrected data, to make known corrections
  if (!is.null(corrected_names)) {
    updated_data <- cleaned_tree_data %>%
      left_join(corrected_names, by = "Species") %>%
      # matched_name2 represents corrected names. If this is NA, originally
      # originally submitted name is retained. If not, the corrected name is used.
      mutate(Species = ifelse(is.na(matched_name2), Species, matched_name2)) %>%
      # If there was no matched_name2 entry, the species name needs review
      mutate(name_validation = ifelse(is.na(matched_name2), "Needs review", "Resolved"))
  } else {
    # Handles the unlikely case that no corrections have been made and all still
    # need review
    updated_data <- cleaned_tree_data %>%
      mutate(
        submitted_species_name = Species,
        name_validation = "Needs review"
      )
  }
  return(updated_data)
}




#' 3. Resolve Uncorrected Names
#'
#' This function calls the gnr_resolve() function on any names that have yet to
#' be resolved. It checks two different databases: Tropicos - Missouri Botanical
#' Garden and The International Plant Names Index. I chose these because Tim had
#' chosen them. Others can be chosen if desired.
#'
#' @param  updated_data The updated and cleaned tree data
#' @return Dataframe withi only the species names that could be resolved by the
#' gnr_resolve() function.
resolve_uncorrected_names <- function(updated_data) {
  # Grabs one of each species name entry that still requires review
  unresolved_names <- updated_data %>%
    filter(name_validation == "Needs review") %>%
    pull(Species) %>%
    unique()

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

  return(resolved_df)
}




#' 4. Create Complete Corrections Dataframe
#'
#' This step takes the new names that the resolver found and adds them to the
#' existing database in order to create a complete table of names with corrections
#' and names that need to be corrected.
#'
#'
#' @param  updated_data The updated and cleaned tree data
#' @param  resolved_dataframe The dataframe resulting from resolve_names() call
#' @param  corrected_names The most recent corrections file
#' @return Dataframe withi only the species names that could be resolved by the
#' gnr_resolve() function.
create_complete_corrections_df <- function(updated_data, resolved_dataframe, corrected_names) {
  # If no corrections file was present (unlikely)
  if (is.null(corrected_names)) {
    complete_df <- updated_data %>%
      left_join(resolved_dataframe, by = c("Species" = "user_supplied_name")) %>%
      select(Species, matched_name2, score, data_source_title)
  } else {
    # If a corrections file was present, there will be repeat column names that
    # need to be dealt with.
    complete_df <- updated_data %>%
      # Adds a suffix to columns to make clear old and new corrections
      left_join(resolved_dataframe, by = c("Species" = "user_supplied_name"), suffix = c(".old", ".new")) %>%
      mutate(
        # Pulls old and new corrections together, favoring the new (although there
        # should be no cases where there is a value in both old and new columns)
        matched_name2 = coalesce(matched_name2.new, matched_name2.old),
        score = coalesce(score.new, score.old),
        data_source_title = coalesce(data_source_title.new, data_source_title.old)
      ) %>%
      select(Species, matched_name2, score, data_source_title)
  }
  return(complete_df)
}


#' 5. Manual Validation
#'
#' This step is interactive, and if there have been many new entries since the
#' last time it was run, can take a lot of human time. For every unique name that
#' has no correction entry, this will prompt the user for input. Some names will
#' be close enough to scientific names that a google search will suffice. Others
#' might be common names, and there are a variety of entries that are unresolvable
#' (i.e. numbers, NAs, etc.).
#'
#' Note: There is a hidden 'save' feature that would allow a user to type 'save'
#' into the console, the rest of the script will run in the midst of these
#' corrections. This should work fine, but is the least 'tested' part of the code.
#' I added it for personal use when making the bulk corrections, since this took
#' multiple hours. If possible, I would just make all corrections in one go without
#' saving to avoid potential confusion.
#'
#' @param  complete_df The full dataframe of corrections/names needing corrections
#' @return Dataframe of corrections with both gnr_resolve corrections and manual
#' validation corrections. There is a column called data_source_title that will
#' show the source of the correction.

manual_validation <- function(complete_df) {
  # Extract unique unresolved species names
  unresolved_unique <- complete_df %>%
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
      return(complete_df)

      # If user does not leave input blank, update all entries with matching name
      # with user correction, and record data_source_title as 'Manual validation'
    } else if (new_name != "") {
      complete_df$matched_name2[complete_df$Species == species] <- new_name
      complete_df$data_source_title[complete_df$Species == species] <- "Manual validation"
    }
    # Every 25 species, report on how many are finished and remaining
    if (count_processed %% 25 == 0) {
      cat(paste(count_processed, "species processed. You have", total_to_correct - count_processed, "remaining...\n"))
    }
  }

  return(complete_df)
}



#' 6. Save Updated Corrections
#'
#' This step will save an updated corrections file that contains all past corrections
#' as well as all new corrections from either the resolve_names() function or the
#' manual_validation() function. It saves the file with a date and time stamp in
#' order to make it clear what is the most recent. These results will be stored
#' in the 'Taxonomic Corrections' folder and can be shared across both Brazil and
#' Primary datasets.
#'
#' @param  final_df The dataframe after undergoing manual validation, which contains
#' all new corrections
#' @param  corrected_names The prior corrections to which the new ones will be
#' appended
#' @return A full corrections CSV that will be saved in the Taxonomic_Corrections
#' folder (as well as in the R environment)
save_updated_corrections <- function(final_df, corrected_names) {
  # Combine old and new corrections
  all_corrections <- bind_rows(final_df, corrected_names) %>%
    # Filter out all NAs--these are not yet resolved
    filter(!is.na(matched_name2)) %>%
    # Remove duplicates if they exist
    distinct(Species, .keep_all = TRUE)

  # Define the path for corrections
  corrections_path <- "Taxonomic_Corrections"

  # Check if the "Species_Corrections" directory exists, if not, create it
  if (!dir.exists(corrections_path)) {
    dir.create(corrections_path, recursive = TRUE)
  }

  # Define the filename with the current date and time
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  file_name <- paste0(corrections_path, "/Taxonomic_Corrections_", date_info, ".csv")

  # Write the corrections to the file
  write.csv(all_corrections, file_name, row.names = FALSE)
  print(paste0("Updated species corrections saved to: ", file_name))
  return(all_corrections)
}

#' 7. Save Corrected Tree Data
#'
#' Here we update the tree data with all corrections--old and new--and save a file
#' called 'Corrected_Tree_Data_xxx.CSV.' Unresolved names will still be unresolved
#' in this file.
#'
#' @param  tree_data The tree_data (after preprocessing step)
#' @param  all_corrections The new, full corrections datagrame
#' @param  raw_data_path the bath to either 'Brazil' or 'Main' data
#' @return An updated tree data file with all known corrections made

save_corrected_tree_data <- function(tree_data, all_corrections, raw_data_path) {
  updated_tree_data <- tree_data %>%
    # Join old data with new corrections
    left_join(all_corrections, by = "Species", suffix = c(".old", ".new")) %>%
    mutate(
      # Pulls old and new corrections together, favoring the new (although there
      # should be no cases where there is a value in both old and new columns)
      matched_name2 = coalesce(matched_name2.new, matched_name2.old),
      score = coalesce(score.new, score.old),
      data_source_title = coalesce(data_source_title.new, data_source_title.old)
    ) %>%
    # Change species column to reflect new corrections
    mutate(Species = ifelse(is.na(matched_name2), Species, matched_name2)) %>%
    mutate(name_validation = ifelse(is.na(matched_name2), "Needs Review", "Resolved")) %>%
    select(
      -names(.)[grep(".old|.new", names(.))],
      -matched_name2
    )

  # Stamp file name with date and time; save file
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  write.csv(updated_tree_data, paste0(raw_data_path, "/Corrected_Tree_Data_", date_info, ".csv"), row.names = FALSE)
  print(paste0("Corrected tree data saved to: ", raw_data_path, "/Corrected_Tree_Data_", date_info, ".csv"))

  return(updated_tree_data)
}




#' 8. Save Unresolved Names
#'
#' An optional question will appear after all scripts have run that asks if you
#' would like to save unresolved species names to the disk. This may be superfluous
#' because you can also just filter the corrected tree data by name_validation, but
#' this provides a quick way to skip that step if desired.
#'
#' @param  updated_tree_data The recently updated tree data with new corrections
#' @param  raw_data_path the bath to either 'Brazil' or 'Main' data
#' @return An updated tree data file with all known corrections made

save_unresolved_names <- function(updated_tree_data, raw_data_path) {
  save_prompt <- tolower(readline(prompt = paste0(
    "Would you like to save a CSV with all ",
    "unresolved species names? \n Enter y/n"
  )))

  if (save_prompt == "y") {
    date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
    needs_review <- updated_tree_data %>%
      filter(name_validation == "Needs Review")
    write.csv(needs_review, paste0(raw_data_path, "/Species_for_Review_", date_info, ".csv"))
    print(paste0("Species in need of review saved to: ", raw_data_path, "/Species_for_Review_", date_info, ".csv"))
  } else {
    needs_review <- NULL
  }
  return(needs_review)
}


#-------------------------------------------------------------------------------
# The above script defines all the functions. The 'main' script below calls them
# each in turn. By having distinct modules, errors/bugs that might arise in the
# future will be easier to diagnose. The print() statements output at each step
# in the console can help locate where things went wrong, and the relevant 
# function above will be a good starting point for debugging.
#-------------------------------------------------------------------------------

# 1. Load Data
print("Loading Data")
data <- load_data()

# 2. Update Tree Data with Existing Corrections
print("Updating Data with Existing Corrections")
updated_data <- update_tree_data_with_existing_corrections(data$tree_data, data$corrected_names)

# 3. Resolve Uncorrected Names
print(paste0(
  "Passing Uncorrected Names to Global Names Resolver Database.",
  "This step can take a very long time if there are many new names."
))
resolved_dataframe <- resolve_uncorrected_names(updated_data)

# 4. Create Complete Corrections Dataframe
print("Adding newly found matches to existing corrections data.")
complete_df <- create_complete_corrections_df(updated_data, resolved_dataframe, data$corrected_names)

# 5. Manual Validation
print("Beginning manual validation...")
final_df <- manual_validation(complete_df)

# 6. Save Updated Corrections
print("Saving Updated Corrections")
all_corrections <- save_updated_corrections(final_df, data$corrected_names)

# 7. Save Corrected Tree Data
print("Saving Corrected Tree Data")
updated_tree_data <- save_corrected_tree_data(updated_data, all_corrections, data$raw_data_path)

# 8. Save Unresolved Names
needing_review <- save_unresolved_names(updated_tree_data, data$raw_data_path)
