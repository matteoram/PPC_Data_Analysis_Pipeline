# ------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Script: Add Family Names
# Author: Johannes Nelson
# Input: The most recent 'Corrected' tree data file (after having passed through
# the correct species name script), along with the most recently updated 'Family_Names'
# file, if one exists.

# This script is built around the GBIF database where it searches for names. It
# first checks to see if a family names  file exists (which it should,
# since I ran th is script over a bulk of the data when I completed it.). It
# automatically adds family names for species where there is a match in the
# existing file. It also saves records that have been passed to the function prior
# without success to avoid needlessly asking the function to go over names for which
# there will be no match. Each running of the script will then pass only the new
# species entries for which there is no existing match, and it will then update
# the corrections file with any new findings as well as creating a new, 'Final'
# tree data file that is prepped for analysis. This is the file that will be passed
# to the next script which generates reports. There is also the option to manually
# add family name information for unknown entries incorporated into the script.

# ------------------------------------------------------------------------------



# Check for and install/load necessary packages
necessary_packages <- c("taxize", "dplyr", "tidyr", "rgbif")

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
#' folder for the most recent family names data.
#' @return List with two dataframes: tree data & existing family names, as well
#' as a filepath variable for later reference.

load_data <- function() {
  # Determine which dataset is being corrected and set path to desired folder
  answer <- readline(prompt = cat("Which Dataset are you correcting for? \n Enter '1' for the Primary Dataset or '2' for the Brazil Dataset:"))
  if (!answer %in% c("1", "2")) {
    print("Invalid response, please rerun script and be sure to enter either '1' or '2'")
  } else if (answer == "1") {
    raw_data_path <- "Main_Data"
  } else if (answer == "2") {
    raw_data_path <- "Brazil_Data"
  }

  # Get all files that match the pattern "Tree_Data_Uncorrected"
  tree_files <- list.files(path = raw_data_path, pattern = "Corrected_Tree_Data", full.names = TRUE)

  # Sort files by modification date to get the most recent and read this into session
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]
  tree_data <- read.csv(latest_tree_file, check.names = FALSE)


  # Find most recent Taxonomic_Ranks
  species_data_path <- "Species_Data"
  family_names_files <- list.files(path = species_data_path, pattern = "Family_Names", full.names = TRUE)

  # Conditional that handles the unlikely case that you are starting from scratch
  if (length(family_names_files) > 0) {
    latest_fam_name_file <- family_names_files[order(file.info(family_names_files)$mtime, decreasing = TRUE)[1]]
    existing_family_names <- read.csv(latest_fam_name_file)
    print(paste0("Latest Family Names file: ", latest_fam_name_file))
  } else {
    existing_family_names <- NULL
    print(paste0("No taxonomic rank file found. Starting from scratch."))
  }
  print(paste0("Latest tree data file: ", latest_tree_file))

  return(list(tree_data = tree_data, existing_family_names = existing_family_names, raw_data_path = raw_data_path))
}







#' 2. Add Existing Family Names
#'
#' This function takes the most recent family names file, and automatically
#' updates the incoming tree data with matches in the file.
#' Note: if ever wanting only to make updates based on prior corrections without
#' running through the whole script, this function can do so on its own.
#'
#' @param  tree_data The tree data dataframe (loaded in with load_data())
#' @param existing_family_names the family names corrections (loaded in with
#' load_data())
#' @return Dataframe of tree data with family names added (if corrections exist)

add_existing_family_names <- function(tree_data, existing_family_names) {
  if (!is.null(existing_family_names)) {
    updated_tree_data <- left_join(tree_data,
      existing_family_names,
      by = c("Species" = "query")
    )
  } else {
    updated_tree_data <- tree_data
  }
  return(updated_tree_data)
}



#' 3. Get Family Names
#'
#' There are actually two functions here: get_family_names and get_family_from_GBIF.
#' I opted to use the second because it was faster and required no API key setup.
#' Should someone later decide they want to use the first and get family names from
#' a different database (e.g. ncbi), they can swamp out the names of these functions
#' in the main script.
#'
#' @param  updated_tree_data The updated tree data dataframe
#' @param existing_family_names The family names corrections dataframe
#' @return Dataframe with new family names found in this pass of tax_name()

get_family_names <- function(updated_tree_data, existing_family_names) {
  # Check if 'family' column exists
  if ("family" %in% names(updated_tree_data)) {
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

  if (!is.null(existing_family_names)) {
    species_to_skip <- existing_family_names %>%
      filter(is.na(family)) %>%
      distinct(query) %>%
      pull(query)

    species_to_lookup <- setdiff(species_to_lookup, species_to_skip)
  }

  species_to_lookup <- species_to_lookup[!species_to_lookup %in% c(".", ",", "")]


  # Retrieve family names from taxonomic database
  if (length(species_to_lookup) > 0) {
    print(paste0("There are ", length(species_to_lookup), "distinct species being queried. This can take some time to run and will require periodic input from the user."))
    new_family_names <- tax_name(species_to_lookup, get = "family", db = "ncbi")
    new_family_names <- new_family_names
    # %>%
    #   select(-db)
  } else {
    new_family_names <- data.frame(
      query = character(0),
      family = character(0)
    )
  }

  return(new_family_names)
}




# This is the function actually being used in the main script
get_family_from_GBIF <- function(updated_tree_data, existing_family_names) {
  # Find species for which there are no family names
  if ("family" %in% names(updated_tree_data)) {
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

  # Use existing names to find out what has already been resolved
  if (!is.null(existing_family_names)) {
    species_to_skip <- existing_family_names %>%
      filter(is.na(family)) %>%
      distinct(query) %>%
      pull(query)

    species_to_lookup <- setdiff(species_to_lookup, species_to_skip)
  }
  # Take out errant punctuation that will trip up the process
  species_to_lookup <- species_to_lookup[!species_to_lookup %in% c(".", ",", "")]
  species_to_lookup <- species_to_lookup[!is.na(species_to_lookup)]

  # If there are species to look up, begin querying GBIF
  if (length(species_to_lookup) > 0) {
    # Instantiate dataframe for results
    new_family_names <- data.frame(query = species_to_lookup, family = NA, db = "GBIF")
    # Tell user how many there are to look up
    print(paste0("There are ", length(species_to_lookup), " distinct species being queried. This can take some time to run and will require periodic input from the user."))
    # Loop through names, querying GBIF for taxonomic family
    for (i in 1:length(species_to_lookup)) {
      message(paste("Processing: ", species_to_lookup[i], "... \n"))
      out <- name_backbone(species_to_lookup[i])
      if (!is.null(out$family)) {
        new_family_names$family[i] <- out$family
        cat("Match found for", species_to_lookup[i], "Family name: ", out$family, "\n")
      } else {
        cat("No family name found for", species_to_lookup[i], "... \n")
        new_family_names$family[i] <- NA
      }
    }
  } else {
    new_family_names <- data.frame(query = character(0), family = character(0), db = character(0))
  }
  return(new_family_names)
}


#' 4. Manually add new family names
#'
#' This function takes all species that still have no associated family name data
#' and shows the user the species one by one. Often no family name data will be
#' due to common names being used, but there are examples where even well formed
#' scientific names may not return a match. Users can do these manually. This
#' can become pretty time intensive depending on the amount of new data since
#' last the last time it was run. Since I will have done a bulk of the major species
#' pool, though, this should be manageable unless there are suddenly numerous new
#' species appearing in monitoring plots.
#'
#'
#' @param  new_family_names The new automatically found names from the tax_name()
#' function in the prior module
#' @param old_family_names Any prior family names corrections dataframe (loaded into
#' the all_data object)
#' @return a dataframe with all new corrections, automatic and manual, that will
#' be used to update tree data and the family names CSV.
manually_find_family_names <- function(new_family_names, old_family_names, tree_data) {
  # Ask if user wants to perform manual validation
  response <- tolower(readline(prompt = "Would you like to manually enter family names for species that could not be automatically matched (This process can take time)? Enter y/n: "))
  all_family_names <- rbind(new_family_names, old_family_names)
  if (response == "n") {
    return(all_family_names)
  }
  # Define all species with no family name
  all_unresolved_family_names <- all_family_names %>%
    filter(is.na(family)) %>%
    distinct(query) %>%
    pull(query)

  unresolved_family_names <- all_unresolved_family_names[all_unresolved_family_names %in% tree_data$Species]

  # Let user know how many they will have to resolve
  total_to_validate <- length(unresolved_family_names)
  cat(paste("You have", total_to_validate, "unique unresolved family names to check...\n"))
  count_processed <- 0
  # Loop through and provide user space to manually input correct names
  for (species in unresolved_family_names) {
    count_processed <- count_processed + 1
    cat(paste("Unable to resolve:", species, "\n"))
    new_name <- readline(prompt = "Please provide the family name (or press Enter to skip): ")
    # Option to break loop, saving what has been done
    if (new_name == "save") {
      cat("Progress saved. You can resume from where you left off.\n")
      return(all_family_names)
      # If input is not left blank, replace NA with user-provided name
    } else if (new_name != "") {
      all_family_names$family[all_family_names$query == species] <- new_name
      all_family_names$db[all_family_names$query == species] <- "Manual validation"
    }
    # Every 25 rows, tell user updated count remaining
    if (count_processed %% 25 == 0) {
      cat(paste(count_processed, "species processed. You have", total_to_validate - count_processed, "remaining...\n"))
    }
  }
  return(all_family_names)
}



#' 5. Add New Family Names
#'
#' This function adds the new family names just found to the tree data so that
#' all possible matches have been updated.
#'
#' @param  updated_tree_data The updated tree dataframe with prior
#' corrections already made
#' @param existing_family_names The family names corrections dataframe (to handle
#' joins with duplicate column names)
#' @return Dataframe with tree data and all new and previously existing family
#' names added


add_new_family_names <- function(updated_tree_data, all_family_names, existing_family_names) {
  # If there were no existing names (unlikely) perform simple join with new updates
  # to the tree data
  if (is.null(existing_family_names)) {
    tree_data_with_family <- left_join(updated_tree_data,
      all_family_names,
      by = c("Species" = "query")
    )
  } else {
    # If there were (likely), handle duplicate columns by providing suffixes, and
    # coalesce results into a single column
    tree_data_with_family <- left_join(updated_tree_data,
      all_family_names,
      by = c("Species" = "query"),
      suffix = c(".old", ".new")
    ) %>%
      mutate(
        family = coalesce(family.new, family.old),
        db = coalesce(db.new, db.old)
      ) %>%
      select(-family.old, -family.new, -db.old, -db.new)
  }

  return(tree_data_with_family)
}





#' 5. Add Genus and Species Columns
#'
#' This function simply splits the binomial name in the Species column in order
#' to create two new columns: genus and specific_epithet. The original species
#' column is also retained.
#'
#' It assumes a well formed binomial name and if it encounters only a single word,
#' will categorize it as genus.
#'
#' @param  updated_tree_data The most recently updated tree data with all
#' corrections
#' @return Same dataframe with new columns for genus and species

add_genus_species_cols <- function(updated_tree_data) {
  tree_data_with_ranks <- updated_tree_data %>%
    # Separate species column into two columns, assuming first word is genus and second
    # is specific epithet
    separate(Species, into = c("genus", "specific_epithet"), sep = " ", remove = FALSE) %>%
    select(Site_ID, Plot_ID, Species, family, genus, specific_epithet, everything())
  return(tree_data_with_ranks)
}



#' 6. Save Family Names
#'
#' Creates the newest file for family names corrections by binding old and new
#' corrections together,
#'
#' @param  existing_family_names The family names file with prior corrections
#' @param  new_family_names The family names file with new corrections from this
#' current run of the script
#' @return Dataframe with all known corrections and record of all names
#' unresolvable by the function.



save_family_names <- function(all_family_names) {
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  species_data_path <- "Species_Data"

  if (!dir.exists(species_data_path)) {
    dir.create(species_data_path, recursive = TRUE)
  }


  file_name <- paste0(species_data_path, "/Family_Names_", date_info, ".csv")
  write.csv(all_family_names, file_name, row.names = FALSE)
  print(paste0("Updated family corrections saved to: ", file_name))
  return(all_family_names)
}


#' 6. Save Updated Tree Data
#'
#' Creates the newest tree data file, which should now have corrected species
#' names (from the prior script) and family names from this script. It titles
#' this file "Final_Tree_Data_YYYY_MM_DD_HHMM.csv"
#'
#' @param  tree_data_with_ranks The most recent tree data instance (with columns
#' for genus and specific_epithet added)
#' @param  raw_data_path variable stored in load_data() return with record of
#' which path to the dataset
#' @return Dataframe of tree data with taxonomic information added

save_updated_tree_data <- function(tree_data_with_ranks, raw_data_path) {
  date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
  write.csv(tree_data_with_ranks, paste0(raw_data_path, "/Final_Tree_Data_", date_info, ".csv"), row.names = FALSE)
  print(paste0("Tree data with family names saved to: ", raw_data_path, "/Final_Tree_Data_", date_info, ".csv"))
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
all_data <- load_data()

# 2. Add Existing Family Names
print("Updating Data with Existing Family Names Information")
updated_tree_data <- add_existing_family_names(all_data$tree_data, all_data$existing_family_names)

# 3. Get Family Names
print("Getting New Family Names. This takes time and requires user input.")
auto_family_names <- get_family_from_GBIF(updated_tree_data, all_data$existing_family_names)
# Below is an alternative option that uses the tax_names function. It is slower, and for fastest
# DB search requires .Renviron to have an API key for the ncbi database
# auto_family_names <- get_family_names(updated_tree_data, all_data$existing_family_names)


# 4. Manually add unresolved family names
print("Beginning interactive loop to manually add family names. This requires user input.")
all_family_names <- manually_find_family_names(new_family_names = auto_family_names, old_family_names = all_data$existing_family_names, tree_data = updated_tree_data)

# 5. Add New Family Names
print("Adding new family name matches to tree data")
tree_data_with_family <- add_new_family_names(updated_tree_data, all_family_names, all_data$existing_family_names)

# 6. Add Genus and Species Columns
print("Splitting species name name into genus and speciific epithet")
tree_data_with_ranks <- add_genus_species_cols(tree_data_with_family)

# 7. Save Family Names
all_family_names <- save_family_names(all_family_names)

# 8. Save Updated Tree Data
print("Saving Final Tree Data")
save_updated_tree_data(tree_data_with_ranks, all_data$raw_data_path)


### END DEV ###
#
# # 4. Add New Family Names
# # print("Adding Newly Found Matches to Tree Data")
# # tree_data_with_family <- add_new_family_names(updated_tree_data, new_family_names, all_data$existing_family_names)
#
# # # 5. Add Genus and Species Columns
# # print("Splitting name into genus and species")
# # tree_data_with_ranks <- add_genus_species_cols(tree_data_with_family)
#
# # # 6. Save Family Names
# # print("Combining and saving family names corrections")
# # all_family_names <- save_family_names(all_data$existing_family_names, new_family_names)
#
# # 7. Save Updated Tree Data
# print("Saving Final Tree Data")
# save_updated_tree_data(tree_data_with_ranks, all_data$raw_data_path)


# save_family_names <- function(existing_family_names, new_family_names) {
#   # new_resolved_names <- new_family_names %>% filter(!is.na(family))
#   all_family_names <- rbind(new_family_names, existing_family_names) %>%
#     distinct()
#   date_info <- format(Sys.time(), "%Y-%m-%d_%H%M")
#   species_data_path <- "Species_Data"
#
#   if (!dir.exists(species_data_path)) {
#     dir.create(species_data_path, recursive = TRUE)
#   }
#
#
#   file_name <- paste0(species_data_path, "/Family_Names_", date_info, ".csv")
#   write.csv(all_family_names, file_name, row.names = FALSE)
#   print(paste0("Updated family corrections saved to: ", file_name))
#   return(all_family_names)
# }


# add_new_family_names <- function(updated_tree_data, new_family_names, existing_family_names) {
#   if (is.null(existing_family_names)) {
#     tree_data_with_family <- left_join(updated_tree_data,
#       select(new_family_names, query, family),
#       by = c("Species" = "query")
#     )
#   } else {
#     tree_data_with_family <- left_join(updated_tree_data,
#       select(new_family_names, query, family),
#       by = c("Species" = "query"),
#       suffix = c(".old", ".new")
#     ) %>%
#       mutate(family = coalesce(family.new, family.old)) %>%
#       select(-family.old, -family.new)
#   }
#
#   return(tree_data_with_family)
# }
