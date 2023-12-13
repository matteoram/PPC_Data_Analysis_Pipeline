
# --------------------------------------------------------------------------------
# Project: Priceless Planet Coalition
# Author: Johannes Nelson
# Input: A CSV file with a column titled 'species' that contains the species you 
# would like to review
# Output: A CSV with reviewed species and pertinent invasive species information
# if it exists.

# This script relies on the Global Invasive Species Database (GISD) to flag 
# potential invasive species. It was developed out of a function in a deprecated
# R package called originR and modified to match project needs and to extract more
# relevant information from the GISD database. To interact with the database,
# script relies on the 'rvest' package to perform webscraping that targets 
# information about the native and alien ranges of the plants in question, as 
# well as a summary of its invasiveness, if one exists. This tool is 
# designed as a first step in identifying potentially invasive species, from 
# a list of planting data.

# This is a pared down version of the original script that can be used to preemptively
# check a species list for potential invasiveness. 

# -------------------------------------------------------------------------------


# Load/install necessary packages
necessary_packages <- c("jsonlite", "rvest", "dplyr", "tidyr", "stringr")

for (pkg in necessary_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(pkg, "not found. Installing now...\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#' Check Invasive Status
#'
#' Checks the GISD database for invasive status and relevant information. Web
#' scraping targets the alien and native ranges as well as a summary of 
#' invasiveness (in the case where these elements exist).
#' 
#' In this pared down version, I've bundled all relevant functions and processes
#' into a single function. So, when it is called, it will prompt the user to 
#' select the file with the species list and then produce a file with results.
#' 
#' @return a dataframe with results from the invasives scan

check_invasive_status <- function (...) 
{
  
  
  gbif_GISD_find <- function (species, ...) 
  {
    args <- list(datasetKey = "b351a324-77c4-41c9-a909-f30f77268bc4", 
                 name = species)
    cli <- crul::HttpClient$new(url = "https://api.gbif.org", 
                                opts = list(...))
    out <- cli$get("v1/species", query = args)
    out$raise_for_status()
    fromJSON(out$parse("UTF-8"))$results
  }
  
  
  
    f_path <- file.choose()
    species_file <- read.csv(f_path)
    if (!"species" %in% names(species_file)) {
      print("There is no column names 'species' in this file. Please rename and try again.")
      return(NULL)
    }
    species_to_check <- species_file$species
    results <- list()
    
    for (i in seq_along(species_to_check)) {
      message(paste("Checking", species_to_check[i]))
      out <- gbif_GISD_find(species_to_check[i], ...)
      if (length(out) == 0) {
        results[[i]] <- list(species = species_to_check[i], status = "Not in GISD")
      }
      else {
        doc <- rvest::read_html(paste0("http://www.iucngisd.org/gisd/species.php?sc=", out$taxonID))

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
        
        results[[i]] <- list(species = species_to_check[i], alien_range = alien, 
                             native_range = native, summary = summary)

      }
    }
    names(results) <- species_to_check
    
    
    not_invasive <- list()
    invasive <- list()
    
    for (species in results) {
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
    
    date_info <- format(Sys.time(), "%Y-%m-%d")
    file_name <- paste0("Invasive_Species_Report_", date_info, ".csv")
    write.csv(final_df, file_name, row.names = FALSE)
    print(paste0("Invasive species report saved to: ", file_name))
    
    return(final_df)
    
  }




# Main Scriot: Call the function, store results in scan_results for continued 
# use in R if desired.
scan_results <- check_invasive_status()
