library(rgbif)
library(dplyr)
library(sf)
library(leaflet)
library(wikifacts)
library(httr)
library(stringr)
library(crul)
library(countrycode)
library(jsonlite)


load_data <- function() {

  IMP_tree_files <- list.files(path = "IMP_Data", pattern = "IMP_planted_trees", full.names = TRUE)
  latest_tree_file <- IMP_tree_files[order(file.info(IMP_tree_files)$mtime, decreasing = TRUE)[1]]
  IMP_tree_data <- read.csv(latest_tree_file, check.names = FALSE)
  print(paste0("Latest IMP data file: ", latest_tree_file))
  
  IMP_seed_files <- list.files(path = "IMP_Data", pattern = "IMP_planted_seeds", full.names = TRUE)
  latest_seed_file <- IMP_seed_files[order(file.info(IMP_seed_files)$mtime, decreasing = TRUE)[1]]
  IMP_seed_data <- read.csv(latest_seed_file, check.names = FALSE)
  print(paste0("Latest IMP data file: ", latest_seed_file))
  
  dataset_key_file <- list.files(path = "Species_Data", pattern = "GBIF_dataset", full.names = TRUE)
  dataset_key_df <- read.csv(dataset_key_file, check.names = FALSE)
  
  return(list(tree_data = IMP_tree_data, seed_data = IMP_seed_data, dataset_keys = dataset_key_df))
}




get_unique_sp_country_combos <- function(IMP_tree_data, IMP_seed_data){
  
  tree_country_combinations <- IMP_tree_data %>% 
    distinct(project_country, tree_species_names) %>% 
    rename(species = tree_species_names)
  
  seed_country_combinations <- IMP_seed_data %>%
    distinct(project_country, seed_species_names) %>% 
    rename(species = seed_species_names)
  
  all_sp_combos <- rbind(tree_country_combinations, seed_country_combinations)
  
  all_sp_combos <- all_sp_combos %>% distinct() %>% filter(!is.na(species))
  
  all_sp_combos <- all_sp_combos %>% 
    mutate(country_name = countrycode(all_sp_combos$project_country, "iso2c", "country.name")) %>% 
    mutate(country_name = ifelse(country_name == "Congo - Brazzaville", "Democratic Republic of Congo", country_name))
  
  
  return(all_sp_combos)

}




# Function to fetch and filter sentences from Wikipedia
fetch_species_info_wiki <- function(species_name) {
  
  out <- name_backbone(species_name)
  
  if(length(out) == 4){
    print(paste0("No species with the name '", species_name, "' could be found."))
    return(paste0("No species with the name '", species_name, "' could be found."))
  } else {
    base_url <- "https://en.wikipedia.org/w/api.php"
    search_params <- list(
      action = "query",
      list = "search",
      srsearch = out$canonicalName,
      format = "json"
    )
    
    search_response <- GET(base_url, query = search_params)
    search_content <- content(search_response, "parsed")
    
    if (length(search_content$query$search) > 0) {
      page_title <- search_content$query$search[[1]]$title
      
      extract_params <- list(
        action = "query",
        prop = "extracts",
        titles = page_title,
        format = "json",
        explaintext = TRUE
      )
      
      response <- GET(base_url, query = extract_params)
      content <- content(response, "parsed")
      page_content <- content$query$pages[[1]]$extract
      
      if (!is.null(page_content)) {
        
        sentences <- unlist(strsplit(page_content, "(?<=\\.)\\s+", perl = TRUE))
        keywords <- c("native", "endemic", "introduced", "cultivated", "invasive", 
                      "cultivation", "cultivar", "occurring", "occurs", "range", 
                      "ranges", "distribution", "distributed", "originates", "origin",
                      "Australia", "Kenya", "Brazil", "Philippines", "Democratic Republic of Congo", 
                      "Cambodia", "Spain", "India", "Mexico", "Colombia", "France", 
                      "United Arab Emirates", "Portugal", "Malawi", "Madagascar", 
                      "Guatemala", "United States", "Poland")
        filtered_sentences <- sentences[grepl(paste(keywords, collapse = "|"), sentences, ignore.case = TRUE)]
        plant_info <- unique(filtered_sentences)
        plant_info<- paste(plant_info, collapse = " ")
        return(plant_info)
      } else {
        return(paste0("No Wikipedia entry could be found for ", species_name))
      }
    } else {
      return(paste0("No Wikipedia entry could be found for ", species_name))
    }
  }
}




manual_classification <- function(sp_country_combos){
  
  
  process_and_add_data <- function(data, color, fillColor) {
    if (!is.null(data) && nrow(data) > 0 && 
        all(c("decimalLatitude", "decimalLongitude") %in% colnames(data))) {
      latlon_data <- data %>% 
        select(decimalLatitude, decimalLongitude) %>% 
        na.omit()
      sf_data <- st_as_sf(latlon_data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
      leaflet_map <<- leaflet_map %>% 
        addCircleMarkers(data = sf_data, color = color, fillColor = fillColor, fillOpacity = 0.8, radius = 3)
    }
  }
  
  
  sp_country_combos$status <- NA
  for (i in 1:nrow(sp_country_combos)) {
    message("Processing: ", sp_country_combos$species[i], " - ", sp_country_combos$country_name[i], "\n")
    
    
    if (!is.na(sp_country_combos$status_notes[i])) {
      message(paste("This species was included on an a checklist for introduced species in this country. \n",
          "Checklist:", sp_country_combos$datasetTitle[i], "\n"))
    }
    # Fetch and filter sentences from Wikipedia
    sentences <- fetch_species_info_wiki(sp_country_combos$species[i])
    gbif_output <- name_backbone(sp_country_combos$species[i]) # get best match in the GBIF backbone
    
    data_gbif_int <- occ_data(taxonKey =gbif_output$speciesKey, establishmentMeans = "Introduced", limit = 500)
    data_gbif_nat <- occ_data(taxonKey =gbif_output$speciesKey, establishmentMeans = "Native", limit = 500)
    data_gbif_all <- occ_data(taxonKey =gbif_output$speciesKey, limit = 500)
    
    leaflet_map <- leaflet() %>% addTiles()
    
    
    # Process and add all data first (in green)
    process_and_add_data(data_gbif_all$data, "blue", "blue")
    
    # Then, add introduced data (in red)
    process_and_add_data(data_gbif_int$data, "red", "red")
    
    # Finally, add native data (in blue)
    process_and_add_data(data_gbif_nat$data, "green", "green")
    
    # Display the map
    print(leaflet_map)
    
    
    # Display sentences to the user
    if (length(sentences) > 0) {
      cat("Relevant information:\n", paste(sentences, collapse = "\n"), "\n")
    } else {
      cat("No relevant information found.\n")
    }
    
    # Prompt user for decision
    message("Classify as (1) Native, (2) Non-native, (3) Invasive: ")
    decision <- readline()
    if (decision == '1'){
      decision <- "Native"
    }else if (decision == '2') {
        decision <- "Non-native"
    } else if (decision == '3') {
      decision <- "Invasive"
      }
    # Record decision
    sp_country_combos$status[i] <- decision
  }
  return(sp_country_combos)
}
# Optionally, convert results to a data frame or write to a file for further analysis







check_gbif_introduced_checklists <- function (species_name, dataset_keys) {
  results <- list()
  
  for(key in dataset_keys){
    args <- list(datasetKey = key, name = species_name)
    cli <- HttpClient$new(url = "https://api.gbif.org/v1/species")
    
    out <- try(cli$get(query = args), silent = TRUE)
    
    
    out$raise_for_status()
    search_result <- fromJSON(out$parse("UTF-8"))$results
    results[[key]] <- search_result
  }
  
  return(results)
}


scan_for_introduced_species <- function(sp_country_combos, all_dataset_keys, tree_data, seed_data){
  results_list <- list()
  for(i in 1:nrow(sp_country_combos)){
    relevant_keys <- all_dataset_keys %>% 
      filter(country_name == sp_country_combos$country_name[i]) %>% 
      pull(datasetKey)
    
    relevant_key_names <- all_dataset_keys %>% 
      filter(country_name == sp_country_combos$country_name[i]) %>% 
      pull(datasetTitle)
    
    message(paste("Processing", sp_country_combos$species[i], "in", 
                   sp_country_combos$project_country[i], "\n", "Checking following databases:",
                  relevant_key_names, "\n"))
            
    gbif_results <- check_gbif_introduced_checklists(sp_country_combos$species[i], relevant_keys)
    
    all_results <- bind_rows(gbif_results)
    if(nrow(all_results)>0){
      results_list[[i]] <- all_results
    }else{
      results_list[[i]] <- data.frame(species =  sp_country_combos$species[i])
    }
  }
  full_results_df <- bind_rows(results_list)
  full_results_short <- full_results_df %>% filter(!is.na(datasetKey))
  full_results_short <- full_results_short %>% 
    mutate(species = coalesce(species, canonicalName)) %>% 
    select(species, datasetKey, scientificName) %>% 
    left_join(select(all_dataset_keys, datasetTitle, datasetKey, country_name), 
              by = "datasetKey") %>% 
    mutate(status_notes = "potentially introduced") %>% 
    distinct(species, country_name, .keep_all = TRUE)
  
  sp_country_combos <- sp_country_combos %>% left_join(full_results_short, by = c("species", "country_name"))
  
  tree_data_scanned <- left_join(tree_data, sp_country_combos, by = c( "tree_species_names" = "species", "project_country" = "project_country"))
  seed_data_scanned <- left_join(seed_data, sp_country_combos, by = c( "seed_species_names" = "species", "project_country" = "project_country"))
  
  return(list(results = full_results_short, updated_sp_country_combos = sp_country_combos, tree_data_scanned = tree_data_scanned, seed_data_scanned = seed_data_scanned))
}



all_data <- load_data()
sp_country_combos <- get_unique_sp_country_combos(all_data$tree_data, all_data$seed_data)
scan_results <- scan_for_introduced_species(sp_country_combos, all_data$dataset_keys, tree_data = all_data$tree_data, all_data$seed_data)

# This is where to start tomorrow. Implement scan results in classification loop. Decide on how to save or make it so that no
manual_results <- manual_classification(scan_results$updated_sp_country_combos)








# NEXT STEPS:\






# ONE TIME data creation loop:
# Main loop
unique_sps <- unique(scan_results$updated_sp_country_combos$species)

scan_results$updated_sp_country_combos$wiki_info <- NA
for (i in 1:length(unique_sps)) {
  cat("Processing:", unique_sps[i], '\n')
  
  # Fetch and filter sentences from Wikipedia
  sentences <- fetch_species_info_wiki(unique_sps[i])
  
  if (grepl("No Wikipedia entry", sentences[1]) | grepl("No species with", sentences[1])){
    sentences <- "No info found."
  }

  # Record decision
  combined_sentences <- paste(sentences, collapse = " ")
  scan_results$updated_sp_country_combos$wiki_info[i] <- combined_sentences
}






# Define the base URL for the Wikipedia API
base_url <- "https://en.wikipedia.org/w/api.php"

# Set up the parameters for the API request
params <- list(
  action = "query",
  prop = "extracts",
  titles = "Pinus sylvestris",
  format = "json",
  # exintro = TRUE,       # Extract only the intro section
  explaintext = TRUE    # Return plain text instead of HTML
)

# Make the API request
response <- GET(base_url, query = params)

# Parse the response content as JSON
content <- content(response, "parsed")

# Extract the page content from the parsed response
page_content <- content$`query`$`pages`[[1]]$`extract`


# Split the result into sentences
sentences <- unlist(strsplit(page_content, "(?<=\\.)\\s+", perl = TRUE))

# Filter for sentences containing the word "native"
native_sentences <- sentences[grepl("\\bnative\\b", sentences, ignore.case = TRUE)]
introduced_sentences <- sentences[grepl("\\bintroduced\\b", sentences, ignore.case = TRUE)]
cultivated_sentences <- sentences[grepl("\\bcultivated\\b", sentences, ignore.case = TRUE)]
invasive_sentences <- sentences[grepl("\\binvasive\\b", sentences, ignore.case = TRUE)]

all_sentences <- c(native_sentences, introduced_sentences, cultivated_sentences, invasive_sentences)

all_sentences <- c(unique(all_sentences))

# Print the result
print(all_sentences)
