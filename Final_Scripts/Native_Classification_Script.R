library(rgbif)
library(dplyr)
library(sf)
library(leaflet)
library(wikifacts)
library(httr)



load_data <- function() {
  # Determine which dataset is being corrected and set path to desired folder
  answer <- readline(prompt = cat("Which Dataset are analyzing? \n Enter '1' for the Primary Dataset or '2' for the Brazil Dataset:"))
  if (!answer %in% c("1", "2")) {
    print("Invalid response, please rerun script and be sure to enter either '1' or '2'")
  } else if (answer == "1") {
    raw_data_path <- "Main_Raw_Data"
  } else if (answer == "2") {
    raw_data_path <- "Brazil_Raw_Data"
  }
  
  # Get all files that match the pattern "Tree_Data_Uncorrected" in the "Brazil_Raw_Data" folder
  tree_files <- list.files(path = raw_data_path, pattern = "Final_Tree_Data", full.names = TRUE)
  
  # Sort files by modification date to get the most recent and read this into session
  latest_tree_file <- tree_files[order(file.info(tree_files)$mtime, decreasing = TRUE)[1]]
  tree_data <- read.csv(latest_tree_file, check.names = FALSE)
  
  print(paste0("Latest tree data file: ", latest_tree_file))
  
  return(list(tree_data = tree_data, raw_data_path = raw_data_path))
}




get_unique_sp_country_combos <- function(tree_data){
  sp_country_df <- tree_data %>% 
    distinct(Species, Country)
}




# Function to fetch and filter sentences from Wikipedia
fetch_species_info <- function(species_name) {
  base_url <- "https://en.wikipedia.org/w/api.php"
  params <- list(
    action = "query",
    prop = "extracts",
    titles = species_name,
    format = "json",
    explaintext = TRUE
  )
  
  response <- GET(base_url, query = params)
  content <- content(response, "parsed")
  page_content <- content$query$pages[[1]]$extract
  
  if (!is.null(page_content)){
    sentences <- unlist(strsplit(page_content, "(?<=\\.)\\s+", perl = TRUE))
    keywords <- c("native", "introduced", "cultivated", "invasive", "cultivation", "cultivar")
    filtered_sentences <- sentences[grepl(paste(keywords, collapse = "|"), sentences, ignore.case = TRUE)]
    plant_info <- unique(filtered_sentences)
    
  }else if (is.null(page_content)){
    plant_info <- paste0("No Wikipedia entry could be found for ", species_name)
  }
  return(plant_info)
}



manual_classification <- function(sp_country_combos){
  sp_country_combos$status <- NA
  for (i in 1:nrow(sp_country_combos)) {
    cat("Processing:", sp_country_combos$Species[i], "-", sp_country_combos$Country[i], "\n")
    
    # Fetch and filter sentences from Wikipedia
    sentences <- fetch_species_info(sp_country_combos$Species[i])
    
    data_gbif <- occ_search(scientificName = sp_country_combos$Species[i], limit = 500)
    
    if (!is.null(data_gbif$data)){
      data_gbif_latlon <- data_gbif$data %>% select(decimalLatitude, decimalLongitude)
      data_gbif_latlon <- na.omit(data_gbif_latlon)
    # Convert to sf object
      sf_data <- st_as_sf(data_gbif_latlon, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

      print(
        leaflet(sf_data) %>%
          addTiles() %>%
          addCircleMarkers()
      )
    }
    
    # Display sentences to the user
    if (length(sentences) > 0) {
      cat("Relevant information:\n", paste(sentences, collapse = "\n"), "\n")
    } else {
      cat("No relevant information found.\n")
    }
    
    # Prompt user for decision
    cat("Classify as (1) Native, (2) Non-native, (3) Invasive: ")
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




all_data <- load_data()
sp_country_combos <- get_unique_sp_country_combos(all_data$tree_data)
validated_combos <- manual_classification(sp_country_combos)














# ONE TIME data creation loop:
# Main loop
sp_country_combos$wiki_info <- NA
for (i in 1:nrow(sp_country_combos)) {
  cat("Processing:", sp_country_combos$Species[i], "-", sp_country_combos$Country[i], "\n")
  
  # Fetch and filter sentences from Wikipedia
  sentences <- fetch_species_info(sp_country_combos$Species[i])
  
  if (grepl("No Wikipedia entry", sentences[1])){
    sentences <- "No info found."
  }

  # Record decision
  combined_sentences <- paste(sentences, collapse = " ")
  sp_country_combos$wiki_info[i] <- combined_sentences
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
