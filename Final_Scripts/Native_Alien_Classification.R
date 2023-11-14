install.packages(c("rgbif", "dplyr", "sf"))
library(rgbif)
library(dplyr)
library(sf)
library(leaflet)
library(wikifacts)
library(httr)


result <- wikifacts::wiki_define("Quercus rubra")


# Split the result into sentences
sentences <- unlist(strsplit(result, "(?<=\\.)\\s+", perl = TRUE))

# Filter for sentences containing the word "native"
native_sentences <- sentences[grepl("\\bnative\\b", sentences, ignore.case = TRUE)]
introduced_sentences <- sentences[grepl("\\bintroduced\\b", sentences, ignore.case = TRUE)]
cultivated_sentences <- sentences[grepl("\\bcultivated\\b", sentences, ignore.case = TRUE)]
invasive_sentences <- sentences[grepl("\\binvasive\\b", sentences, ignore.case = TRUE)]

all_sentences <- c(native_sentences, introduced_sentences, cultivated_sentences, invasive_sentences)

all_sentences <- unique(all_sentences)

# Print the filtered sentences
print(all_sentences)


# Sample data
plants <- data.frame(
  species = c("Harungana madagascariensis", "Eriobotrya japonica"), 
  latitude = c(40.7128, 40.7128), 
  longitude = c(-74.0060, -74.0060)
)

plants$native <- sapply(1:nrow(plants), function(i) {
  species <- plants$species[i]
  lat <- plants$latitude[i]
  lon <- plants$longitude[i]
  
  # Query GBIF
  data_gbif <- occ_search(
    scientificName = species, 
    return = "data", 
    limit = 300 # Adjust this as needed
  )
  
  # Check if the location is within the observed range
  within_range <- any(
    lat > min(data_gbif$data$decimalLatitude, na.rm = TRUE) & 
      lat < max(data_gbif$data$decimalLatitude, na.rm = TRUE) & 
      lon > min(data_gbif$data$decimalLongitude, na.rm = TRUE) & 
      lon < max(data_gbif$data$decimalLongitude, na.rm = TRUE)
  )
  
  return(within_range)
})

print(plants)




# Query GBIF for a species
species_name <- "Phragmites australis"
data_gbif <- occ_search(scientificName = species_name, limit = 1000)
data_gbif_latlon <- data_gbif$data %>% select(decimalLatitude, decimalLongitude)
data_gbif_latlon <- na.omit(data_gbif_latlon)
# Convert to sf object
sf_data <- st_as_sf(data_gbif_latlon, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


leaflet(sf_data) %>%
  addTiles() %>%
  addCircleMarkers()
# Optional: visualize
plot(st_geometry(sf_data))



#####################
# Define the base URL for the Wikipedia API
base_url <- "https://en.wikipedia.org/w/api.php"

# Set up the parameters for the API request
params <- list(
  action = "query",
  prop = "extracts",
  titles = "Phragmites",
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

all_sentences <- unique(all_sentences)

# Print the result
print(all_sentences)

