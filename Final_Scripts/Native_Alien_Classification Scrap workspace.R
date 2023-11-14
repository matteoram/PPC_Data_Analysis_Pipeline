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
species_name <- "Albizia lebbeck"
data_gbif <- occ_search(scientificName = species_name, limit = 1000)
data_gbif_latlon <- data_gbif$data %>% select(decimalLatitude, decimalLongitude)
data_gbif_latlon <- na.omit(data_gbif_latlon)
# Convert to sf object
sf_data <- st_as_sf(data_gbif_latlon, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


leaflet(sf_data) %>%
  addTiles() %>%
  addCircleMarkers()
# Optional: visualize



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



unique_sps <- unique(scaled_data$Species)
unique_sps <- unique_sps[!unique_sps %in% c(".", ",", "")]
testing <- check_invasive_status(unique_sps)
