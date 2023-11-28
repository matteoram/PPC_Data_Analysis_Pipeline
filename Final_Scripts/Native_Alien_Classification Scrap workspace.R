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
species_name <- "Adenanthera pavonina"
data_gbif <- occ_data(scientificName = species_name, establishmentMeans = "Introduced")
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








# Define parameters
taxon_key <- 1293    # Example Taxon Key

tile_format <- "@1x.png"  # Tile format

# Construct the URL
base_url <- "https://api.gbif.org/v2/map/occurrence/density"
api_url <- sprintf("%s/%d/%d/%d%s?taxonKey=%d", base_url, z, x, y, tile_format, taxon_key)

# You can print api_url to check if it's correctly formatted
print(api_url)



leaflet() %>%
  # addProviderTiles(providers$OpenStreetMap) %>%  # Adding OpenStreetMap as base layer
  addTiles(urlTemplate = api_url, options = tileOptions(opacity = 0.9)) %>%
  setView(lng = 0, lat = 0, zoom = 2)







link <- "http://www.iucngisd.org/gisd/speciesname/Phragmites+australis"
page <- read_html(link)
page %>% 
  html_elements("#summary") %>% 
  html_text() %>% 
  str_replace_all("[\r\n\t]", " ") %>%
  str_trim()




link <- "http://www.iucngisd.org/gisd/speciesname/Phragmites+australis"
page <- read_html(link)
page %>% 
  html_elements("#summary") %>% 
  html_text() %>% 
  str_replace_all("[\r\n\t]", " ") %>%
  str_trim()



check_invasive_status <- function (species_to_check, simplify = FALSE, ...) 
{
  if (length(species_to_check) == 0) {
    print("No new species to check.")
    return(NULL)
  }else{
    
    outlist <- list()
    
    for (i in seq_along(species_to_check)) {
      message(paste("Checking", species_to_check[i]))
      out <- gbif_find(species_to_check[i], ...)
      if (length(out) == 0) {
        outlist[[i]] <- list(species = species_to_check[i], status = "Not in GISD")
      }
      else {
        doc <- rvest::read_html(paste0("http://www.iucngisd.org/gisd/species.php?sc=", out$taxonID))
        if (!simplify) {
          alien <- doc %>%
            html_elements("#ar-col li") %>%
            html_text() %>%
            str_replace_all("\\[\\d+\\]\\s*", "")  # Removes patterns like '[1] '

          
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


##############

function (sp, where, region = c("america", "europe"), ...) 
{
  if (!region %in% c("america", "europe")) {
    stop("region must be one of america or europe", call. = FALSE)
  }
  if (length(sp) > 1) {
    stop("sp should be a single species", call. = FALSE)
  }
  if (region == "america") {
    if (!where %in% c("Continental US", "Alaska", "Canada", 
                      "Caribbean Territories", "Central Pacific Territories", 
                      "Hawaii", "Mexico")) {
      stop("where must be one America region, see help for accepted names", 
           call. = FALSE)
    }
    tsn_ <- taxize::get_tsn(sp, ...)[1]
    if (is.na(tsn_)) {
      Out <- "species not in itis"
    }
    else {
      origin <- taxize::itis_native(tsn = tsn_, ...)
      if (all(is.na(origin$origin))) {
        Out <- "species with no available origin in itis"
      }
      else {
        Out <- origin[which(origin$jurisdictionvalue == 
                              where), "origin"][[1]]
        if (length(Out) == 0) 
          Out <- NA_character_
      }
    }
  }
  if (region == "europe") {
    if (!where %in% c("Albania", "Austria", "Azores", "Belgium", 
                      "Islas_Baleares", "Britain", "Bulgaria", "Corse", 
                      "Kriti", "Czechoslovakia", "Denmark", "Faroer", "Finland", 
                      "France", "Germany", "Greece", "Ireland", "Switzerland", 
                      "Netherlands", "Spain", "Hungary", "Iceland", "Italy", 
                      "Jugoslavia", "Portugal", "Norway", "Poland", "Romania", 
                      "USSR", "Sardegna", "Svalbard", "Sicilia", "Sweden", 
                      "Turkey", "USSR_Northern_Division", "USSR_Baltic_Division", 
                      "USSR_Central_Division", "USSR_South_western", "USSR_Krym", 
                      "USSRSouth_eastern_Division")) {
      stop("where must be one eu country, see help for accepted names", 
           call. = FALSE)
    }
    origin <- flora_europaea(sp)
    if (length(origin) < 5) {
      Out <- "Species not in flora europaea"
    }
    else {
      Out <- "Species not present in your selected region"
      if (where %in% origin$native) {
        Out <- "Native"
      }
      if (where %in% origin$exotic) {
        Out <- "Introduced"
      }
      if (where %in% c(origin$status_doubtful, origin$occurrence_doubtful, 
                       origin$extinct)) {
        Out <- "status or occurrence doubtful or species extinct"
      }
    }
  }
  data.frame(name = sp, origin = Out, stringsAsFactors = FALSE)
}
# <bytecode: 0x0000016ded477400>
#   <environment: namespace:originr>
# ################
# 






### To show map
out <- name_backbone("Albizia lebbeck") # get best match in the GBIF backbone
map_fetch(taxonKey = out$speciesKey)

name_backbone_checklist(c("Eriobotyra japonica")) # look up multiple names





library(rgbif)
library(dplyr)
library(leaflet)
library(sf)

# Query GBIF for a species
out <- name_backbone("Psidium guajava") # get best match in the GBIF backbone
# Fetch data
data_gbif_int <- occ_data(taxonKey =out$speciesKey, establishmentMeans = "Introduced", limit = 1000)
data_gbif_nat <- occ_data(taxonKey =out$speciesKey, establishmentMeans = "Native", limit = 1000)
data_gbif_all <- occ_data(taxonKey =out$speciesKey, limit = 1000)

leaflet_map <- leaflet() %>% addTiles()

# Function to process and add data to the map
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

# Process and add all data first (in green)
process_and_add_data(data_gbif_all$data, "green", "green")

# Then, add introduced data (in red)
process_and_add_data(data_gbif_int$data, "red", "red")

# Finally, add native data (in blue)
process_and_add_data(data_gbif_nat$data, "blue", "blue")

# Display the map
leaflet_map





# Define the base URL for the Wikipedia API
base_url <- "https://en.wikipedia.org/w/api.php"

# Set up the parameters for the API request
params <- list(
  action = "query",
  prop = "extracts",
  titles = out$canonicalName,
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



















leaflet_map_all <- leaflet() %>% addTiles()

# Function to process and add data to the map
process_and_add_data_all <- function(data, color, fillColor) {
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

# Process and add all data
process_and_add_data_all(data_gbif_all$data, "green", "green")

# Display the map
leaflet_map_all

















data_gbif_latlon_int <- data_gbif_int$data %>% select(decimalLatitude, decimalLongitude)
data_gbif_latlon_int <- na.omit(data_gbif_latlon_int)

data_gbif_latlon_nat<- data_gbif_nat$data %>% select(decimalLatitude, decimalLongitude)
data_gbif_latlon_nat <- na.omit(data_gbif_latlon_nat)



# Convert to sf object
sf_data_int <- st_as_sf(data_gbif_latlon_int, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
sf_data_nat <- st_as_sf(data_gbif_latlon_nat, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)




leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = sf_data_int, color = "red", fillColor = "red", fillOpacity = 0.8, radius = 3) %>%
  addCircleMarkers(data = sf_data_nat, color = "blue", fillColor = "blue", fillOpacity = 0.8, radius = 3)


## Check native in EURO and USA

sp_tsn <- get_tsn("Albizia lebbeck")[1]

origin <- itis_native(sp_tsn)




url <- "http://rbg-web2.rbge.org.uk/cgi-bin/nph-readbtree.pl/feout"

cli <- crul::HttpClient$new(url = url)
args <- list(FAMILY_XREF = "", GENUS_XREF = "Phragmites", SPECIES_XREF = "australis", 
             TAXON_NAME_XREF = "", RANK = "")
url_check <- cli$get(query = args)
doc <- rvest::read_html(url_check$parse("UTF-8"), encoding = "UTF-8")
tables <- xml2::xml_find_all(doc, "//table")





############

griis_datasets4 <- dataset_search(query = "Invasive",limit = 1000)$data


inv_dataset_keys$datasetKey



library(crul)
library(jsonlite)

gbif_find <- function (species_name, dataset_keys) {
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

# Example usage
# Ensure that 'inv_dataset_keys$datasetKey' is defined or pass the dataset keys directly

dataset_keys <- unique(inv_dataset_keys$datasetKey)
gbif_find_results <- gbif_find("Albizia lebbeck", dataset_keys)

all_results <- bind_rows(gbif_find_results
                         )
results_with_names <- left_join(select(all_results, canonicalName, datasetKey), inv_dataset_keys, by = "datasetKey")
