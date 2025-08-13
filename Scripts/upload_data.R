# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2025-08-08
# Version: 1.0

# Description: This script handles the automated uploading of digitized data 
# from the Colombia paper form to KoboToolbox. The source data is a JSON file 
# containing the digitized data. These records are transformed into 
# API-compliant payloads using a helper function (`create_kobo_payload`) and 
# sent to the KoboToolbox server using POST requests.

# The script's inputs are:
#   - "main_colombia_data.json", a JSON file containing the Colombian data, 
#     harmonized to match the structure of the main Kobo form for seamless 
#     upload.
#     digitized version of the data collected in Colombia using a paper form.
#   - ".Renviron", a hidden file that is safely stored in your local machine to 
#     store your API key.
#   - API Key: a personal token that authenticate the user when submitting a 
#     POST request.
#   - Form ID: a code that uniquely identifies the form that the user wants to
#     upload data for. The user must have access to the form.

# The script's output is:
#   - "upload_log.txt", a text file logging the upload process. It is used to 
#     verify whether all observations were uploaded on the server.

# Lines that can be changed when using it for a new round are marked with the
# text "# CHANGE HERE".

# For more information on this script, please refer to the document "PPC 
# Pipline Supplementary Documentation - Version 2".

# The script uses VS Code minimap's regions.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0. Load packages and data

necessary_packages <- c("httr", "jsonlite", "here", "uuid")
for (pkg in necessary_packages) {
    if (!require(pkg, character.only = TRUE)) {
        cat(pkg, "not found. Installing now...\n")
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

# Load the helper function from your other R script
source(here::here("Scripts/build_payload.R"))

# Define sleep parameter in seconds
sleep_param <- 2 # CHANGE HERE

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1. Configuration

KOBO_API_TOKEN <- Sys.getenv("KOBO_API_TOKEN")
KOBO_API_URL   <- "https://kc.kobotoolbox.org/api/v1/submissions.json"
FORM_ID        <- ""  # CHANGE HERE
DATA_FILEPATH  <- here::here("Colombia_Data/main_colombia_data.json") # CHANGE HERE
LOG_FILEPATH   <- here::here("Colombia_Data/upload_log.txt") # CHANGE HERE

# Check if API Key was found
if (nchar(KOBO_API_TOKEN) == 0) {
    stop(
        "API key not found. Please set the 'KOBO_API_TOKEN' environment variable.\n",
        call. = FALSE
    )
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2. Upload process

# Check if the data file exists
if (!file.exists(DATA_FILEPATH)) stop("Data file not found.")

# Read the JSON data file into a list
records <- jsonlite::fromJSON(DATA_FILEPATH, simplifyDataFrame = FALSE)

# Create a UUID for each record
records_with_uuid <- lapply(records, function(record) {

    # Ensure a new UUID is generated even if _uuid exists and is null
    record$`_uuid` <- uuid::UUIDgenerate()
    return(record)
})

# Initialize elements to track progress
record_count <- length(records_with_uuid)
log_file_con <- file(LOG_FILEPATH, "w")

# Start the upload process by looping through each row of the data frame
cat(paste0("Starting upload of ", record_count, " records...\n"))
writeLines(paste0("Upload started at: ", Sys.time()), log_file_con)
for (i in 1:record_count) {
    
    record_as_list <- records_with_uuid[[i]]

    cat(paste0("\nProcessing record ", i, " of ", record_count, "...\n"))

    # Call function to build the payload
    payload <- build_payload(record_as_list, FORM_ID)

    if (is.null(payload)) {
        cat("Skipping record due to payload creation error.\n")
        writeLines(
            paste0("SKIPPED record ", i, ": Payload creation failed."),
            log_file_con
        )
        next
    }

    # Make the POST request to the KoboToolbox API
    response <- httr::POST(
        url = KOBO_API_URL,
        body = payload,
        encode = "json",
        add_headers(Authorization = paste("Token", KOBO_API_TOKEN))
    )

    # Check the response status
    if (httr::status_code(response) == 201) {

        # If the upload was successful, print a success message
        success_msg <- paste0(
            "Record (Plot: ", record_as_list$Plot[['Plot_ID']], ") uploaded successfully."
        )
        cat(success_msg, "\n")
        writeLines(paste0("SUCCESS: ", success_msg), log_file_con)

    } else {

        # If the upload failed, print an error message with details
        error_msg <- paste0(
            "ERROR uploading record (Plot: ", record_as_list$Plot[['Plot_ID']], "). Status: ",
            httr::status_code(response), "\n   Response: ", 
            httr::content(response, "text", encoding = "UTF-8")
        )
        cat(error_msg, "\n")
        writeLines(error_msg, log_file_con)
    }

    # Pause for 2 second to respect API rate limits
    Sys.sleep(sleep_param)
}

# Finalize the process
writeLines(paste0("\nUpload finished at: ", Sys.time()), log_file_con)
close(log_file_con)
cat("\nUpload process completed.\n")
cat(paste("Log has been saved to:", LOG_FILEPATH, "\n"))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END