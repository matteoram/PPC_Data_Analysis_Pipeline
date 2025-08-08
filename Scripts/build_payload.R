#' Build KoboToolbox Payload
#'
#' @description This function takes a single record (as a list) and a form ID,
#' then constructs the nested list required by the KoboToolbox API.
#'
#' @param record A list representing one row of data. It must contain
#' an element named `_uuid`.
#'
#' @param form_id The character string ID of the form on KoboToolbox.
#'
#' @return A nested list formatted for the Kobo API payload. If `_uuid` is
#' missing, it returns NULL and issues a warning.

build_payload <- function(record, form_id) {
    
    # The 'record' passed in should be a list
    submission_data <- record

    # Extract the UUID, which is essential for the metadata
    uuid <- submission_data$`_uuid`

    # If there's no UUID, we can't create a valid payload
    if (is.null(uuid)) {
        warning(
            "Record is missing '_uuid'. Cannot create payload.", call. = FALSE)
        return(NULL)
    }

    # Remove the uuid from the main list to avoid duplicating it
    submission_data$`_uuid` <- NULL

    # Construct the final payload list
    payload <- list(
        id = form_id,
        submission = c(
        submission_data, # Appends the form answers
        list(meta = list(instanceID = paste0("uuid:", uuid))) # Appends meta
        )
    )

    return(payload)
}