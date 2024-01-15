# --------------------------------------------------------------------------------
# Invasive Species Scanner Shiny App
#
# This app scans a list of species for potential invasiveness using the GISD database.
# Creator: Johannes Nelson https://github.com/johannesnelson
# Credit: I created this app with inspiration from an existing function in an old
# package called 'originr' (https://github.com/ropensci-archive/originr).
# --------------------------------------------------------------------------------

library(jsonlite)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)



# Define primary function that checks invasiveness
check_invasive_status <- function (file_path = NULL, species_list = NULL, ...) 
{
  
  # Define helper function that checks GBIF first
  check_GBIF <- function (species, ...) 
  {
    args <- list(datasetKey = "b351a324-77c4-41c9-a909-f30f77268bc4", 
                 name = species)
    cli <- crul::HttpClient$new(url = "https://api.gbif.org", 
                                opts = list(...))
    out <- cli$get("v1/species", query = args)
    out$raise_for_status()
    fromJSON(out$parse("UTF-8"))$results
  }
  
  

    # Decide whether to use file input or direct input
    if (!is.null(species_list)) {
      species_to_check <- species_list
    } else if (!is.null(file_path)) {
      species_file <- read.csv(file_path)
      if (!"species" %in% names(species_file)) {
        print("There is no column named 'species' in this file. Please rename and try again.")
        return(NULL)
      }
      species_to_check <- species_file$species
    } else {
      print("No species data provided.")
      return(NULL)
    }
    
    total_species <- length(species_to_check)
  results <- list()
  
  # Loop through names and check for invasiveness
  for (i in seq_along(species_to_check)) {
    
    

    message(paste("Checking", species_to_check[i]))
    out <- check_GBIF(species_to_check[i], ...)
    # If no match is in GBIF (which has a record of GISD species), return negative
    # result here.
    if (length(out) == 0) {
      results[[i]] <- list(species = species_to_check[i], status = "Not in GISD")
      spec_status <- "Not in GISD"
    }
    
    # If there is a record, build URL with taxonID and navigate to corresponding
    # GISD page
    else {
      
      doc <- rvest::read_html(paste0("http://www.iucngisd.org/gisd/species.php?sc=", out$taxonID))
      # Find alien range info
      alien <- doc %>%
        html_elements("#ar-col li") %>%
        html_text() %>%
        str_replace_all("\\[\\d+\\]\\s*", "") 
      # Find native range info
      native <- doc %>%
        html_elements("#nr-col li") %>%
        html_text()
      # Include invasiveness summary
      summary <- doc %>% 
        html_elements("#summary") %>% 
        html_text() %>% 
        str_replace_all("[\r\n\t]|Summary", " ") %>%
        str_trim()
      # Store results
      results[[i]] <- list(species = species_to_check[i], alien_range = alien, 
                           native_range = native, summary = summary)
      spec_status <- "Species exists in GISD!"
      
    }
    setProgress(message = paste("Checking", species_to_check[i]), value = i / total_species, detail = spec_status)
  }
  # Organize output for legibility
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
      summary = x$summary,
      status = "Invasive"
    )
  })
  all_invasives_df <- do.call(rbind, invasive_dfs)
  
  final_df <- bind_rows(all_invasives_df, not_invasive_df)
  # final_df <- final_df %>% mutate(status = ifelse(is.na(status), "Invasive", status))
  
  # date_info <- format(Sys.time(), "%Y-%m-%d")
  # file_name <- paste0("Invasive_Species_Report_", date_info, ".csv")
  # write.csv(final_df, file_name, row.names = FALSE)
  # print(paste0("Invasive species report saved to: ", file_name))
  # 
  return(final_df)
  
}


ui <- fluidPage(
  # Add custom CSS to style the app
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Helvetica', sans-serif;
        background-image: url('species_graphic.png');
        background-position: center top;
        background-size: cover; 
        background-repeat: no-repeat;

      }
      .container {
        max-width: 800px;
        margin: 0 auto;
        padding: 20px;
        background-color: rgba(222, 227, 225, 0.97); 
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
        border-radius: 5px;
        margin-top: 20px;
      }

      h1 {
        font-size: 24px;
        margin-bottom: 20px;
      }
      .scrollable-table {
        max-height: 400px;  /* Adjust the height as needed */
        overflow-y: scroll;
        border: 1px solid #ccc;
        border-radius: 5px;
        padding: 10px;
        background-color: rgba(255, 255, 255, .9); 
      }

      .shiny-notification {
              height: 150px;
              width: 800px;
              position: fixed;
              top: 50%;
              left: 50%;
              transform: translate(-50%, -50%);
              font-size: 20px;
              background-color: white;
              opacity: 1;
      }
      .custom-sidebar {
      background-color: rgba(255, 255, 255, .8);
      }
      

    "))
  ),
  # Main container
  div(class = "container",
      titlePanel("Invasive Species Scanner"),
      sidebarLayout(
        sidebarPanel(
          class = "custom-sidebar",
          tags$strong("Run the app by choosing one of the three options below:"),
          tags$br(),
          
          fileInput("file1", "1.) Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          tags$p(paste0("Note: If you are using your own CSV, make sure there is a column, ",
                        "called 'species' that contains scientific names for the species you want to scan."), style = "font-size: 10px;"),
          tags$br(),
          textInput("speciesInput", "2.) Or Enter Species Names (comma-separated)"),
          actionButton("scan", "Scan for Invasive Species"),
          tags$p("Once you've loaded a CSV or entered species names, press the 'scan' button above.", style = "font-size: 10px;"),
          tags$strong("3.) Or click the 'Run Demo' button below to demo the app using pre-loaded data"),
          tags$br(),
          actionButton("runDemo", "Run Demo", title = "Click to run a demonstration using example data"),
          tags$br(),
          tags$br(),

          tags$p("Once scan is finished, you can download the results."),
          downloadButton("downloadResults", "Download Results")
        ),
        mainPanel(
          tags$p("Results will appear here once the app is run."),
          div(class = "scrollable-table",  # Add scrollable div
              tableOutput("results")
          ),
          # tags$img(src = "species_graphic.png"),
          
          # Add this line for status text
          textOutput("statusText")
        )
      )
  )
)
server <- function(input, output) {
  results <- reactiveVal()
  
  observeEvent(input$scan, {
    if (input$speciesInput != "") {
      species_names <- unlist(strsplit(input$speciesInput, ",\\s*"))
      withProgress(message = 'Scanning species for invasiveness', value = 0, {
        results(check_invasive_status(species_list = species_names))
      })
    } else {
      req(input$file1)
      inFile <- input$file1
      withProgress(message = 'Scanning species for invasiveness', value = 0, {
        results(check_invasive_status(file_path = inFile$datapath))
      })
    }
    
    output$results <- renderTable({
      results()
    })
  })
  
  observeEvent(input$runDemo, {
    # Define the path to the example data file
    example_file_path <- "example_data.csv" # Adjust the path if needed
    print("Run Demo clicked")  # Debugging line
    withProgress(message = 'Running demo with example data', value = 0, {
      results(check_invasive_status(file_path = example_file_path))
    })
    output$results <- renderTable({
      results()
    })
  })
  
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste("invasive-species-report-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)



shinyApp(ui = ui, server = server)

      



