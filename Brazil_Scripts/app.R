library(shiny)
library(dplyr)
library(stringr)
library(tidyverse)
library(DT)

ui <- fluidPage(
  titlePanel("CSV File Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      selectInput("spatialGranularity", "Choose Spatial Granularity:", 
                  choices = c("Country", "Organization_Name", "Site_ID", "Plot_ID"), 
                  selected = "Country",
                  multiple = TRUE),
      
      radioButtons("detailLevel", "Choose Data Grouping:",
                   choices = c("Tree Types" = "treeTypes", 
                               "Size Classes" = "sizeClasses", 
                               "Species Level" = "speciesLevel"),
                   selected = "treeTypes"),
      
      
      actionButton("group_data", "Group Data"),
      downloadButton("downloadData", "Download Grouped Data")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Data", div(style = "overflow-x: scroll; max-height: 300px; width: 100%;", DTOutput("raw_data"))),
        tabPanel("Grouped Data", DTOutput("grouped_data"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  raw_data <- reactiveVal()
  
  observe({
    inFile <- input$file1
    if (!is.null(inFile)) {
      tree_data <- read.csv(inFile$datapath, header = TRUE, check.names = FALSE)
      
      tree_and_size <- tree_data %>%
        mutate(size_class = ifelse(Plot_Type %in% c("30x30", "30x15") & !grepl("census", origin_table, ignore.case = TRUE), ">10cm",
                                   ifelse(Plot_Type == "3x3", "1 - 9.9cm", 
                                          ifelse(grepl("census", origin_table, ignore.case= TRUE),"1 - 9.9cm", "<1cm")))) %>% 
        mutate(size_class = ifelse(Tree_Type == "planted", "small (planted)", size_class))
      
      scale_tree_count <- function(data) {
        data %>%
          mutate(scaled_count = case_when(
            (Plot_Type == "30x30" & grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count,
            (Plot_Type == "30x30" & !grepl("census", origin_table, ignore.case= TRUE)) ~ Tree_Count / (Resample_Main_Plot + 1),
            Plot_Type == "10x10" ~ Tree_Count / (Resample_Main_Plot + 1),
            Plot_Type == "3x3" ~ Tree_Count * 100 / (Resample_3x3_Subplot + 1),
            Plot_Type == "1x1" ~ Tree_Count * 900,
            TRUE ~ Tree_Count
          )) %>% 
          mutate(scaled_count = ifelse(Tree_Type == "planted", Tree_Count, scaled_count)) %>% 
          mutate(Tree_Type_Group = case_when(
            Tree_Type == "planted" ~"Planted",
            Tree_Type %in% c("naturally_regenerating", "Present") ~ "Already Present",
            Tree_Type == "don_t_know" ~ "Unknown"
          ))
        
      }
      
      tree_size_count <- scale_tree_count(tree_and_size)
      raw_data(tree_size_count)
    }
  })
  
  # Observe spatial granularity changes
  observeEvent(input$spatialGranularity, {
    if("Plot_ID" %in% input$spatialGranularity) {
      updateSelectInput(session, "spatialGranularity", selected = c("Country", "Organization_Name", "Site_ID", "Plot_ID"))
    } else if("Site_ID" %in% input$spatialGranularity) {
      updateSelectInput(session, "spatialGranularity", selected = c("Country", "Organization_Name", "Site_ID"))
    } else if("Organization_Name" %in% input$spatialGranularity) {
      updateSelectInput(session, "spatialGranularity", selected = c("Country", "Organization_Name"))
    }
  })
  
  output$raw_data <- renderDT({
    if (is.null(raw_data())) return(NULL)
    raw_data()
  })
  
  output$grouped_data <- renderDT({
    if (is.null(raw_data()) || is.null(input$spatialGranularity)) return(NULL)
    
    df <- raw_data()
    df <- df %>%
      filter(Timeframe == "Y0", !(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2")))
    
    grouping_vars <- c(input$spatialGranularity, "Tree_Type_Group")
    
    if(input$detailLevel == "sizeClasses") {
      grouping_vars <- c(grouping_vars, "size_class")
    } else if(input$detailLevel == "speciesLevel") {
      grouping_vars <- c(grouping_vars, "Species")
    }
    
    result <- df %>%
      
      group_by(across(all_of(grouping_vars))) %>%
      summarise(tree_count = sum(scaled_count, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
      replace_na(list(`Planted` = 0, `Already Present` = 0, `Unknown` = 0))
    
    return(result)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("grouped_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data_to_save <- isolate({
        df <- raw_data()
        
        df <- df %>%
          filter(Timeframe == "Y0", !(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2")))
        
        grouping_vars <- c(input$spatialGranularity, "Tree_Type_Group")
        if(input$detailLevel == "sizeClasses") {
          grouping_vars <- c(grouping_vars, "size_class")
        } else if(input$detailLevel == "speciesLevel") {
          grouping_vars <- c(grouping_vars, "Species")
        }
        
        result <- df %>%
          group_by(across(all_of(grouping_vars))) %>%
          summarise(tree_count = sum(scaled_count, na.rm = TRUE), .groups = 'drop') %>%
          pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
          replace_na(list(`Planted` = 0, `Already Present` = 0, `Unknown` = 0))
        
        result
      })
      
      write.csv(data_to_save, file, row.names = FALSE)
    }
  )
  
  
}


shinyApp(ui, server)
