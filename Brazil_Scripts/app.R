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
      
      checkboxGroupInput("grouping", "Choose Grouping Attributes:", 
                         choices = c("Organization" = "Organization_Name", 
                                     "Site" = "Site_ID", 
                                     "Plot" = "Plot_ID", 
                                     "Size Class" = "size_class", 
                                     "Species" = "Species")),
      
      actionButton("group_data", "Group Data")
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
      
      
      
      
      
      # NOTE: figure out what to do with planted trees
      
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
          mutate(scaled_count = ifelse(Tree_Type == "planted", Tree_Count, scaled_count))
        
      }
      
      tree_size_count <- scale_tree_count(tree_and_size)
      
      
      raw_data(tree_size_count)
    }
  })
  
  output$raw_data <- renderDT({
    if (is.null(raw_data())) return(NULL)
    raw_data()
  }, options = list(paging = FALSE))
  
  output$grouped_data <- renderDT({
    if (is.null(raw_data()) || is.null(input$grouping)) return(NULL)
    
    df <- raw_data()
    
    processed_result <- df %>%
      filter(Timeframe == "Y0", !(Tree_Type == "planted" & !origin_table %in% c("Planted_30x30", "Planted_30x30_2"))) %>%
      mutate(Tree_Type_Group = case_when(
        Tree_Type == "planted" ~ "Planted",
        Tree_Type %in% c("Present", "naturally_regenerating") ~ "Already Present",
        Tree_Type == "don_t_know" ~ "Unknown"
      )) %>%
      group_by(across(all_of(input$grouping)), size_class, Species, Tree_Type_Group) %>%
      summarise(tree_count = sum(scaled_count, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Tree_Type_Group, values_from = tree_count) %>%
      replace_na(list(Planted = 0, `Already Present` = 0))
    
    return(processed_result)
  })
}

shinyApp(ui, server)
