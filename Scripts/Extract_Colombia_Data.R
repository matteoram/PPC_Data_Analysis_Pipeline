# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# Project: Priceless Planet Coalition
# Author: Matteo Ramina (raminamatteo@me.com)
# Date: 2025-08-08
# Version: 1.0

# Description: This code forms the data extraction and preprocessing component
# of PPC's data analysis pipeline. This is the script specific to the Colombia 
# Data, which was collected using a paper form and then digitised into an Excel.

# The script's inputs are:
#   - "26022025 BD Monitoreo Musesi Parcelas Permanentes RV Final.xlsx", the 
#     digitized version of the data collected in Colombia using a paper form.
#   - "Forms_mapping.xlsx": a file that links some columns from the Colombian 
#     dataset to the corresponding fields in the main Kobo form.

# The script's output is:
#   - "main_colombia_data.json", a JSON file containing the Colombian data, 
#     harmonized to match the structure of the main Kobo form for seamless 
#     upload.

# For more information on this script, please refer to the document "PPC 
# Pipline Supplementary Documentation - Version 2".

# Lines that can be changed when using it for a new round are marked with the
# text "# CHANGE HERE".

# The script uses VS Code minimap's regions.
# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 0. Load packages and data

# Check for and install missing packages and load them into session
necessary_packages <- c(
    "here", "readxl", "openxlsx", "dplyr", "stringr", "purrr", "tidyr", 
    "jsonlite"
)
for (pkg in necessary_packages) {
    if (!require(pkg, character.only = TRUE)) {
        cat(pkg, "not found. Installing now...\n")
        install.packages(pkg)
        library(pkg, character.only = TRUE)
    }
}

# Specify file names
input_name <- "26022025 BD Monitoreo Musesi Parcelas Permanentes RV Final.xlsx"    # CHANGE HERE
mapping_name <- "Forms_mapping.xlsx"
output_name <- "main_colombia_data.json"    # CHANGE HERE

# Load the main data file
colombia_data <- read_excel(
    file.path("Colombia_Data/", input_name),
    skip = 1
)

# Load sheet mapping paper form columns to kobo fields
paper_to_kobo_map <- read_excel(
    file.path("Colombia_Data/", mapping_name),
    sheet = "paper_to_kobo_map"
)

# Load sheet mapping dataset columns to paper columns
dataset_to_paper_map <- read_excel(
    file.path("Colombia_Data/", mapping_name),
    sheet = "dataset_to_paper_map"
)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 1. Basic data cleaning and data exploration

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1.1. Basic data cleaning

# Create a copy of the dataframe
colombia_data_mod <- colombia_data

# Rename columns in colombia_data_mod
colombia_data_mod <- colombia_data_mod %>%
    rename(
        Plot_Size = `TIPO DE PARCELA`,
        Plot_Type_2 = `TIPO DE CODIFICACIÓN`,
        Plot_ID = `CODIFICACIÓN PARCELA/SUBPARCELA`,
        d1 = `D1\r\n(MM)`,  # CHANGE HERE
        d2 = `D2\r\n(MM)`,  # CHANGE HERE
        d3 = `D3\r\n(MM)`,  # CHANGE HERE
        d4 = `D4\r\n(MM)`,  # CHANGE HERE
        Resampling = `NÚMERO DE REMUESTREOS`,
        Plot_Type = `CATEGORÍA PARCELA (CONTROL/MONITOREO)`,
        Tree_Type = `TIPO DE ÁRBOL2`,
        Tree_Species = `ESPECIE (NOMBRE CIENTÍFICO)`,
        Date = `FECHA \r\nMONITOREO`
    )

# Ensure numerical variables are numeric
numeric_columns <- c(
    "d1", "d2", "d3", "d4", "Resampling", "ALTITUD(M)", 
    "S_LAT", "G_LAT", "M_LAT", "S_LONG", "G_LONG", "M_LONG"
)
colombia_data_mod[numeric_columns] <- lapply(
    colombia_data_mod[numeric_columns], 
    function(x) as.numeric(gsub(",", ".", x))
)

# Create lists for future use
big_plots <- c("30X30", "30x30", "10X10")
small_plots <- c("3X3 de 30x30", "3x3 de 10x10")

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1.2. Fix data mistakes

# Trees >10cm in small plots
colombia_data_mod <- colombia_data_mod %>%
    mutate(
        Plot_ID = if_else(
            Plot_Size %in% small_plots & d1 >= 100 & !is.na(d1),
            gsub("S(?=\\d*$)|S$", "", Plot_ID, perl = TRUE),
            Plot_ID
        ),
        Plot_Size = if_else(
            Plot_Size %in% small_plots & d1 >= 100 & !is.na(d1),
            "30X30",
            Plot_Size
        )
    )

# Trees missing DBH measurements. See assumption in "PPC Pipline Supplementary 
# Documentation - Version 2".
colombia_data_mod <- colombia_data_mod %>%
    mutate(
        d1 = case_when(
        is.na(d1) &
        !is.na(Tree_Species) &
        Tree_Type == "Ya presentes antes del proyecto" &
        Plot_Size %in% big_plots ~ 101,
        is.na(d1) &
        !is.na(Tree_Species) &
        Tree_Type == "Ya presentes antes del proyecto" &
        Plot_Size %in% small_plots ~ 50,
        TRUE ~ d1
        )
    )

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1.3. Preliminary checks

# Confirm number of plots
cat("\nNumber of distinct Plot_IDs for PARCELA: ",
    colombia_data_mod %>%
        filter(Plot_Type_2 == "PARCELA") %>%
        distinct(Plot_ID) %>%
        nrow(),
    "\n"
)

# Confirm raw number of trees (not extrapolated)
cat("Number of non-empty Tree_Species values: ",
    sum(
        !is.na(colombia_data_mod$Tree_Species) &
        colombia_data_mod$Tree_Species != ""
        ),
    "\n"
)

# Confirm that sub-plots always end in S or Sx (where x is a digit)
subparcelas <- colombia_data_mod[
    colombia_data_mod$Plot_Type_2 == "SUBPARCELA", ]
invalid_rows <- subparcelas[
    !grepl("S(\\d+)?$", subparcelas$Plot_ID), ]
if (nrow(invalid_rows) == 0) {
    cat("\nAll SUBPARCELA entries END with 'S' or 'Sx'.\n")
} else {
    cat("\nSome SUBPARCELA entries DO NOT end with 'S' or 'Sx':\n")
    print(invalid_rows)
}

# Confirm that plots do not end in S or Sx (where x is a digit)
parcelas <- colombia_data_mod[
    colombia_data_mod$Plot_Type_2 == "PARCELA", ]
invalid_rows <- parcelas[
    grepl("S(\\d+)?$", parcelas$Plot_ID), ]
if (nrow(invalid_rows) == 0) {
    cat("\nAll PARCELA entries DO NOT end with 'S' or 'Sx'.\n\n")
} else {
    cat("\nSome PARCELA entries END with 'S' or 'Sx':\n\n")
    print(invalid_rows)
}

# Remove trailing 'S' or 'Sx' followed by digits in the specified column
colombia_data_mod$Plot_ID_2 <- sub(
    "S(?=\\d?$)", "",
    colombia_data_mod$Plot_ID, perl = TRUE
)

# Confirm that all plots have a subplot and viceversa
presence_flags <- colombia_data_mod %>%
    group_by(Plot_ID_2) %>%
    summarize(
        has_parcela = any(Plot_Type_2 == "PARCELA"),
        has_subparcela = any(Plot_Type_2 == "SUBPARCELA"),
        .groups = "drop"
    )
colombia_data_mod <- colombia_data_mod %>%
    left_join(presence_flags, by = "Plot_ID_2")
invalid_rows <- colombia_data_mod %>%
    filter(!(has_parcela & has_subparcela))
if (nrow(invalid_rows) == 0) {
    cat("All plots have at least one PARCELA and one SUBPARCELA.\n\n")
} else {
    cat("Some plots are missing either PARCELA or SUBPARCELA:\n\n")
    print(invalid_rows)
}
colombia_data_mod <- colombia_data_mod %>%
    select(-has_parcela, -has_subparcela)

# Check that the first DBH is higher than subsequent ones
dbh_values <- subset(colombia_data_mod,
  (d1 < d2) |
  (d1 < d3) |
  (d1 < d4)
)
dbh_values <- dbh_values[
    , c("Plot_ID_2", "d1", "d2", "d3", "d4")
]
if (nrow(dbh_values) > 0) {
    print("\nObservations that do NOT satisfy the condition (D1 > D2, D1 > D3, D1 > D4):")
    print(dbh_values)
} else {
    print("\nAll observations satisfy the condition (D1 > D2, D1 > D3, D1 > D4). No violating rows found.")
}

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 2. Create "Tree Monitoring" sheet

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2.1. Extract and rename columns that are clearly mapped

# Update the paper_form_column names due to previous renaming
paper_to_kobo_map <- paper_to_kobo_map %>%
    mutate(
        paper_form_column = recode(
        paper_form_column,
        "FECHA \r\nMONITOREO" = "Date",
        "CODIFICACIÓN PARCELA/SUBPARCELA" = "Plot_ID",
        "CATEGORÍA PARCELA (CONTROL/MONITOREO)" = "Plot_Type",
        "TIPO DE PARCELA" = "Plot_Size",
        "NÚMERO DE REMUESTREOS" = "Resampling"
        )
    )

# Keep only those columns in colombia_data_mod (if they exist)
tree_monitoring_sheet_raw <- colombia_data_mod[,
    colnames(colombia_data_mod) %in% (paper_to_kobo_map %>%
    filter(include == 1, tree_monitoring_sheet == 1) %>%
    pull(paper_form_column))
]

# Filter column names that are clearly associated with field names
valid_mapping <- paper_to_kobo_map[
    !grepl("^(\\?|.*na.*|.*`)", paper_to_kobo_map$kobo_form_field_name,
    ignore.case = TRUE),
]

# Create an old-new rename name map
rename_map <- setNames(
    valid_mapping$kobo_form_field_name,
    valid_mapping$paper_form_column
)

# Find which columns are actually present in tree_monitoring_sheet_raw
columns_to_rename <- intersect(
    names(rename_map), colnames(tree_monitoring_sheet_raw)
)

# Rename only those columns
colnames(tree_monitoring_sheet_raw)[
    match(columns_to_rename, colnames(tree_monitoring_sheet_raw))
    ] <- rename_map[columns_to_rename]

# Aggregate data to obtain info at the plot-level by uniquely identifying each
# plot according to Plot_ID and Enter_a_date
tree_monitoring_sheet <- tree_monitoring_sheet_raw %>%
    group_by(Plot_ID, Enter_a_date) %>%
    slice(1) %>%
    ungroup()

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2.2. Calculate variables that are not clearly mapped

# While latitude and longitude are not fields included in the Kobo form,
# creating them will be useful for applying accurate geo-referencing to the
# fields.
# Latitude
tree_monitoring_sheet$latitude <- (
    tree_monitoring_sheet$G_LAT + (tree_monitoring_sheet$M_LAT/60) + 
    (tree_monitoring_sheet$S_LAT/3600)
)
# Longitude
tree_monitoring_sheet$longitude <- -1*(
    tree_monitoring_sheet$G_LONG + (tree_monitoring_sheet$M_LONG/60) + 
    (tree_monitoring_sheet$S_LONG/3600)
)

# Country
tree_monitoring_sheet$Country <- "Colombia"

# Organization_Name
tree_monitoring_sheet$Organization_Name <- "CI Colombia"

# Timeframe
tree_monitoring_sheet$Timeframe <- "Y0 (baseline)"  # CHANGE HERE

# SiteSize. 
tree_monitoring_sheet$SiteSize <- ifelse(
    tree_monitoring_sheet$Plot_Type == "CONTROL",
    NA,
    "Yes"
)

# ControlSize
tree_monitoring_sheet$ControlSize <- ifelse(
    tree_monitoring_sheet$Plot_Type == "MONITOREO",
    NA,
    ifelse(
        (tree_monitoring_sheet$Plot_Size == "10X10" |
        tree_monitoring_sheet$Plot_Size == "3x3 de 10x10"),
    "Yes",
    "No"
    )
)

# Plot_Permanence
tree_monitoring_sheet$Plot_Permanence <- ifelse(
    tree_monitoring_sheet$Plot_Type == "CONTROL",
    NA,
    "Permanent"
)

# Resampling1
tree_monitoring_sheet$Resampling1 <- ifelse(
    tree_monitoring_sheet$Plot_Type != "CONTROL" & 
    tree_monitoring_sheet$SiteSize != "No" &
    tree_monitoring_sheet$Plot_Size %in% big_plots,
    tree_monitoring_sheet$Resampling,
    NA_real_
)

# TreesPresent. This is calculated based on the "D1 (MM)" column in 
# the Colombia Data: If any tree in the plot has a DBH greater than 10cm (or 
# 100mm), then the plot is considered to have trees present.
# First, I create a logical flag in colombia_data_mod for trees >10cm (>100mm)
colombia_data_mod$has_tree_over_10cm <- colombia_data_mod[["d1"]] >= 100
# I now summarise by plot: does each plot have any tree over 10cm (>100mm)?
plot_has_tree_over_10cm <- colombia_data_mod %>%
    group_by(Plot_ID) %>%
    summarise(has_tree = any(has_tree_over_10cm, na.rm = TRUE)) %>%
    rename(Plot_ID = Plot_ID)
# Merge with tree_monitoring_sheet
tree_monitoring_sheet <- tree_monitoring_sheet %>%
    left_join(plot_has_tree_over_10cm, by = "Plot_ID")
# Create the new variable based on the conditions below
tree_monitoring_sheet$TreesPresent <- ifelse(
    tree_monitoring_sheet$Resampling1 == 2,
    ifelse(tree_monitoring_sheet$has_tree, "Yes", "No"),
    NA
)

# Resampling1. Fix issue with Resampling1 being <2 and no trees with DBH≥10
tree_monitoring_sheet$Resampling1 <- ifelse(    # CHANGE HERE
    tree_monitoring_sheet$Resampling1 < 2 & 
    tree_monitoring_sheet$has_tree == FALSE,
    2, tree_monitoring_sheet$Resampling1
)

# TreesPresent. Fix issue with Resampling1 being <2 and no trees with DBH≥10
tree_monitoring_sheet$TreesPresent <- ifelse(    # CHANGE HERE
    tree_monitoring_sheet$Resampling1 == 2 & 
    tree_monitoring_sheet$has_tree == FALSE,
    "No", tree_monitoring_sheet$TreesPresent
)

# Remove helper column
tree_monitoring_sheet$has_tree <- NULL

# Coordinate_System_Used
tree_monitoring_sheet$Coordinate_System_Used <- "Degrees-Minutes-Seconds (DMS)"

# Northeast_Corner_of_30m_x_30m_Plot. GPS coordinates in Colombian data 
# correspond to centroid also for 30x30 plots. Because there is no corresponding
# field in the Kobo form, I will store the centroid coordinates in northeast
tree_monitoring_sheet$Northeast_Corner_of_30m_x_30m_Plot <- ifelse(
    (tree_monitoring_sheet$SiteSize != "No" &
    tree_monitoring_sheet$Plot_Size %in% big_plots) | 
    (tree_monitoring_sheet$ControlSize != "Yes" &
    tree_monitoring_sheet$Plot_Size == "30x30"),
    paste(tree_monitoring_sheet$latitude,
        tree_monitoring_sheet$longitude,
        tree_monitoring_sheet$`ALTITUD(M)`,
        sep = " "),
    NA
)

# Northeast_Corner_of_10m_x_10m_Plot. GPS coordinates in Colombian data 
# correspond to centroid also for 10x10 plots. Because there is no corresponding
# field in the Kobo form, I will store the centroid coordinates in northeast
tree_monitoring_sheet$Northeast_Corner_of_10m_x_10m_Plot <- ifelse(
    tree_monitoring_sheet$ControlSize == "Yes" &
    tree_monitoring_sheet$Plot_Size == "10X10",
    paste(tree_monitoring_sheet$latitude,
        tree_monitoring_sheet$longitude,
        tree_monitoring_sheet$`ALTITUD(M)`,
        sep = " "),
    NA
)

# Centroid_of_3m_x_3m_Subplot1
tree_monitoring_sheet$Centroid_of_3m_x_3m_Subplot1 <- ifelse(
    tree_monitoring_sheet$ControlSize == "Yes" &
    tree_monitoring_sheet$Plot_Size == "3x3 de 10x10",
    paste(tree_monitoring_sheet$latitude,
        tree_monitoring_sheet$longitude,
        tree_monitoring_sheet$`ALTITUD(M)`,
        sep = " "),
    NA
)

# Resampling2
tree_monitoring_sheet$Resampling2 <- ifelse(
    (tree_monitoring_sheet$ControlSize != "Yes" &
    tree_monitoring_sheet$Plot_Size == "3X3 de 30x30") |
    (tree_monitoring_sheet$SiteSize != "No" &
    tree_monitoring_sheet$Plot_Size %in% small_plots),
    tree_monitoring_sheet$Resampling,
    NA_real_
)

# LittleTreesPresent. This is calculated based on the "D1 (MM)" column in 
# the Colombia Data: If any tree in the plot has a DBH between 1cm and 9.9cm
# (or between 10mm and 99mm), then the plot is considered to have little trees 
# present. This is not applicable to control plots, that do not have captured
# the resampling of the plot.
# Procedure is the same as for TreesPresent.
colombia_data_mod$has_tree_1_to_9_9cm <- (
    colombia_data_mod$d1 >= 10 &
    colombia_data_mod$d1 < 100
)
plot_has_tree_1_to_9_9cm <- colombia_data_mod %>%
    group_by(Plot_ID) %>%
    summarise(has_tree = any(has_tree_1_to_9_9cm, na.rm = TRUE)) %>%
    rename(Plot_ID = Plot_ID)
tree_monitoring_sheet <- tree_monitoring_sheet %>%
    left_join(plot_has_tree_1_to_9_9cm, by = "Plot_ID")
tree_monitoring_sheet$LittleTreesPresent <- ifelse(
    tree_monitoring_sheet$Resampling2 == 2,
    ifelse(tree_monitoring_sheet$has_tree, "Yes", "No"),
    NA
)

# Resampling2 Fix missing Resampling2 info for control plots
tree_monitoring_sheet$Resampling2 <- ifelse(    # CHANGE HERE
    is.na(tree_monitoring_sheet$Resampling2) &
    tree_monitoring_sheet$Plot_Type == "CONTROL" &
    tree_monitoring_sheet$ControlSize != "Yes" &
    tree_monitoring_sheet$Plot_Size %in% small_plots,
    0,
    tree_monitoring_sheet$Resampling2
)

# Resampling2. Fix issue with Resampling2 being <2 and no trees with 1≤DBH<9.9
tree_monitoring_sheet$Resampling2 <- ifelse(    # CHANGE HERE
    tree_monitoring_sheet$Resampling2 < 2 & 
    tree_monitoring_sheet$has_tree == FALSE &
     tree_monitoring_sheet$Plot_Type != "CONTROL",
    2, tree_monitoring_sheet$Resampling2
)

# LittleTreesPresent. Fix issue with LittleTreesPresent being <2 and no trees
# with 1≤DBH<9.9
tree_monitoring_sheet$LittleTreesPresent <- ifelse(    # CHANGE HERE
    tree_monitoring_sheet$Resampling2 == 2 & 
    tree_monitoring_sheet$has_tree == FALSE & 
    tree_monitoring_sheet$Plot_Type != "CONTROL",
    "No", tree_monitoring_sheet$LittleTreesPresent
)

# Remove helper column
tree_monitoring_sheet$has_tree <- NULL

# Centroid_of_3m_x_3m_Subplot2. Select relevant plots by elimination
tree_monitoring_sheet$Centroid_of_3m_x_3m_Subplot2 <- ifelse(
    is.na(tree_monitoring_sheet$Centroid_of_3m_x_3m_Subplot1) &
    tree_monitoring_sheet$Plot_Size %in% small_plots,
    paste(tree_monitoring_sheet$latitude,
        tree_monitoring_sheet$longitude,
        tree_monitoring_sheet$`ALTITUD(M)`,
        sep = " "),
    NA
)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2.3. Drop unnecesary columns and add functional columns

# Drop columns that are not needed except for Plot_Size
cols_to_drop <- c(
  "Resampling", "S_LAT", "G_LAT", "M_LAT", "latitude", "S_LONG", "G_LONG", 
  "M_LONG", "longitude", "ALTITUD(M)", "Latitud", "Longitud"
)
tree_monitoring_sheet <- tree_monitoring_sheet[,
!(names(tree_monitoring_sheet) %in% cols_to_drop)]

# Add functional columns
tree_monitoring_sheet$`__version__` <- "Colombian paper survey"
tree_monitoring_sheet$`_uuid` <- ""

# 2.4. Final cosmetic changes

# Translate Plot_Type values
tree_monitoring_sheet$Plot_Type <- recode(
    tree_monitoring_sheet$Plot_Type,
    "CONTROL" = "Control",
    "MONITOREO" = "Restoration",
    .default = tree_monitoring_sheet$Plot_Type
)

# Remove NA in PlantingPattern
tree_monitoring_sheet$PlantingPattern <- na_if(
    tree_monitoring_sheet$PlantingPattern,
    "No aplica"
)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 3. Create repeat group sheets

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.1. Generate data for repeat groups

# Filter only tiny trees below 1cm (10mm)
trees_below_1 <- colombia_data_mod %>%
    filter(
        d1 < 10
    )

# Filter only small trees (those with DBH 1-9.9 cm, or 10-99.9mm) and control
# nested plots (as assumed that they all have small trees)
trees_between_1_9 <- colombia_data_mod %>%
    filter(
        (d1 >= 10 & d1 < 100) |
        (Tree_Type == "Ya presentes antes del proyecto" &   # CHANGE HERE
        Plot_Size %in% small_plots)
    )

# Filter only tall trees (those with DBH >= 10cm, or >= 100mm) and control
# plots (as assumed that they all have tall trees)
trees_above_10 <- colombia_data_mod %>%
    filter(
        d1 >= 100 |
        (Tree_Type == "Ya presentes antes del proyecto" &   # CHANGE HERE
        Plot_Size %in% big_plots)
    )

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.2. "PlantedTrees3" sheet (Planted_30x30)

# Select relevant plots via regular logic: 30x30 plots with <1 planted trees
relevant_plots <- colombia_data_mod %>%
    filter(
        Plot_Type == "MONITOREO" &
        Plot_Size %in% big_plots &
        Tree_Type == "Plantados por el proyecto"
    ) %>%
    pull(Plot_ID)

# Aggregate tree values
PlantedTrees3_1 <- trees_below_1 %>%    # CHANGE HERE
    filter(
        Plot_ID %in% relevant_plots
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type
    ) %>%
    summarise(
        count = n(),
        Plot_Size = first(Plot_Size),
        .groups = "drop"
    ) %>%
    mutate(
        count = ifelse(
            Plot_Size == "10X10",
            count * 9,
            count
        )
    )

# Select census plots (monitoring only). Fix issue with Resampling1 being <2
# and no trees with DBH≥10
relevant_plots_2 <- tree_monitoring_sheet %>%    # CHANGE HERE
    filter(
        LittleTreesPresent == "No" & # LittleTreesPresent is for 3x3 plots
        Plot_Type == "Restoration"
    ) %>%
    pull(Plot_ID) # Need now to remove 'S' or 'Sx' to consider 30x30 plots
relevant_plots_2 <- gsub("S(?=\\d*$)|S$", "", relevant_plots_2, perl = TRUE)
relevant_plots_2 <- tree_monitoring_sheet %>%
    filter(
        Plot_ID %in% relevant_plots_2
    ) %>%
    pull(Plot_ID)
relevant_plots_2 <- unique(relevant_plots_2) # Obtain corresponding 30x30 plots

# Select final relevant plots
relevant_plots_final <- setdiff(relevant_plots, relevant_plots_2) # CHANGE HERE

# Aggregate tree values. Fix issue with Resampling1 being <2 and no trees with 
# DBH≥10
PlantedTrees3_2 <- trees_between_1_9 %>%    # CHANGE HERE
    filter(
        Plot_ID %in% relevant_plots_final &
        Tree_Type == "Plantados por el proyecto"
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type
    ) %>%
    summarise(
        count = n(),
        Plot_Size = first(Plot_Size),
        .groups = "drop"
    ) %>%
    mutate(
        count = ifelse(
            Plot_Size == "10X10",
            count * 9,
            count
        )
    ) %>%    
    mutate(
        Note3 = paste0("Contains ", count, " tree(s) between 1cm and 9.9cm.")
    )

# Combine the two dataframes
PlantedTrees3_3 <- bind_rows(PlantedTrees3_1, PlantedTrees3_2) %>%    # CHANGE HERE
    # Regroup by all the non-count columns
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type,
        Plot_Size
    ) %>%
    summarise(
        # Sum the counts for identical rows
        count = sum(count),
        # Keep the non-NA value for Note3
        Note3 = first(na.omit(Note3)),
        .groups = "drop"
    )

# Select <1 trees in 3x3 plots (monitoring only). Fix issue 3x3 plots and DBH
# containing trees <1
relevant_plots_3 <- colombia_data_mod %>%    # CHANGE HERE
    filter(
        Plot_Type == "MONITOREO" &
        Plot_Size %in% small_plots &
        Tree_Type == "Plantados por el proyecto"
    ) %>%
    pull(Plot_ID)

# Aggregate tree values for 3x3 plots
PlantedTrees3_3x3 <- trees_below_1 %>%    # CHANGE HERE
    filter(
        Plot_ID %in% relevant_plots_3
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type
    ) %>%
    summarise(
        count = n(),
        Plot_Size = first(Plot_Size),
        .groups = "drop"
    ) %>%
    mutate(
        Plot_ID = gsub("S(?=\\d*$)|S$",
        "", Plot_ID, perl = TRUE)
    )

PlantedTrees3_final <- bind_rows(PlantedTrees3_3, PlantedTrees3_3x3) %>% # CHANGE HERE
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type,
        Plot_Size
    ) %>%
    summarise(
        count = sum(count),
        Note3 = first(na.omit(Note3)),
        .groups = "drop"
    )

# Clean data
PlantedTrees3 <- PlantedTrees3_final %>%    # CHANGE HERE
    # Rename columns
    rename(
        Tree_Species_Seedlings = Tree_Species,
        Numer_of_This_Species3 = count,
        Enter_a_date = Date
    ) %>%
    # Flag 10x10 plots
    mutate(
        Note3 = ifelse(
            Plot_Size == "10X10",
            ifelse(
                is.na(Note3),
                "Extrapolated from 10x10 plot.",
                paste0(Note3, "; Extrapolated from 10x10 plot.")
            ),
            Note3
        )
    ) %>%
    # Flag 3x3 plots
    mutate(
        Note3 = ifelse(
            Plot_Size %in% small_plots,
            ifelse(
                is.na(Note3),
                paste0(
                    "Contains ",
                    Numer_of_This_Species3,
                    " tree(s) below 1cm from 3x3 plots."
                ),
                paste0(
                    Note3, "; Contains ",
                    Numer_of_This_Species3,
                    " trees below 1cm from 3x3 plots."
                )
            ),
            Note3
        )
    ) %>%
    # Remove unwanted column
    select(-Tree_Type)

# Final aggregation by grouping nested plots with regular plots
PlantedTrees3 <- PlantedTrees3 %>%
    group_by(Enter_a_date, Plot_ID, Tree_Species_Seedlings) %>%
    summarise(
        Numer_of_This_Species3 = sum(Numer_of_This_Species3, na.rm = TRUE),
        Plot_Size = first(Plot_Size),
        Note3 = paste(na.omit(unique(Note3)), collapse = "; "),
        .groups = "drop"
    )

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.3. "group_an2yk58" ("Normal_30x30_2")

# Select relevant plots
relevant_plots <- tree_monitoring_sheet %>%
    filter(
        (SiteSize == "Yes" &
        Plot_Size %in% big_plots &
        (Resampling1 < 2 | (Resampling1 == 2 & TreesPresent == "Yes"))) |
        (ControlSize == "No" & Plot_Size == "30X30")
    ) %>%
    pull(Plot_ID)

# Aggregate tree values
group_an2yk58 <- trees_above_10 %>%
    filter(
        Plot_ID %in% relevant_plots
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type,
    ) %>%
    summarise(
        count = n(),
        Plot_Size = first(Plot_Size),
        .groups = "drop"
    )

group_an2yk58 <- group_an2yk58 %>%

    # Rename columns
    rename(
        Tree_Species2 = Tree_Species,
        Tree_Type2 = Tree_Type,
        Number_of_Trees_of_this_Species2 = count,
        Enter_a_date = Date
    ) %>%

    # Translate values and add Note111
    mutate(Tree_Type2 = case_when(
        Tree_Type2 == "Plantados por el proyecto" ~ "Planted by your project",
        Tree_Type2 == "Ya presentes antes del proyecto" ~ "Already present prior to the project"
    ),
    Note111 = ifelse(
        Plot_Size == "10X10",
        "Extrapolated from 10x10 plot.",
        NA
    ))

# Extrapolate 10x10 trees to 30x30
group_an2yk58$Number_of_Trees_of_this_Species2 <- ifelse(
    group_an2yk58$Plot_Size == "10X10",
    group_an2yk58$Number_of_Trees_of_this_Species2 * 9,
    group_an2yk58$Number_of_Trees_of_this_Species2
)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.4. "group_ka7vj63" ("Nested_3x3_within_30x30_2")

# Select relevant plots
relevant_plots <- tree_monitoring_sheet %>%
    filter(
        (Resampling2 < 2 | (Resampling2 == 2 & LittleTreesPresent == "Yes")) &
        ((Plot_Size %in% small_plots & Plot_Type == "Restoration") |
        (Plot_Size == "3X3 de 30x30" & Plot_Type == "Control"))
    ) %>%
    pull(Plot_ID)

# Aggregate tree values
group_ka7vj63 <- trees_between_1_9 %>%
    filter(
        Plot_ID %in% relevant_plots
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type,
    ) %>%
    summarise(count = n(), .groups = "drop")

group_ka7vj63 <- group_ka7vj63 %>%

    # Rename columns
    rename(
        Tree_Species_0012 = Tree_Species,
        Tree_Type_0012 = Tree_Type,
        Number_of_Trees_of_this_Species_0012 = count,
        Enter_a_date = Date
    ) %>%

    # Translate values
    mutate(Tree_Type_0012 = case_when(
        Tree_Type_0012 == "Plantados por el proyecto" ~ "Planted by your project",
        Tree_Type_0012 == "Ya presentes antes del proyecto" ~ "Already present prior to the project"
    ))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.5. "group_oq8nt56" (Control_10x10_2)

# Select relevant plots
relevant_plots <- tree_monitoring_sheet %>%
    filter(
        ControlSize == "Yes" & Plot_Size == "10X10"
    ) %>%
    pull(Plot_ID)

# Aggregate tree values
group_oq8nt56 <- trees_above_10 %>%
    filter(
        Plot_ID %in% relevant_plots
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type
    ) %>%
    summarise(count = n(), .groups = "drop")

group_oq8nt56 <- group_oq8nt56 %>%

    # Rename columns
    rename(
        Tree_Species1 = Tree_Species,
        Tree_Type1 = Tree_Type,
        Number_of_Trees_of_this_Species1 = count,
        Enter_a_date = Date
    ) %>%

    # Translate values
    mutate(Tree_Type1 = case_when(
        Tree_Type1 == "Plantados por el proyecto" ~ "Planted by your project",
        Tree_Type1 == "Ya presentes antes del proyecto" ~ "Already present prior to the project"
    ))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.6. "group_ql6qg69" (Census_30x30)

# Select relevant plots
relevant_plots <- tree_monitoring_sheet %>%
    filter(
        LittleTreesPresent == "No" # LittleTreesPresent is for 3x3 plots
    ) %>%
    pull(Plot_ID) # Need to remove 'S' or 'Sx' to consider 30x30 plots
relevant_plots <- gsub("S(?=\\d*$)|S$", "", relevant_plots, perl = TRUE)
relevant_plots <- colombia_data_mod %>%
    filter(
        Plot_ID %in% relevant_plots
    ) %>%
    pull(Plot_ID)
relevant_plots <- unique(relevant_plots) # Obtain corresponding 30x30 plots

# Aggregate tree values
group_ql6qg69 <- trees_between_1_9 %>%
    filter(
        Plot_ID %in% relevant_plots
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type,
    ) %>%
    summarise(
        count = n(),
        Plot_Size = first(Plot_Size),
        .groups = "drop"
    )

group_ql6qg69 <- group_ql6qg69 %>%

    # Rename columns
    rename(
        Tree_Species_0014 = Tree_Species,
        Tree_Type_0014 = Tree_Type,
        Number_of_Trees_of_this_Species_0014 = count,
        Enter_a_date = Date
    ) %>%

    # Flag 10x10 plots
    mutate(
        Note24 = ifelse(Plot_Size == "10X10",
    "Extracted from 10x10 plot.", NA)
    ) %>%

    # Translate values
    mutate(Tree_Type_0014 = case_when(
        Tree_Type_0014 == "Plantados por el proyecto" ~ "Planted by your project",
        Tree_Type_0014 == "Ya presentes antes del proyecto" ~ "Already present prior to the project"
    ))

# Extrapolate 10x10 trees to 30x30
group_ql6qg69$Number_of_Trees_of_this_Species_0014 <- ifelse(
    group_ql6qg69$Plot_Size == "10X10",
    group_ql6qg69$Number_of_Trees_of_this_Species_0014 * 9,
    group_ql6qg69$Number_of_Trees_of_this_Species_0014
)

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.7. "group_wj4lz16" (Nested_3x3_within_10x10_Control_2)

# Select relevant plots
relevant_plots <- tree_monitoring_sheet %>%
    filter(
        ControlSize == "Yes" & Plot_Size == "3x3 de 10x10"
    ) %>%
    pull(Plot_ID)

# Aggregate tree values
group_wj4lz16 <- trees_between_1_9 %>%
    filter(
        Plot_ID %in% relevant_plots
    ) %>%
    group_by(
        Date,
        Plot_ID,
        Tree_Species,
        Tree_Type,
    ) %>%
    summarise(count = n(), .groups = "drop")

group_wj4lz16 <- group_wj4lz16 %>%
    # Rename columns
    rename(
        Tree_Species_0011 = Tree_Species,
        Tree_Type_0011 = Tree_Type,
        Number_of_Trees_of_this_Species_0011 = count,
        Enter_a_date = Date
    ) %>%

    # Translate values
    mutate(Tree_Type_0011 = case_when(
        Tree_Type_0011 == "Plantados por el proyecto" ~ "Planted by your project",
        Tree_Type_0011 == "Ya presentes antes del proyecto" ~ "Already present prior to the project"
    ))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3.8. Verify results

# Sum all counts from the listed dataframes
total_tree_counts <- sum(
    PlantedTrees3$Numer_of_This_Species3,
    group_an2yk58$Number_of_Trees_of_this_Species2,
    group_ka7vj63$Number_of_Trees_of_this_Species_0012,
    group_oq8nt56$Number_of_Trees_of_this_Species1,
    group_ql6qg69$Number_of_Trees_of_this_Species_0014,
    group_wj4lz16$Number_of_Trees_of_this_Species_0011,
    na.rm = TRUE
)

# Calculate tree counts in original dataset
colombia_data_verify <- colombia_data_mod

# Create the flag indicators by summarizing at plot level
plot_flags <- colombia_data_verify %>%
    group_by(Plot_ID) %>%
    summarise(
        has_30x30_d1_100plus = any(
            Plot_Size %in% big_plots & d1 >= 100, na.rm = TRUE) * 1,
        has_30x30_d1_10to99 = any(
            Plot_Size %in% big_plots & d1 >= 10 & d1 < 100, na.rm = TRUE) * 1,
        has_30x30_d1_less_than_10 = any(
            Plot_Size %in% big_plots & d1 < 10, na.rm = TRUE) * 1,
        has_3x3_d1_100plus = any(
            Plot_Size %in% small_plots & d1 >= 100, na.rm = TRUE) * 1,
        has_3x3_d1_10to99 = any(
            Plot_Size %in% small_plots & d1 >= 10 & d1 < 100, na.rm = TRUE) * 1,
        has_3x3_d1_less_than_10 = any(
            Plot_Size %in% small_plots & d1 < 10, na.rm = TRUE) * 1,
        .groups = "drop"
    )

# Merge back to the original dataset if needed
colombia_data_verify <- colombia_data_verify %>%
    left_join(plot_flags, by = c(
        "Plot_ID")
    )


# Calculate the total number of trees according to original dataset
tree_count_original <- colombia_data_mod %>%
    filter(
        !is.na(Tree_Species),
        Tree_Species != "") %>%
    group_by(Plot_ID) %>%
    summarise(
    count = n(),
    tipo_parcela = first(Plot_Size),
    categoria_parcela = first(Plot_Type)
    ) %>%
    mutate(
    extrapolated_count = if_else(
        tipo_parcela == "10X10" & categoria_parcela == "MONITOREO",
        count * 9,
        count
    )
    ) %>%
    summarise(count = sum(extrapolated_count))

# Print results
cat("\n\nTotal summed tree counts: ", total_tree_counts, "\n")
cat("Non-empty 'ESPECIE' values: ", tree_count_original$count, "\n")

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#region 4. Create JSON file

# Harmonize datasets

# Define function to clean Plot_ID
clean_df <- function(df) {
    df %>%
        mutate(Plot_ID = gsub("S(?=\\d*$)|S$", "", Plot_ID, perl = TRUE)) %>%
        select(-any_of("Plot_Size"))
}

# Apply to all data frames
tree_monitoring_sheet <- clean_df(tree_monitoring_sheet)
PlantedTrees3 <- clean_df(PlantedTrees3)
group_an2yk58 <- clean_df(group_an2yk58)
group_ka7vj63 <- clean_df(group_ka7vj63)
group_oq8nt56 <- clean_df(group_oq8nt56)
group_ql6qg69 <- clean_df(group_ql6qg69)
group_wj4lz16 <- clean_df(group_wj4lz16)

# Aggregate tree monitoring sheet's Plot_ID
tree_monitoring_sheet_agg <- tree_monitoring_sheet %>%
    group_by(Plot_ID) %>%
    arrange(Plot_ID) %>%
    summarise(across(
        everything(),
        ~ {
        # Filter out NA or empty values
        non_na_vals <- .x[!is.na(.x) & .x != ""]
        if (length(non_na_vals) > 0) non_na_vals[1] else NA
        }
    ), .groups = "drop")

# Specify nested data
nested_PlantedTrees3 <- PlantedTrees3 %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(PlantedTrees3 = -c(Enter_a_date, Plot_ID))

nested_an2yk58 <- group_an2yk58 %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(group_an2yk58 = -c(Enter_a_date, Plot_ID))

nested_ka7vj63 <- group_ka7vj63 %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(group_ka7vj63 = -c(Enter_a_date, Plot_ID))

nested_oq8nt56 <- group_oq8nt56 %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(group_oq8nt56 = -c(Enter_a_date, Plot_ID))

nested_ql6qg69 <- group_ql6qg69 %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(group_ql6qg69 = -c(Enter_a_date, Plot_ID))

nested_wj4lz16 <- group_wj4lz16 %>%
    group_by(Enter_a_date, Plot_ID) %>%
    nest(group_wj4lz16 = -c(Enter_a_date, Plot_ID))

# Create a list of all data frames to join
dfs_to_join <- list(
    tree_monitoring_sheet_agg,
    nested_PlantedTrees3,
    nested_an2yk58,
    nested_ka7vj63,
    nested_oq8nt56,
    nested_ql6qg69,
    nested_wj4lz16
)

# Safely join all the nested data frames
final_nested_data <- reduce(
    dfs_to_join,
    ~ full_join(.x, .y, by = c("Enter_a_date", "Plot_ID"))
)

# Structure and JSON Conversion
structured_list <- final_nested_data %>%

    rowwise() %>%
    mutate(
        json_object = list(list(

        Enter_a_date = Enter_a_date,
        Country = Country,
        Organization_Name = Organization_Name,
        Site_ID = Site_ID,
        Timeframe = Timeframe,
        
        Plot = list(
            Plot_ID = Plot_ID,
            Plot_Type = Plot_Type,
            SiteSize = SiteSize,
            ControlSize = ControlSize,
            Plot_Permanence = Plot_Permanence,
            Resampling1 = Resampling1,
            TreesPresent = TreesPresent,
            PlantingPattern = PlantingPattern,
            Coordinate_System_Used = Coordinate_System_Used
        ),
        
        SmallSitesControl = list(
            Northeast_Corner_of_10m_x_10m_Plot = Northeast_Corner_of_10m_x_10m_Plot,
            Large1 = list(group_oq8nt56 = group_oq8nt56),
            Small1 = list(
            Centroid_of_3m_x_3m_Subplot1 = Centroid_of_3m_x_3m_Subplot1,
            group_wj4lz16 = group_wj4lz16
            )
        ),
        Large2 = list(group_an2yk58 = group_an2yk58),
        Small3 = list(
            Resampling2 = Resampling2,
            LittleTreesPresent = LittleTreesPresent,
            Centroid_of_3m_x_3m_Subplot2 = Centroid_of_3m_x_3m_Subplot2,
            group_ka7vj63 = group_ka7vj63
        ),
        Census = list(group_ql6qg69 = group_ql6qg69),
        PlantedTrees3 = PlantedTrees3,
        `__version__` = `__version__`,
        `_uuid` = `_uuid`
        ))
    ) %>%
    ungroup() %>%
    pull(json_object)

# Convert to JSON
json_output <- toJSON(
    structured_list, pretty = TRUE, auto_unbox = TRUE, dataframe = "rows"
)

# Save the JSON to a file
write(json_output, file.path("Colombia_Data", output_name))

# -#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
# END