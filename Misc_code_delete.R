
library(httr)

# Replace "YOUR_API_TOKEN" with your actual API token
token <- "23357f916460d272edb4672a14f83ea6ec806679"
auth <- paste0("Token ", token)

forms_url <- "https://kf.kobotoolbox.org/api/v2/assets"
forms_response <- GET(url = forms_url, add_headers(Authorization = auth))
forms_data <- content(forms_response, as = "text", encoding = "UTF-8")
forms <- jsonlite::fromJSON(forms_data)$results

form_id <- "axhiGJQ8JzwkTYcgDTroJu"
data_url <- paste0("https://kf.kobotoolbox.org/api/v2/assets/", form_id, "/data")
data_response <- GET(url = data_url, add_headers(Authorization = auth))
data_content <- content(data_response, as = "text", encoding = "UTF-8")
data <- jsonlite::fromJSON(data_content)$results
data2 <- jsonlite::fromJSON(data_content, simplifyVector=FALSE)$results

length(data2)
str(data2[[38]])
colnames(data2[[38]])
names(data2[[38]])

#Apply a function to cycle through the "rows" of data:
#each row is potentially a nested list.
lapply(1:length(data2), FUN=function(x){
  
  #This selects data from teach row
  df=data2[[x]]
  df=data2[[12]]
  
  #This determines which columns within rows are nested lists
  listed_cols = sapply(df, is.list)
  list_names=names(df[which(listed_cols)])
  
  #These are the rows within listed columns that contain information that needs 
  #to be repeated within the other listed columns:
  repeat4_rows = c("_attachments", "_geolocation", "_tags", "_notes", "_validation_status" ) 
  repeat4_row_num = list_names[which(list_names %in% repeat4_rows)]
  
  
  #Cycle thorugh the repeat4_rows list and flatten these data into one row of data
  lapply(1:length(repeat4_row_num), FUN=function(r){
    r=repeat4_row_num[1]
    #ok, turns out i'll need to cycle though this list, too because
    df[[r]]
    str(df[[r]])
    listed_out1=lapply(length(1:length(df[[r]])), FUN=function(r2){
      return_listed=data.frame(t(df[[r]][[2]]))
      return(return_listed)
      #for the attachments data, I'll have to us the ID column to match to appropriate
      #ID column for other data.
      #Other data sshoulf be easy to transform
    }
    do.call("rbind", listed_out1)
    
  })
  
  #list_names are the names of the lists that need to be 1) extracted
  #2) have other site & plot data appendend to each row, and 3) concatenated to 
  #other  list data from the same/row site.
  
  list_names=names(data2[[12]][which(listed_cols)])
  
  #Section 1: Repeat the data from these sections for each row:
  
  #I have to cycle through each column of the 
  lapply(1:length(repeat4_rows), FUN=function(x){
    selected_row=repeat4_rows[x]
    print(data2[[12]][which(listed_cols)][[x]])
  })
  
  
  =length(data2[[12]][which(listed_cols)][[]][1])
 [1]
  
  #repeat4_rows removed from list of columns that need to be flattned.
  list_names2=list_names[-which(list_names %in% repeat4_rows)]
  
  
  #list names is going to be different for each "row"
  
  #Cycle through through lists and determine which lists are null/empty
  #then, remove these columns, or just have them enter an NA
  
  #All listed columns need to saved separately except for geolocation
  
})


sapply(data2[[38]], is.list)

data2[[38]]$`Small3/begin_repeat_ELDOiv5Dr`

str(data2) 
#Order data by date to match online table for simplicity
data=data[order(data$Enter_a_date),]

colnames(data)
#Get the columsn that are nested as lists 
list_cols <- sapply(data, is.list)
#iterare through all of the nested/list columns

colnames(data[which(list_cols)[1]]) #Large/begin_repeat_uPU1AjRnH = ? 
colnames(data[which(list_cols)[2]]) #_attachments can be save as a separate file, not useful here. 
colnames(data[which(list_cols)[3]]) # _geolocation = lon, lat
colnames(data[which(list_cols)[4]]) #_tags =  #nothing
colnames(data[which(list_cols)[5]]) #_notes =  nothing
colnames(data[which(list_cols)[6]]) #_validation_status  =nothing
colnames(data[which(list_cols)[7]]) #Large2/begin_repeat_ztsNCjoPm =  Tree Sampling: 30m x 30m Plot (All trees with DBH >10cm are recorded)       ;Species, tree type, number of individuals  - Large plot?
colnames(data[which(list_cols)[8]]) #Small3/begin_repeat_ELDOiv5Dr = Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded)                  ;Species, tree type, number of individuals
colnames(data[which(list_cols)[9]]) #SmallSitesControl/Large1/begin_repeat_YDvKUvA32 = Special Scenario: Restoration site is too small to accommodate 30m x 30m monitoring plot so a 10m x 10m monitoring plot with a nested 3m x 3m sub-plot is used: 10m x 10m Plot (All trees with DBH >10cm are recorded)
colnames(data[which(list_cols)[10]]) #SmallSitesControl/Small1/begin_repeat_Wq3dUfnDG = Special Scenario: Control site is too small to accommodate 30m x 30m monitoring plot so a 10m x 10m monitoring plot with a nested 3m x 3m sub-plot is used/Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded): Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded)
colnames(data[which(list_cols)[11]]) #PlantedTrees3 =  If there are any remaining PLANTED trees that were not already counted, then they should be recorded below./Number of Trees of this Species: Data: Tree species, number of species. #tree
colnames(data[which(list_cols)[12]]) #Large2/group_an2yk58 = Tree Sampling: 30m x 30m Plot (All trees with DBH >10cm are recorded)
colnames(data[which(list_cols)[13]]) #Small3/group_ka7vj63 = Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded)
colnames(data[which(list_cols)[14]]) #TinyPlots/group_qr1fe53 = Optional Tree Sampling: 1m x 1m nested plots (all trees with a DBH <1cm are recorded)TinyPlots
colnames(data[which(list_cols)[15]]) #SmallSites/group_ai86m63 = #It's unclear how this relates to the data sheet., but column title is: Tree Sampling: 30m x 30m Plot (All trees with DBH >10cm are recorded)/group_an2yk58/Tree Species (use scientific name)
colnames(data[which(list_cols)[16]]) #SmallSitesControl/Large1/group_oq8nt56 = Special Scenario: Restoration site is too small to accommodate 30m x 30m monitoring plot so a 10m x 10m monitoring plot with a nested 3m x 3m sub-plot is used

str(data[which(list_cols)[1]]) #Large/begin_repeat_uPU1AjRnH = ? 
str(data[which(list_cols)[2]]) #_attachments can be save as a separate file, not useful here. 
str(data[which(list_cols)[3]]) # _geolocation = lon, lat
str(data[which(list_cols)[4]]) #_tags =  #nothing
str(data[which(list_cols)[5]]) #_notes =  nothing
str(data[which(list_cols)[6]]) #_validation_status  =nothing
str(data[which(list_cols)[7]]) #Large2/begin_repeat_ztsNCjoPm =  Tree Sampling: 30m x 30m Plot (All trees with DBH >10cm are recorded)       ;Species, tree type, number of individuals  - Large plot?
str(data[which(list_cols)[8]]) #Small3/begin_repeat_ELDOiv5Dr = Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded)                  ;Species, tree type, number of individuals
str(data[which(list_cols)[9]]) #SmallSitesControl/Large1/begin_repeat_YDvKUvA32 = Special Scenario: Restoration site is too small to accommodate 30m x 30m monitoring plot so a 10m x 10m monitoring plot with a nested 3m x 3m sub-plot is used: 10m x 10m Plot (All trees with DBH >10cm are recorded)
str(data[which(list_cols)[10]]) #SmallSitesControl/Small1/begin_repeat_Wq3dUfnDG = Special Scenario: Control site is too small to accommodate 30m x 30m monitoring plot so a 10m x 10m monitoring plot with a nested 3m x 3m sub-plot is used/Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded): Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded)
str(data[which(list_cols)[11]]) #PlantedTrees3 =  If there are any remaining PLANTED trees that were not already counted, then they should be recorded below./Number of Trees of this Species: Data: Tree species, number of species. #tree
str(data[which(list_cols)[12]]) #Large2/group_an2yk58 = Tree Sampling: 30m x 30m Plot (All trees with DBH >10cm are recorded)
str(data[which(list_cols)[13]]) #Small3/group_ka7vj63 = Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded)
str(data[which(list_cols)[14]]) #TinyPlots/group_qr1fe53 = Optional Tree Sampling: 1m x 1m nested plots (all trees with a DBH <1cm are recorded)TinyPlots
str(data[which(list_cols)[15]]) #SmallSites/group_ai86m63 = Tree Sampling: 30m x 30m Plot (All trees with DBH >10cm are recorded)Large2
str(data[which(list_cols)[16]]) 
data[which(list_cols)[16]]


max_len=max(sapply(df$scores, length), na.rm = TRUE)
num_cols <- max(sapply(df$scores, length), na.rm = TRUE)


# Determine the number of columns in the list-column
max_len=max(sapply(data[,which(list_cols)[16]], length), na.rm = TRUE)

df=data[,which(list_cols)[16]]
df[1]
df[1]

stack(setNames(data[,which(list_cols)[16]]))
colnames(data[,which(list_cols)[16]])

lapply(1:length(df), FUN=function(x){
  ll=length(df[x])
  strsplit( colnames(df[x][[ll]], "/" )
  print(colnames(df[x][[ll]] ))
})

ll=length(df[52])
strsplit(  colnames(df[52][[ll]]), "/")


unlist(df)
# Create a function to flatten each row of the dataframe
flatten_row <- function(df) {
  dfx=df
  # Create a vector with the same number of columns as the list-column
  column_vec <- rep(NA, max_len)
  
  # Fill the vector with the values from the list-column
  if (!is.null(dfx)) {
    column_vec[1:length(ncol(dfx))] <- dfx
  }
  
  return(column_vec)
}

# Apply the flatten_row function to each row of the dataframe
df_flat <- t(lapply(df, flatten_row))


# Convert the resulting matrix to a dataframe
df_flat <- data.frame(df_flat)

# Rename the columns
colnames(df_flat) <- c("id", "name", paste0("score", 1:num_cols))

# Print the flattened dataframe
df_flat


#for each nested column:
  #1) determine which "rows" corresponding to list numbers have several entries
  #2) replicate the rows with 
  #Take length of list, divide by three because there are 3 types of data: Species, type of planting and number of individuals
data[which(list_cols)[2]]


data$Organization_Name
data$Site_ID
data$Timeframe
data$SiteType
data$Start_Time
data$End_Time
data$`Plot/Plot_ID`
data$`Plot/Plot_Type`
data$`Plot/Plot_Permanence`
data$`Plot/Strata_Number`
data$`Plot/Resampling1`
data$`Plot/PlantingPattern`
data$`Plot/Coordinate_System_Used`
#These rows need to be un-nested
data$`Plot/Northeast_Corner_of_30m_x_30m_Plot`
data$`Plot/Southeast_Corner_of_30m_x_30m_Plot`
data$`Plot/Northwest_Corner_of_30m_x_30m_Plot`
data$`Plot/Southwest_Corner_of_30m_x_30m_Plot`
#this is a list
data$`Large/begin_repeat_uPU1AjRnH`
data$`meta/instanceID`
data$`Plot/SiteSize`
#This is a list
data$`Large2/begin_repeat_ztsNCjoPm`
data$`Large2/Corner_Photo_Taken_From1_001`
data$`Large2/Photo_of_AB_Sightline2`
data$`Large2/Photo_of_AC_Sightline2`
data$`Large2/Photo_of_AD_Sightline2`
data$`Small3/Resampling2`
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
data$
  


library(tidyr)
unnested_data <- unnest(data, results1, results2)
colnames(data)

data$`Large/begin_repeat_uPU1AjRnH`
'Tree Sampling: 3m x 3m Nested Plot (All trees with DBH of 1 - 9.9 cm are recorded)'