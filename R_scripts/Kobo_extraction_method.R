#--------------------------------------------------------------------------------
#Project: Priceless Planet Coalition
#Author: Timothy Perez
#Date Updated: May 30, 2023
#Input: Kobo username and password. User will be prompted for these inputs.
#Outputs: 7 .csv file stored in a "Raw_Data" folder

#Description: This code is the 1st part of the analysis pipeline for key indicators for the Priceless 
#Planet Coalition. This code allows an authorized user to access the Kobotools
#website and download the data that has been input by users. This code unnests and
#flattens the the data downloaded from Kobo. The outputs from this code are used
#for the "Taxonomic_corrections_script.R" and "Generate_data_for_PPC_indicators.R
#R scripts.
#--------------------------------------------------------------------------------


setwd("/Users/tperez/Library/CloudStorage/OneDrive-ConservationInternationalFoundation/Desktop/CI_git_projects/PPC/")
#remotes::install_gitlab("dickoa/robotoolbox")
library(robotoolbox)
library("dplyr")

#have user input kobo username and PW:
UN <- readline(prompt="Enter Kobo username: ")
PW <- readline(prompt="Enter Kobo password: ")

#get kobot access token:
tkn=kobo_token(username = UN,
           password = PW,
           url = "kf.kobotoolbox.org")

#define the URL for the PPC data:
#KOBOTOOLBOX_URL="kf.kobotoolbox.org/#/forms/axhiGJQ8JzwkTYcgDTroJu" #
#KOBOTOOLBOX_TOKEN=tkn

kobo_settings()
l <- kobo_asset_list()

uid <- l |>
  filter(name == "Tree Monitoring") |>
  pull(uid) |>
  first()

asset <- kobo_asset(uid)

df <- kobo_submissions(asset) ## or df <-  kobo_data(asset)

#Check dimensions
#names(df) #101 columns, 324 rows, ncol(df$main); nrow(df$main) #Number of columns increased!

#The main data has retained some nested columns that need to be extracted and 
#made into additional, external df's.

#The code below cycle through the 'main' df and extract all lists then, cycles through rows of
#each list because there are dataframes stored within these rows. #The output
#is a "flattened" list of the listed dfs in "main"
main_listed_cols = which(sapply(df$main, is.list))
unlisted_from_maindf=lapply(1:length(main_listed_cols), FUN = function(x){
   
  listed_col_numx = main_listed_cols[x]
  #listed_col_numx = main_listed_cols[19]
  #print(paste(x))
  
  #Get nested df within main
  listed_colx = df$main[listed_col_numx]
  listed_colx_name = names(df$main[listed_col_numx])
  
  #Get which rows have dfs within them
  list_within_cols = sapply(listed_colx[[listed_colx_name]], is.list)
  
  #Here i need to cycle through all of the rows of the parent index
  unnested_dfx = lapply(1:length(list_within_cols), FUN=function(y){
    parent_index = y
    #id=df$main$`_id`[parent_index]
    print(paste(x, y))
    #parent_index = 9
    nested_data = list_within_cols[parent_index]
  
    #If there is nested data, extract it. Add `_index` `_parent_index` `_parent_table_name` tables
    if(nested_data == T){
      #print("if 1")
      nested_df=listed_colx[[listed_colx_name]][[parent_index]]
      nested_df$`_index`=1:length(nested_df[,1])
      nested_df$`_parent_index`=rep(parent_index, length(nested_df[,1]))
      nested_df$`_parent_table`=rep("main", length(nested_df[,1]))
      nested_df$Data_type= rep(listed_colx_name, length(nested_df[,1]))
      nested_df$`_id`=df$main$`_id`[parent_index]
      
      return(nested_df)
    }else if(nested_data == F){
      #If False, the cell has potentially other types of data in it, but is not necessarily a list
      #print("if 2")
      
      sublist1_length=length(listed_colx[[listed_colx_name]][[parent_index]])
      if(sublist1_length<1){
        #If length is 0, meaning empty, values are likely null, too
        #print("if 2.1")
        nested_df=data.frame(col1=NA)
        names(nested_df)=paste(listed_colx_name)
        nested_df$`_index`=1:length(nested_df[,1])
        nested_df$`_parent_index`=rep(parent_index, length(nested_df[,1]))
        nested_df$`_parent_table`=rep("main", length(nested_df[,1]))
        nested_df$Data_type= rep(listed_colx_name, length(nested_df[,1]))
        nested_df$`_id`=df$main$`_id`[parent_index]
        return(nested_df)
        
      }else if(sublist1_length>1){
        #If T, this is (probably) the geolocation data; split "cells" into lat and lon data
        #print("if 2.2")
        lat=nested_df=listed_colx[[listed_colx_name]][[parent_index]][1]
        lon=nested_df=listed_colx[[listed_colx_name]][[parent_index]][2]
        nested_df=data.frame(lat=lat, lon=lon)
        nested_df$`_index`=1:length(nested_df[,1])
        nested_df$`_parent_index`=rep(parent_index, length(nested_df[,1]))
        nested_df$`_parent_table`=rep("main", length(nested_df[,1]))
        nested_df$Data_type= rep(listed_colx_name, length(nested_df[,1]))
        nested_df$`_id`=df$main$`_id`[parent_index]
        
        return(nested_df)
      }else{
        #Other data not longer than 1 item, take
        #print("if 2.3")
        col1=listed_colx[[listed_colx_name]][[parent_index]][[1]]
        nested_df=data.frame(column1 = col1[1] )
        names(nested_df)=paste(listed_colx_name)
        nested_df$`_index`=1:length(nested_df[,1])
        nested_df$`_parent_index`=rep(parent_index, length(nested_df[,1]))
        nested_df$`_parent_table`=rep("main", length(nested_df[,1]))
        nested_df$Data_type= rep(listed_colx_name, length(nested_df[,1]))
        nested_df$`_id`=df$main$`_id`[parent_index]
        
        return(nested_df)
      }
    }
  }) 
  
  #This takes the place of the do.call("rbind" data) code by filling-in NA for mismatched columns
  extracted_dfx=data.table::rbindlist(unnested_dfx, fill = TRUE)
  
   
})

#name list items
names(unlisted_from_maindf)=names(df$main[main_listed_cols]) 
#table_names=names(unlisted_from_maindf)

#Convert unlisted_from_maindf list into a list
unlisted_from_maindf=as.list(unlisted_from_maindf)

#Remove the main_listed_cols from the main df
remove_cols=names(df$main)[main_listed_cols] #select column names to remove from df$main
main=df$main %>% select(!remove_cols) #redefine main df with the main_listed_cols removed
main=as.list(main) #make sure main df is a list

#Remove "main df from the list of df's
df2=df[-which(names(df)=="main")] 
#make sure df is a list
df2=as.list(df2) 

#Add the edited main df, that had main_listed_cols removed removed, back into the list of df's
df3=c(df2, main)

#Add the data that was unnested from the main df into all of the data.
df4=c(df3, unlisted_from_maindf)

#Get the unique indices of columns that need to be reformatted
col_names_toformat=c(grep("group", names(df4)),
grep("Planted", names(df4)),
grep("begin", names(df4)))

#Cycle through data frames and rename and add certain columns
#This is done so that the names of the columns can be consistent when data are
#stacked in a future step.
dfout = lapply(1:length(col_names_toformat), function(y){
  
  df_number = col_names_toformat[y]
  #df_number = col_names_toformat[3]
  
  print(y)
  
  #Change column names for all nested df other than the "main" df
  column_names=names(df4[[df_number]]) #get columns names
  
  #change to df because index operation to remove "group" column is not working for some reason
  df1=as.data.frame(df4[[df_number]])  
  #df4$PlantedTrees3
  
  #Get column name to determine plot type
  plot_type = "NA"
  col_nameplot=colnames(df1) [grep("Tree_Species", colnames(df1)) ]
  if(col_nameplot=="Tree_Species2"){
    plot_type ="30x30m"
  }else if(col_nameplot=="Tree_Species_0014"){
    plot_type ="30x30m_Special"
  }else if(col_nameplot=="Tree_Species_Seedlings"){
    plot_type = "30x30m"
  }else if(col_nameplot=="Tree_Species_Seedlings5"){
    plot_type = "30x30m_Special"
  }else if(col_nameplot=="Tree_Species1"){
    plot_type = "10x10m"
  }else if(col_nameplot=="Tree_Species_001"){
    plot_type = "3x3m_Special"
  }else if(col_nameplot=="Tree_Species_0011"){
    plot_type = "3x3m within 10x10m"
  }else if(col_nameplot=="Tree_Species_0012"){
    plot_type = "3x3m within 30x30m"
  }else if(col_nameplot=="Tree_Species_Tiny"){
    plot_type = "1x1m"
  }else if(col_nameplot=="Tree_Species_0012_1"){
    plot_type = "Unk"
  }else if(col_nameplot=="Tree_Species"){
    plot_type = "Unk"
  }
    
  
  #Change column names
  colnames(df1) [grep("Tree_Species", colnames(df1)) ] = "Species"
  
  
  #Remove column names that have "group", "planted", begin in them
  remove_group_colnames=unique(c( grep("begin", colnames(df1)), 
                                  grep("group", colnames(df1)),
                                  grep("Planted", colnames(df1))))
  
  
  if(length(remove_group_colnames)<1){
    df1=df1
  }else{
    df1=df1[,-c(remove_group_colnames)]
  }
  
  column_names2=names(df1) #get columns names
  #Rename columsns to say "Species"
  df2 = df1
  num_tree_col=grep("his_Species", column_names2)
  if(length(num_tree_col)<1){ 
    
    df2 <- df1 %>% tibble::add_column(Number_of_trees = "NA", .after = "Species")
    df2 = df2 %>% select(order(colnames(df2)))
    
    #as_factor(df3$Tree_Type()
    #df3 = df2$Tree_type=NA
    #return(df3)
  }else{
    
    df2 = df1 %>% rename_with(.col=grep("his_Species", column_names2) , ~"Number_of_trees")
    df2 %>% select(order(colnames(df2)))
    #return(df3)
  }
  
  ##Rename columns to say Tree-Type
  df3 = df2
  #If no Tree_Type column exists, add it to the data frame
  tree_type_col=grep("Tree_Type", column_names2)
  if(length(tree_type_col)<1){ 
      
     df3 <- df2 %>% tibble::add_column(Tree_type = "NA", .after = "Species")
     df2 = df2 %>% select(order(colnames(df2)))
    #as_factor(df3$Tree_Type()
    #df3 = df2$Tree_type=NA
    #return(df3)
  }else{
      
    df3 = df2 %>% rename_with(.col=grep("Tree_Type", column_names2) , ~"Tree_type")
    df3 = df3[,sort(colnames(df3))]
    #return(df3)
  }
   
  #Add Data_type columns if it doesn't exist
  Data_type_col=grep("Data_type", column_names2)
  if(length(Data_type_col)<1){
    df_name = names(df4)[df_number] #subset df of interest
    df3$Data_type= rep(df_name, length(df3[,1]))
    df3 = df3[,sort(colnames(df3))]
  }
  df3$plot_type=plot_type
  df3$Tree_type=as.character(df3$Tree_type)
  return(df3) 
})

#Name lists
#names(dfout)=names(df4)[col_names_toformat]

#All df's should have same format and can be stakced into a simplified df.
dfout = data.table::rbindlist(dfout, fill = TRUE)


#Determine which df are in names(df4), but not names(dfout), and remove them.
remove_col_names_from_df4=names(df4)[col_names_toformat]
df5=df4[-which( names(df4) %in% remove_col_names_from_df4 )]

#add renamed df's in dfout back to list of tables
df6=list(df5, dfout)
names(df6)=c("main", "tree_dat")
names(df6$tree_dat)
#Misc tree data to reformat and combine with other tree data
data2=data.frame(Species = df6$main$Tree_Species2,
           Number_of_trees = df6$main$Number_of_Trees_of_this_Species2,
           Tree_type=as.character(df6$main$Tree_Type2),
           plot_type = "30x30m")
data2$`_index` = 1
data2$`_parent_index` = 1:length(data2[,1])
data2$`_parent_table_name` = rep("Tree_Species2", length(data2[,1]))
data2$Data_type = NA
data2$`_id` = df6$main$`_id`
data2$`_parent_table` = "main"

#data_0012
data12=data.frame(Species = df6$main$Tree_Species_0012,
                 Number_of_trees = df6$main$Number_of_Trees_of_this_Species_0012,
                 Tree_type=as.character(df6$main$Tree_Type_0012 ),
                 plot_type = "3x3m within 30x30m") 
data12$`_index` = 1
data12$`_parent_index` = 1:length(data12[,1])
data12$`_parent_table_name` = rep("Tree_Species_0012", length(data12[,1]))
data12$Data_type = NA
data12$`_id` = df6$main$`_id`
data12$`_parent_table` = "main"

#data_0012_1
data0012_1=data.frame(Species = df6$main$Tree_Species_0012_1,
                  Number_of_trees = df6$main$Number_of_Trees_of_this_Species_0012_1,
                  Tree_type=as.character(df6$main$Tree_Type_0012_1 ),
                  plot_type = "Unk")
data0012_1$`_index` = 1
data0012_1$`_parent_index` = 1:length(data0012_1[,1])
data0012_1$`_parent_table_name` = rep("Tree_Species_0012", length(data0012_1[,1]))
data0012_1$Data_type = NA
data0012_1$`_id` = df6$main$`_id`
data0012_1$`_parent_table` = "main"

misc_tree_data=do.call("rbind", list(data2, data12,  data0012_1))
#sort(colnames(df6$tree_dat))==sort(colnames(misc_tree_data)) #are column names the same?
#Sort misc_tree_data to follow order of f6$tree_dat
misc_tree_data=misc_tree_data[,colnames(df6$tree_dat)] 
#Add misc tree data to 
tree_data=rbind(df6$tree_dat, misc_tree_data)


#Extract nested lists
attachments = df6$main$`_attachments`
geolocation = df6$main$`_geolocation`
tags = df6$main$`_tags`
notes = df6$main$`_notes`
validation = df6$main$`_validation_status`

#
df7=df6$main
#Remove columns containing the misc_tree_data and lists from the "main" df
misc_col_names_remove = names(df7)[intersect( grep("ree", names(df7)), grep("2", names(df7)))]
misc_col_names_remove = intersect( grep("ree", names(df7)), grep("2", names(df7)))
remove_listed_cols_names = unname(which(sapply(df7, is.list)))
remove_from_df7 = c(remove_listed_cols_names, misc_col_names_remove)
main_data=as.data.frame(df7[-remove_from_df7])


#Combine all external dataframes into one list
final_data=list(main_data, tree_data, attachments, geolocation, tags, notes, validation)
names(final_data)=c("main_data", "tree_data", "attachments", "geolocation", "tags", "notes", "validation")

setwd("/Users/tperez/Library/CloudStorage/OneDrive-ConservationInternationalFoundation/Desktop/CI_git_projects/PPC/Raw_Data")
#Write final data to .csvs
length(final_data)
lapply(1:length(final_data), FUN=function(x){
  list_name=names(final_data)[x]
  date=Sys.Date()
  file_name=paste(paste(list_name, "PPC_Data", date, sep="_" ), "csv", sep=".")
  write.csv(final_data[x], file_name)
})




