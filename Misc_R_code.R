


#Get names of columns with gps coordinates in them
cols_with_plot_coords=c(grep("_Plot", names(df6$main)), grep("ubplot", names(df6$main)) ) 

#Cycle through columns of the "main" df with gps coordinates
lapply(1:length(cols_with_plot_coords), FUN=function(x){
  
  col_name=names(df6$main)[23]
  col_to_split=df6$main[[col_name]]
  plot_coords_out=lapply(1:length(col_to_split), FUN=function(y){
    coly=col_to_split[y]
    split_col=strsplit(coly[[1]], " ")
    plot_corner_coords=data.frame(lat=split_col[[1]][1],
                                  lon=split_col[[1]][2],
                                  altitude_m=split_col[[1]][3],
                                  accuracy_m=split_col[[1]][4] )
    
    plot_corner_coords$`_parent_index`
    plot
    attributes(col_to_split)$label
    return(plot_corner_coords)
  })
  =# do.call("rbind", plot_coords_out)
    
    
    split_col=strsplit(df6$main[col_name][[1]], " ")
  plot_corner_coords=data.frame(lat=split_col[[1]][1],
                                lon=split_col[[1]][2],
                                altitude_m=split_col[[1]][3],
                                accuracy_m=split_col[[1]][4])
  
})
names(df6$main)[c(grep("_Plot", names(df6$main)), grep("ubplot", names(df6$main)) ) ]
names(df6$main)[grep("ubplot", names(df6$main))]

df6$main$Centroid_of_3m_x_3m_Subplot

which(df6$main)
#Lat, long, alt, accuracy
function(colx){
  #colx=df6$main$Northeast_Corner_of_30m_x_30m_Plot[[1]]
  split_col=strsplit(colx, " ")
  plot_corner_coords=data.frame(lat=split_col[[1]][1],
                                lon=split_col[[1]][2],
                                altitude_m=split_col[[1]][3],
                                accuracy_m=split_col[[1]][4])
  return(plot_corner_coords)
  
}




#Maybe as a data-cleaning step, I can remove the columns of the 'main' df that 
#have no data in them?

#Cycle through each row of 'main' data, then determine if  `_parent_index` column in 
#the other given dataframes matche the row of the main dataframe. 
#Then repeat the main dataframe data for the length of the extracted concatenated data

out2=lapply(1:nrow(dfout$main), FUN=function(x){
  
  #Define row index from main df
  row_index = x
  #row_index = 17
  print(paste("l1",x))
  
  #Repeat these rows after extracting other 
  rep_rows = as.data.frame(dfout$main[row_index,])
  
  
  rep_rows$PlantedTrees3
  rep_rows$begin_repeat_Wq3dUfnDG
  
  #Cycle through non-main df's by name and extract all the rows from data.
  #If no rows exist, fill them in with NA's. Note: Start at 2 because 1st "name"
  #is the "main" df, which needs to be excluded from the processes below.
  rows_out=lapply(2:length(names(dfout)), FUN=function(y){
    df_number = y
    print(paste("l2",x, y))
    #df_number = 7
    
    df_name = names(dfout)[df_number]
    extract_rows = which(dfout[[df_name]]$`_parent_index`==row_index)
    
    #1) If non-main df has a row with an index value same as row number in main df
    #extract those rows, then repeat the main df data for each extracted row
    if(length(extract_rows)>0){
      
      unnested_rows=dfout[[df_name]][extract_rows,]
      unnested_rows$Data_type=rep(df_name, length(unnested_rows[,1]))
      
      return(unnested_rows)
    }else{
      
      #Return an df of NA's with same column names as current df
      blank_matrix=matrix(data= NA, nrow=1, ncol = ncol(dfout[[df_name]]))
      colnames(blank_matrix)=colnames(dfout[[df_name]])
      unnested_rows=as.data.frame(blank_matrix)
      unnested_rows$Data_type=rep(df_name, length(unnested_rows[,1]))
      return(unnested_rows)
    }
  })
  
  #Rbind data into a single df
  unnested_data=do.call("rbind", rows_out)
  
  #Replicate rows from main df for every row of the other df's extracted
  n = length(unnested_data[,1])
  reptd_rows = do.call("rbind", replicate(n, rep_rows, simplify = FALSE))
  data_out=cbind(reptd_rows,unnested_data)
  return(data_out)
}
)




