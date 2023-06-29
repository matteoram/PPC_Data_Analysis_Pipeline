#--------------------------------------------------------------------------------
#Project: Priceless Planet Coalition
#Author: Timothy Perez
#Date Updated: May 31, 2023
#Input: The most recent "main_data_PPC_Data__XXXX-XX_XX.csv", and the most recent 
#"tree_data_taxonomy_corrections_XXXX-XX-XX.csv" .
#Outputs: The final file is: Data_for_PPC_indicators_XXXX-XX-XX.

#Description: This code is part 3 of the analysis pipeline for key indicators for the Priceless 
#Planet Coalition. This code takes takes plot size information and tree abundance for all plots and formats
#data from two data sources above to prep data to calculate 1) Number of trees restored
#2) Number of of trees naturally regenerating, and 3) % survival of planted trees.
#-----------------------------------------------------------------------

#Get PPC site infor names for
#setwd("/Users/tperez/Library/CloudStorage/OneDrive-ConservationInternationalFoundation/Desktop/CI_git_projects/PPC/Raw_Data")
#get file names
path_to_raw_data = paste(getwd(), "Raw_Data", sep = "/")
ppc_data_files=list.files(path_to_raw_data)

#import main and tree data
main_data=read.csv(ppc_data_files[grep("main", ppc_data_files)])
tree_data=read.csv(ppc_data_files[grep("tree_data_taxonomy_corrections", ppc_data_files)])
unique(tree_data$tree_data.Tree_type)
main_data[which(main_data$main_data.Timeframe=="Y2.5"),]


#Project metrics:
#Project, plot, site
#1) Number of trees restored (survived and crowded in at year #) #number of trees retored dis aggregated by species.
#total number of tree will be scaled to the the 30x30m plot
#2) Number of of trees naturally regenerating disaggregated by species  
#Same as above but onlyy for regenerating trees
#3) % survival of planted trees
#Within Plot Survival Rate = (# of living planted trees in 30 x 30 m plot at Y5** / # of planted trees in 30 x 30 m plot at Y0) * 100
#main_data


upi=na.omit(unique(tree_data$tree_data._id))
treefreq_data=lapply(1:length(upi), FUN=function(x){
  maindf=main_data[which(main_data$main_data.X_id== upi[x]),]
  #print(x)
  #Select columns of interest main df
  main_cols=which(colnames(maindf) %in% c("main_data.Country",  "main_data.Site_ID", 
                                "main_data.Plot_ID", "main_data.Plot_Type",
                                "main_data.Plot_Permanence", "main_data.Timeframe",
                                "main_data.PlantingPattern", "main_data.Enter_a_date",
                                "main_data.Resampling1", "main_data.Resampling2",
                                "main_data.X_id"))
  
  main_add_to_tree = maindf[,main_cols]
  
  #get tree data with appropriate ID
  trees = tree_data[which(tree_data$tree_data._id== upi[x]),]
  
  #repeat main data for length of tree data
  repped_main = main_add_to_tree[rep(seq_len(nrow(main_add_to_tree)), each = length(trees$tree_data._id)), ]
  
  #cbind replicatedmain data to tree data
  site_tree_data=cbind(trees, repped_main)[,-1]
  
  #Remove rows that have no tree numbers #NOTE: This assumes users are entering 0's for tree frequencies
  site_tree_data=site_tree_data[-which(is.na(site_tree_data$tree_data.Number_of_trees)),]
   
  if(length(site_tree_data[,1])>0){
  #cylce through each row and scale the number of trees:
  Number_of_trees_per_30x30m = lapply(1:length(site_tree_data[,1]), FUN=function(x){
    
    #select row
    dfx=site_tree_data[x,]
    
    #resampled number for 30x or 10x plots
    resample_number1 = 1
    if(is.na(dfx$main_data.Resampling1 )){
      #assume this is 0
      resample_number1 =1
    }else if(dfx$main_data.Resampling1==0){
      resample_number1 = 1
    }else if(dfx$main_data.Resampling1==1){
      resample_number1 = 2
    }else if(dfx$main_data.Resampling1==2){
      resample_number1 = 3
    }
    
    #resampled number for 3x plots
    resample_number2 = 1
    if(is.na(dfx$main_data.Resampling2 )){
      #assume this is 0
      resample_number2 =1
    }else if(dfx$main_data.Resampling2==0){
      resample_number2 = 1
    }else if(dfx$main_data.Resampling2==1){
      resample_number2 = 2
    }else if(dfx$main_data.Resampling2==2){
      resample_number1 = 3
    }
    
    #get number of large dbh trees >10cm dbh
    Number_of_tree_in_30x30=NA
    size_class=NA
    if(dfx$tree_data.plot_type == "30x30m" | dfx$tree_data.plot_type=="30x30m_Special"){
      Number_of_tree_in_30x30=dfx$tree_data.Number_of_trees/resample_number1 
      size_class=">10cm"
      }
    
    if( dfx$tree_data.plot_type == "10x10m"){
      #Scale tree number to 30x30m by multiplying by 9
      Number_of_tree_in_30x30=(dfx$tree_data.Number_of_trees * 9)/resample_number1
      size_class=">10cm"
    }
     
    #Get number of medium-sized trees 1-9.9cm dbh
    if(dfx$tree_data.plot_type == "3x3m within 30x30m" | 
       dfx$tree_data.plot_type=="3x3m_Special" |
       dfx$tree_data.plot_type=="3x3m within 10x10m" ){
      #Scale tree number to 30x30m by multiplying by 100
      Number_of_tree_in_30x30=((dfx$tree_data.Number_of_trees*100)/resample_number2) 
      size_class="1-9.9cm"
      }
    
    #Get number of medium-sized trees 1cm dbh
    if(dfx$tree_data.plot_type== "1x1m"){
      #Scale tree number to 30x30m by multiplying by 900
      Number_of_tree_in_30x30=((dfx$tree_data.Number_of_trees*900)/resample_number2) 
      size_class="<1cm"}
    dfx_out = data.frame(Number_of_tree_in_30x30=Number_of_tree_in_30x30, size_class=size_class)
    return(dfx_out)
  })
  Number_of_trees_per_30x30m = do.call("rbind", Number_of_trees_per_30x30m)
  
  #Number of trees per 30x30m:
  site_tree_data$Number_of_trees_per_30x30m = Number_of_trees_per_30x30m$Number_of_tree_in_30x30
  site_tree_data$size_class = Number_of_trees_per_30x30m$size_class
  
  #get the number restores, planted, regnerating, etc, 
  tree_freqs=aggregate(site_tree_data$Number_of_trees_per_30x30m, list("Tree_type"=site_tree_data$tree_data.Tree_type,
                                                            "Species" = site_tree_data$tree_data.Species,
                                                            "size_class"=site_tree_data$size_class), sum)
  #Change column names to x
  colnames(tree_freqs)[which(colnames(tree_freqs)=="x")] = "Tree_per_sq_30m"
  
  #Get pertinent plot data and add it to the tree frequency data
  repped_main2=main_add_to_tree[rep(seq_len(nrow(main_add_to_tree)), each = length(tree_freqs[,1])), ]
  tree_freq_out=cbind(tree_freqs, repped_main2)
  
  return(tree_freq_out)
  }else{
    
    tree_freqs = data.frame("Tree_type" = NA, "Species" = NA, "size_class"=NA, "Tree_per_sq_30m" = NA)
    #Get pertinent plot data and add it to the tree frequency data
    repped_main2=main_add_to_tree[rep(seq_len(nrow(main_add_to_tree)), each = length(tree_freqs[,1])), ]
    tree_freq_out=cbind(tree_freqs, repped_main2)
    return(tree_freq_out)
    
  }
})
tree_freq_data_site=do.call("rbind", treefreq_data)

#-------------------------------------------------------------------------------
#OK now, I need a function that: 
#1) Subsets each unique site - plot
#get unique site/plot combos
uplot = unique(paste(tree_freq_data_site$main_data.X_id, tree_freq_data_site$main_data.Timeframe ))
#get unique size classes
U_sizes = unique(tree_freq_data_site$size_class)
U_sizes = U_sizes[-which(is.na(U_sizes))]

#get unique planting planting type
U_treetype = unique(tree_freq_data_site$Tree_type)
U_treetype = U_treetype[-which(U_treetype %in% c(NA, "don_t_know"))]

#Add missing 
tree_freq_data_site_v2 = lapply(1:length(uplot), FUN = function(x){
 #get rows of interest 
  #rows_oi = which( paste(tree_freq_data_site$main_data.Site_ID, tree_freq_data_site$main_data.Plot_ID) ==uplot[7] )
  rows_oi = which( paste(tree_freq_data_site$main_data.X_id, tree_freq_data_site$main_data.Timeframe) == uplot[x] )
  #print(paste("x=", x))
  data = tree_freq_data_site[rows_oi,]
  
  data_tt_out = lapply( 1:length(U_treetype), FUN=function(y){
      data_tt = data[which( data$Tree_type == U_treetype[y]),]
      #print(paste("y=", y))
      #If the length of this df is 0, it's empty, meaning that it has not size class data.
      #In this case, wee need to add 3 rows with empty species & frequencies data
      if( nrow(data_tt) ==0 ){
        #add addtional rows
        data_tt [ nrow(data_tt) + 3 , ] = NA
        #fill-in rows with necessary data
        data_tt$size_class = U_sizes
        data_tt$Tree_type = U_treetype[y]
        data_tt$Tree_per_sq_30m = 0
        data_tt$main_data.Plot_ID = unique(data_tt$main_data.Plot_ID)
        data_tt$main_data.Site_ID = unique(data_tt$main_data.Site_ID)
        data_tt$main_data.Country = unique(data_tt$main_data.Country)
        data_tt$main_data.X_id = unique(data_tt$main_data.X_id)
        data_tt$main_data.Plot_Type = unique(data_tt$main_data.Plot_Type)
        data_tt$main_data.Timeframe = unique(data_tt$main_data.Timeframe)
        #colnames(data_tt)
        #print("here")
        return(data_tt)        
      }else{
        #determine which size classes are missing from the data
        size_class_to_add = U_sizes[-which(U_sizes %in% unique(data_tt$size_class))]
        #add addtional rows
        nrow_add = length(size_class_to_add)
          if(nrow_add>0){
          data_tt2 = data_tt[-which(data_tt$size_class %in% U_sizes ),]
          data_tt2[ nrow(data_tt2) + nrow_add,] = NA
          #fill-in rows with necessary data
          data_tt2$size_class = size_class_to_add
          data_tt2$Tree_type = U_treetype[2]
          data_tt2$Tree_per_sq_30m = 0
          data_tt2$main_data.Plot_ID = unique(data_tt$main_data.Plot_ID)
          data_tt2$main_data.Site_ID = unique(data_tt$main_data.Site_ID)
          data_tt2$main_data.Country = unique(data_tt$main_data.Country)
          data_tt2$main_data.X_id = unique(data_tt$main_data.X_id)
          data_tt2$main_data.Plot_Type = unique(data_tt$main_data.Plot_Type)
          data_tt2$main_data.Timeframe = unique(data_tt2$main_data.Timeframe)
          data_tt3 = rbind(data_tt, data_tt2)
          #colnames(data_tt)
          #print("there")
          return(data_tt3)
        }else{
          return(data_tt)
        }
      }
    } )
  
  data_tt_out = do.call("rbind", data_tt_out)
  return(data_tt_out)
  
})
#Combine data into one dataframe:
data_out = do.call("rbind", tree_freq_data_site_v2)

#cycle through all of the different types of "main_data.PlantingPattern"s that were entered
#and manually enter the area each planting was given. Divide 900m2 by this number
# and save the output. This is the estimate number of plantings per 30m2.
uplantpat = unique(data_out$main_data.PlantingPattern)
planting_density_estimate = lapply(1:length(uplantpat), FUN=function(x){
  print(uplantpat[x])
   area_text = readline("Enter the two numbers to multiply (in meters) to calculate area for the entry above (e.g. 3*3: ")
   planting_density = eval(parse(text=area_text))
   planted_seedling_estimate = 900/planting_density
   return(planted_seedling_estimate)
})
planting_density_estimate = do.call("rbind", planting_density_estimate)
est_pl_dens_dat=data.frame(est_pl_dens = planting_density_estimate, PlantingPattern =uplantpat)


tree_freq_data_site_F = merge(data_out, est_pl_dens_dat, by.x = "main_data.PlantingPattern", by.y = "PlantingPattern")
tree_freq_data_site_F[which(is.na(tree_freq_data_site_F$est_pl_dens)),]$est_pl_dens = 0
nrow(data_out)==nrow(tree_freq_data_site)

date = Sys.Date()
PPC_ind_file_name = paste(paste("Data_for_PPC_indicators", date, sep="_"), "csv", sep=".")
write.csv(tree_freq_data_site_F, PPC_ind_file_name, row.names = F)


#Notes from Starry & Isabel:
#1) take site-level average of 1x1  -  determine if a 1x1 plot is a single value or the average within a site. 
#2) Trying to differentiate restults based on restoration strategies, then implementation partner, & variation in success from country to country
#What are the effects of different locations on KPIs (e.g. survival, rainfall -  what other environmental parameter can predict KPI's,
#how does land-use history influence restoration KPIs?)

#Land-use: have distrubance history back to 2010, we have recrods of fire, records if deforestation occurred; some user-entered data about landscape. 
#Somewhere in the database, there is the option of recroding disturbance. Users can enter type of distrubance: ecological, flood, fire,

#ANR is a research priority, we also have financial information to incorporate into effectiveness. 