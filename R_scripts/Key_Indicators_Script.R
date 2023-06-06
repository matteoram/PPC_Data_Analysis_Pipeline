#--------------------------------------------------------------------------------
#Project: Priceless Planet Coalition
#Author: Timothy Perez
#Date Updated: May 31, 2023
#Input: The most recent "Data_for_PPC_indicators_XXXX-XX_XX.csv"
#Outputs: The final data are located in the Final_Data Folder. There are 6 files:
#Regeneration, restored, and survival data for plot-levels and site-levels. Currentl

#Description: This code is part 4 of the analysis pipeline for key indicators for the Priceless 
#Planet Coalition. This code calculates regeneration, restored, and survival
#-----------------------------------------------------------------------
#Get data for PPC indicators:
setwd("/Users/tperez/Library/CloudStorage/OneDrive-ConservationInternationalFoundation/Desktop/CI_git_projects/PPC/Raw_Data")
ppc_data_files= list.files(getwd())

#import main and tree data
ind_data = read.csv(ppc_data_files[grep("Data_for_PPC_indicators", ppc_data_files)])

#Number of trees restored: Plot, Site, country
#For every plot that is missing data from a given size classes, it needs to be populated with a zero
#-------------------------------------------------------------------------------
#Calculate the number of restored trees

ind_data_regen = ind_data[which(ind_data$Tree_type == "naturally_regenerating" | 
                            ind_data$Tree_type == "planted"),]

#Number of regeneratin trees by plot
Ntrees_regen_plot = aggregate(ind_data_regen[,c("Tree_per_sq_30m", "est_pl_dens")], 
                              list("size_class"=ind_data_regen$size_class,
                                                                 "Tree_type"=ind_data_regen$Tree_type,
                                                                 "Species"=ind_data_regen$Species,
                                                                 "Timeframe"=ind_data_regen$main_data.Timeframe,
                                                                 "Plot_ID"=ind_data_regen$main_data.Plot_ID,
                                                                 "Site_ID"=ind_data_regen$main_data.Site_ID), sum)
#colnames(Ntrees_regen_plot)[which(colnames(Ntrees_regen_plot)=="x")]="num_regen_trees"

#Number of regenerating trees by site
Ntrees_regen_site=aggregate(ind_data_regen[,c("Tree_per_sq_30m", "est_pl_dens")], 
                            list("size_class"=ind_data_regen$size_class,
                                                                 "Tree_type"=ind_data_regen$Tree_type,
                                                                 "Species"=ind_data_regen$Species,
                                                                 "Timeframe"=ind_data_regen$main_data.Timeframe,
                                                                 "Site_ID"=ind_data_regen$main_data.Site_ID), sum)
#colnames(Ntrees_regen_site)[which(colnames(Ntrees_regen_site)=="x")]="num_regen_trees"

#-------------------------------------------------------------------------------
#Calculate the number of naturally regenerating trees

ind_data_natregen=ind_data[which(ind_data$Tree_type == "naturally_regenerating"),]

#Number of regeneratin trees by plot
Ntrees_natregen_plot = aggregate(ind_data_natregen[,c("Tree_per_sq_30m", "est_pl_dens")], 
                               list("size_class"=ind_data_natregen$size_class,
                                                                 "Tree_type"=ind_data_natregen$Tree_type,
                                                                 "Species"=ind_data_natregen$Species,
                                                                 "Timeframe"=ind_data_natregen$main_data.Timeframe,
                                                                 "Plot_ID"=ind_data_natregen$main_data.Plot_ID,
                                                                 "Site_ID"=ind_data_natregen$main_data.Site_ID), sum)
#colnames(Ntrees_natregen_plot)[which(colnames(Ntrees_natregen_plot)=="x")]="num_natregen_trees"

#Number of regenerating trees by site
Ntrees_natregen_site = aggregate(ind_data_natregen[,c("Tree_per_sq_30m", "est_pl_dens")], 
                               list("size_class"=ind_data_natregen$size_class,
                                                                 "Tree_type"=ind_data_natregen$Tree_type,
                                                                 "Species"=ind_data_natregen$Species,
                                                                 "Timeframe"=ind_data_natregen$main_data.Timeframe,
                                                                 "Site_ID"=ind_data_natregen$main_data.Site_ID), sum)
#colnames(Ntrees_natregen_site)[which(colnames(Ntrees_natregen_site)=="x")]="num_natregen_trees"

#-------------------------------------------------------------------------------
#Calculate % survival from year-to-year and size class.

#Subset all planted trees data
planted_trees = Ntrees_regen_plot[which(Ntrees_regen_plot$Tree_type=="planted"),]
#Remove Y to get year that data was monitored
planted_trees$year = as.numeric(gsub("Y", "", planted_trees$Timeframe))
planted_trees=planted_trees[order(planted_trees$year),]

sl_out = lapply(unique(planted_trees$Site_ID), FUN=function(x){
  
  site_level = planted_trees[which(planted_trees$Site_ID==x),]
  print(x)
  pl_out = lapply(unique(site_level$Plot_ID), FUN=function(y){
    plot_level = site_level[which(site_level$Plot_ID==y),]
    print(y)
    
    sl_out = lapply(unique(plot_level$Species), FUN=function(z){
      species_level = plot_level[which(plot_level$Species==z),]
      print(z)
      
     sc_out = lapply(unique(species_level$size_class), FUN=function(q){
        size_class = species_level[which(species_level$size_class==q),]
        print(q)
        
        size_class = size_class[order(size_class$year),]
        num_survived = c(diff(c(size_class$Tree_per_sq_30m, NA)),NA)
        dl = nrow(size_class)
        size_class$num_survived2 = num_survived[1:dl]
        size_class$percent_survived = (size_class$Tree_per_sq_30m-num_survived2)/size_class$Tree_per_sq_30m
        
        return(size_class)
      })
     sc_out_df = do.call("rbind", sc_out) 
     return(sc_out_df)
    })
    sl_out_df = do.call("rbind", sl_out )

    return(sl_out_df)
  })
  pl_out_df = do.call("rbind", pl_out)
  return(pl_out_df)
})
sl_out_df = do.call("rbind", sl_out)




################################################################################

setwd("/Users/tperez/Library/CloudStorage/OneDrive-ConservationInternationalFoundation/Desktop/CI_git_projects/PPC/Final_Data")
#write results:
date=Sys.Date()

#Number of regeneratin trees by plot
Restored_by_plot = paste(paste("Restored_by_plot", date, sep="_"), "csv", sep=".")
write.csv(Ntrees_regen_plot, Restored_by_plot)

#Number of regenerating trees by site
Restored_by_site = paste(paste("Restored_by_site", date, sep="_"), "csv", sep=".")
write.csv(Ntrees_regen_site, Restored_by_site)

#Number of regeneratin trees by plot
Regeneration_by_plot = paste(paste("Regeneration_by_plot", date, sep="_"), "csv", sep=".")
write.csv(Ntrees_natregen_plot, Regeneration_by_plot)

#Number of regenerating trees by site
Regeneration_by_site = paste(paste("Regeneration_by_site", date, sep="_"), "csv", sep=".")
write.csv(Ntrees_natregen_site, Regeneration_by_site)

#Survival of planted trees:
Survival_data = paste(paste("Survival_by_site", date, sep="_"), "csv", sep=".")
write.csv(sl_out_df, Survival_data)



