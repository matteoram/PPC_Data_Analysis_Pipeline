#--------------------------------------------------------------------------------
#Project: Priceless Planet Coalition
#Author: Timothy Perez
#Date Updated: May 31, 2023
#Input: The most recent "tree_data_PPC_Data_XXXX-XX_XX.csv", and the most recent 
#"Taxonomy_updates_XXXX-XX-XX.csv" if the latter exists.
#Outputs: The final file is: tree_data_taxonomy_corrections_XXXX-XX-XX, and updated
#Taxonomy_updates_XXXX-XX-XX file is produced. 

#Description: This code is part 2 of the analysis pipeline for key indicators for the Priceless 
#Planet Coalition. This code takes all of the species names entered in Kobo and attempts
#to resolve taxonomic information and adds family names. This script requires user input
#to manually correct names or add correct family names when the functions from other libraries fail
# to retrieve the data.
#-----------------------------------------------------------------------
#PPC Taxonomic names corrections
library(taxize)
library(taxizedb)
library(plyr)


setwd("/Users/tperez/Library/CloudStorage/OneDrive-ConservationInternationalFoundation/Desktop/CI_git_projects/PPC/Raw_Data")

#Import the data most recent "tree_data_PPC_Data" data
PPC_files=file.info(list.files(getwd(), full.names = T))
recent_tree_data=PPC_files[grep("tree_data_PPC_Data", row.names(PPC_files)),]
recent_tree_data_file=row.names(recent_tree_data[which(recent_tree_data$mtime == max(recent_tree_data$mtime)),])
tree_data=read.csv(recent_tree_data_file) 

#remove column labeled "X"
tree_data = tree_data[,-which(colnames(tree_data)=="X")]
tree_data$tree_data.Species = trimws(tree_data$tree_data.Species)

#Remove species with NA names, to add them back later
na_sp_tree_data = tree_data[which(is.na(tree_data$tree_data.Species)),]
tree_data = tree_data[-which(is.na(tree_data$tree_data.Species)),]
unique_species_original = sort(unique(tree_data$tree_data.Species))
unique_species = data.frame(unique_species= unique_species_original)

#-----------------------------------------------------------------------
#Section 1 - import newest existing taxonomic corrections
corrected_tax_files=PPC_files[grep("Taxonomy_updates", row.names(PPC_files)),]
taxlist=row.names(corrected_tax_files[which(corrected_tax_files$mtime == max(corrected_tax_files$mtime)),])
full_tax_corrects=read.csv(taxlist) 
#Remove first column
full_tax_corrects = full_tax_corrects[,-1]
#-----------------------------------------------------------------------
#Get rows of uncorrected species names already in corrected species names
rows_to_remove=which(unique_species$unique_species %in% full_tax_corrects$submitted_name)

#The following script can take a long time to run, esp. for "NA" data. 
#Remove these rows from unique_species data
#If the number unique_species is greater than one after removing correct names
#there are still names that need to be corrected. Continue with script
#Note: This could be zero
if(length(unique_species[-rows_to_remove,])>0) {
  unique_species=unique_species[-rows_to_remove,]
  unique_species = data.frame(unique_species= unique_species)

  #-----------------------------------------------------------------------
  #Section 2:
  #Correct species names function:
  #Get corrected taxonomic names from taxize package
  #Input: Dataframe of the unique species names
  #Output: Dataframe of taxize outputs
  Correct_taxonomy_fun=function(unique_species){
  
  #Retrieve correct taxonomic names from tropics and ncbi databases.
  #Note: you may have to oibtain an API acess key to do this.
  print("1")
  result.long <-as.data.frame(gnr_resolve(sci=unique_species$unique_species, data_source_ids = c(165, 167), canonical=T)) #165= tropicos
  
  #Create a shortened version of results that selects highest score or first name return if scores are ambiguous:
  result.long=do.call("rbind", by(result.long, result.long["user_supplied_name"], FUN=function(x){
    out=x[which(x$score==max(x$score)),]
    if(length(out[,1])>1){
      out=out[1,]
      return(out)
    }else{
      return(out)  
    }
  }))
  
  #Merge original unique_species df back into output from gnr_resolve
  merged_taxon=merge(result.long, unique_species, by.x = "submitted_name", by.y="unique_species", all=T)
  
  #Extract any rows with NA's, which indicate gnr_resolve did not find taxonomic names
  if(length(which(is.na(merged_taxon$matched_name2)))>=1){
    need_names=merged_taxon[which(is.na(merged_taxon$matched_name2)),]
    #Function to prompt user to input the corrected names that gnr_resolve couldn not resolve
    name_edits=do.call("rbind", by(need_names, need_names["submitted_name"], FUN=function(x){
      if (length(x[,1]>=1)){
        print("No taxonomic ID found for:") 
        print(paste(x$submitted_name))
        matched_name2=readline("Enter correct name if known:")
        submitted_name=x$submitted_name
        return(data.frame(matched_name2, "submitted_name"=x$submitted_name))
      }else if(length(need_names[,1]<1)){
      }}))
    
    #Remove rows of uncorrected data
    merged_taxon=merged_taxon[-which(is.na(merged_taxon$matched_name2)),]
    
    #Merge new rows with corrected data
    method1_out=merge(merged_taxon, name_edits, all=T)
    return(method1_out)
  }else{
    method1_out=merged_taxon
    return(method1_out)    
  }
  print("2")
  
}
  method1_out=Correct_taxonomy_fun(unique_species)
  data_out1 = method1_out
  print("3")
  #-------------------------------------------------------------------------------
  #Section 3:
  #Get family names for corrected species
  #Method 1:
  #Taxize is effective for retrieving correct species names, but many family names 
  #seem to be missing from the present databases, and it may take too long.
  
  #Input: The output of section 2 -ie a data frame of correted and uncorrected species names
  #Output: A data frame of all uncorreted and corrected species names and their associated families
  #-------------------------------------------------------------------------------
  #Function to separate genus and specific epithet names
  #intermediate function step
  gen_spe_names_fun=function(x){do.call("rbind", lapply(x, FUN=function(x){
    Genus=strsplit(x, " ")[[1]][1]
    specific_epithet=strsplit(x, " ")[[1]][2]
    return(data.frame(Genus, specific_epithet))
  } ))}
  print("4")

  #Note 1: Function below will take some time as it requires user input.
  #Taxize package will search for family names, and user will have to provide input;
  #so, far this is pretty unecessary bc the function returns erroneous names that
  #the user will have to correct. So, just hit the ENTER button when prompted to select a
  #row number.

  #Note 2:Users will also be prompted to supply family names for any species where
  #taxize was unbale ot find one.
  get_family_names_function = function(method1_out){
  #check that entered names are valid
  method1_out=method1_out[order(method1_out$submitted_name),]
  #Name check passes
  #method1_out$submitted_name== unique_species$unique_species
  
  #Get family name using the tax_name function
  #Note: This is a LONG step, and require user input
  cat("If no options look correct, just hit enter.")
  ids = tax_name(method1_out$matched_name2, get = "family")
  
  #Add family ID's to data corrected by user
  method1_out$method1_family=ids$family
  
  #check that entered names are valid
  #method1_out$submitted_name== unique_species$unique_species
  #Name check passes
  
  
  method1_out$genus_name=gen_spe_names_fun(method1_out$matched_name2)$Genus
  
  #check that entered names are valid
  #method1_out$submitted_name== unique_species$unique_species
  #Name check passes
  
  #-----------------------------------------------------------------------
  #Method 2:
  #Use the taxize database to extract correct family names
  #-----------------------------------------------------------------------
  #library(taxizedb)
  #extract  ID's for species , then extract Family names for these species
  ids2 = name2taxid(method1_out$matched_name2, out_type="summary")
  
  #There are some duplicate names that get passed to this function
  #Extract family names from list of taxonomic ranks
  method2_out=do.call("rbind",lapply(classification(ids2$id), FUN=function(x){
    family_name=x[which(x$rank=='family'),]$name
    return(data.frame(method2_family=family_name))
  }
  ))
  
  #Add back names submitted to this function, which are method1_out$matched_name2
  method2_out$matched_name2=ids2$name
  
  #There are now 4 duplicated cells these rows seem to have errors in their family names, too.
  #Rows match and should be delete
  
  duprows=which(duplicated(method2_out$matched_name2))
  #duprows==grep("idae", method2_out$method2_family)
  if(length(duprows)<0){
    #drop undesirable duplicated rows
    method2_out=method2_out[-duprows,]
  }
  
  
  #merge the submitted_name and matched_name2 columns. 
  cols2extract=which(colnames(method1_out)%in% c("submitted_name", "matched_name2"))
  method2_out=merge(method2_out, method1_out[,cols2extract], by="matched_name2", all= T)
  #method2_out=method2_out[order(method2_out$submitted_name),]
  #method2_out$submitted_name== unique_species$unique_species
  #Name check passes
  
  
  #-----------------------------------------------------------------------
  #Combine the data from the different families
  #-----------------------------------------------------------------------
  #combine different metrics for getting families
  #str(method1_out) #species only
  
  merge1=merge(method1_out, method2_out, by=c("submitted_name", "matched_name2"),  all= T)
  
  #Function replace replace missing rows from column1 with values from column 2.
  coalesce_fun <- function(...) {
    Reduce(function(x, y) {
      i <- which(is.na(x))
      x[i] <- y[i]
      x},
      list(...))
  }
  
  #Coalesce family names
  merge1$family_data_out=coalesce_fun(merge1$method1_family, merge1$method2_family, "family")
  
  #remove unneeded columns  and duplicated cells
  merge1=merge1[,-which(colnames(merge1)=="method1_family" | colnames(merge1)=="method2_family")]
  
  #merge1=merge1[order(merge1$submitted_name),]
  #merge1$submitted_name==unique_species$unique_species
  #Passes name check
  
  #Extract any rows with NA's, which indicate gnr_resolve did not find taxonomic names
  family_name_checks=merge1[c(which(is.na(merge1$family_data_out)),grep("idae", merge1$family_data_out)),]
  
  if(length(family_name_checks[,1])>0){
    #Function to prompt user to input the corrected names that gnr_resolve could not resolve
    family_name_edits=as.data.frame(t(sapply(1:nrow(family_name_checks), FUN=function(x){
      df=family_name_checks[x,]
      if (length(df[,1]>=1)){
        print("Check the family informtion for:") 
        print(df) #
        matched_name2=readline("Enter correct family name if known:")
        df$family_data_out=matched_name2
        return(df)
      }else if(length(family_name_checks[,1]<1)){
        return(df)
      }})))
    
    #Remove rows of uncorrected data
    merge1=merge1[-c(which(is.na(merge1$family_data_out)),grep("idae", merge1$family_data_out)),]
    
    #Merge new rows with corrected data
    fam_out=rbind(merge1, family_name_edits)
    fam_out=as.data.frame(apply(fam_out, 2 ,FUN=function(x){do.call("rbind", x)}))
    return(fam_out)
  }else{
    fam_out=merge1
    return(fam_out)
  }
  #fam_out=fam_out[order(fam_out$submitted_name),]
  #fam_out$submitted_name==unique_species$unique_species
  #Passes name check
  
}

  #Get rows with "NA" entries bc this takes forever
  rows_with_NAs=which(data_out1$matched_name2=="NA")
  #Get families for species wth names
  fam_data_out = get_family_names_function(data_out1[-rows_with_NAs,])
  fam_data_out = unique(fam_data_out)
  #add rows of data that have NA species names back into the data with family names
  fam_data_out = rbind.fill( fam_data_out, data_out1[rows_with_NAs,])
  
  
  fam_data_out2 = rbind(fam_data_out, full_tax_corrects)
  #Name and save corrected taxonomy data
  date = Sys.Date()
  file_name = paste(paste("Taxonomy_updates", date, sep="_"), "csv", sep=".")
  write.csv(fam_data_out2, file_name)
  
#I need to add the corrected names back into the uncorrected names
}


PPC_files=file.info(list.files(getwd(), full.names = T))
corrected_tax_files=PPC_files[grep("Taxonomy_updates", row.names(PPC_files)),]
taxlist=row.names(corrected_tax_files[which(corrected_tax_files$mtime == max(corrected_tax_files$mtime)),])
full_tax_corrects=read.csv(taxlist) 
#Remove first column
full_tax_corrects = full_tax_corrects[,-1]

full_tax_corrects=unique(full_tax_corrects)
freq_df=as.data.frame(table(full_tax_corrects$submitted_name))
dup_name=freq_df[which(freq_df$Freq>1),]$Var1

#Add updated taxonomic infor back into main data, "tree_data" .csv
#Check that the data have similar names
setdiff(tree_data$tree_data.Species, full_tax_corrects$user_supplied_name)
setdiff( full_tax_corrects$user_supplied_name, tree_data$tree_data.Species)
setdiff(tree_data$tree_data.Species, full_tax_corrects$submitted_name)
setdiff(full_tax_corrects$submitted_name, tree_data$tree_data.Species)

length(tree_data$tree_data._index)
tree_data_taxonomy_editted=merge(tree_data, full_tax_corrects, by.x=c("tree_data.Species"), by.y=c("user_supplied_name"), all.x=T)
length(tree_data_taxonomy_editted$tree_data._index)
tree_data_taxonomy_editted=data.table::rbindlist(list(tree_data_taxonomy_editted, na_sp_tree_data), fill = TRUE)
date = Sys.Date()
tree_file_name = paste(paste("tree_data_taxonomy_corrections", date, sep="_"), "csv", sep=".")
setwd("/Users/tperez/Library/CloudStorage/OneDrive-ConservationInternationalFoundation/Desktop/CI_git_projects/PPC/Raw_Data")
write.csv(tree_data_taxonomy_editted, tree_file_name)



#Trash below
#lapply(1:length(dup_name), FUN=function(x){
#  dup_check = dup_name[]
#  rows_to_check=which(full_tax_corrects$submitted_name==dup_check)
#  full_tax_corrects[rows_to_check,]
#  print(whifull_tax_corrects$submitted_name==dup_check,])
#})
