# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# ===============
# PROJECT NAME: PPC Kobo Toolbox Data Access 
# ===============
# Description: Grab data from Kobo Toolbox API and manipulate
#
# ===============
# AUTHOR: Vivian Griffey
# Date created: 1/05/23
# Date updated:
# ===============
# load libraries
library(tidyverse)
library(httr)
library(jsonlite)

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

## set variables
kc_server_url<-"https://kf.kobotoolbox.org/" #https://kf.kobotoolbox.org/api/v2/assets/axhiGJQ8JzwkTYcgDTroJu/data.json
form_id <- "axhiGJQ8JzwkTYcgDTroJu"
url<-paste0(kc_server_url,"api/v2/assets/",form_id,"/data.json") #endpoint to use
#when these data are made private you will need token to access 
#token f8041726a8848144f1843520e0be4a8ae803ff41


# ------------------------------------------------------------------------

## access API
js <-GET(url) %>%
  prettify() %>%
  fromJSON() #will preserve all the possible column names in $results (and keep NA if Country isn't entered)
# js is scary

#we don't need the other parts of js for now. just get results
df <- js$results
head(df)
#looks like the nested/repeated answers are preserved as named lists in the columns with begin repeat


# ------------------------------------------------------------------------

## format data into something that is R-friendly/tidy
# big issue right now is that some columns are lists
# need to unnest those lists
# this method below works somewhat, not sure if it's 100% though

#these columns are the lists
cols_count <- c("Large/begin_repeat_uPU1AjRnH","Large2/begin_repeat_ztsNCjoPm",
                "Small3/begin_repeat_ELDOiv5Dr",  "SmallSitesControl/Large1/begin_repeat_YDvKUvA32",
                "SmallSitesControl/Small1/begin_repeat_Wq3dUfnDG","PlantedTrees3",
                "Large2/group_an2yk58", "Small3/group_ka7vj63", "_attachments", 
                "_geolocation","_tags","_notes")

## from https://stackoverflow.com/questions/71780575/struggling-to-unnest-tibble-dput-code-and-error-included-in-r
# make dummy df
df_copy <- df
#Overwrite relevant parts of the dataframe
df_copy[cols_count] <- apply(df_copy[cols_count],
                            c(1,2), # go into every cell 
                            function(x) length(# get length of
                              unlist(x, recursive = FALSE) # a tibble cell is 
                              # a list itself, therefore unlist first
                            )
)# Apply function over relevant cells

head(df_copy)

# ------------------------------------------------------------------------

## other methods of unnesting

# using unnest
test <-  df %>% #turns up 0 rows, probably due to error shown below
  unnest(`Large/begin_repeat_uPU1AjRnH`) %>%
  unnest(`Large2/begin_repeat_ztsNCjoPm`) %>%
  unnest(`Small3/begin_repeat_ELDOiv5Dr`) %>%
  unnest(`SmallSitesControl/Large1/begin_repeat_YDvKUvA32`) %>%
  unnest(`SmallSitesControl/Small1/begin_repeat_Wq3dUfnDG`) %>%
  unnest(PlantedTrees3) %>%
  unnest(`Large2/group_an2yk58`) %>%
  unnest(`Small3/group_ka7vj63`)
#unnest(`_attachments`) %>%
#unnest(`_geolocation`) %>%
#unnest(`_tags`) %>%
#unnest(`_notes`)


#test which of these columns fails
#first two work, error comes when adding in the third column
unnest_df <- unnest( 
  df , cols = c("Large/begin_repeat_uPU1AjRnH","Large2/begin_repeat_ztsNCjoPm",
                "Small3/begin_repeat_ELDOiv5Dr")) #throws Error in `fn()`:! In row 23, can't recycle input of size 2 to size 3.



# Tim suggestions
N <- 100
df <- tibble(a = 1:N, b = map2(1:N, 1:N, c))
base_foo <- cbind(df[,1],t(data.frame(df$b))) %>%
  as_tibble()

#this won't work since "nested" is a normal column, not a list
#but still useful for future reference!
# df <- data.frame(id = 1:4, nested = c("a, b, f", "c, d", "e", "e, f"))
# with(df, setNames(stack(setNames(strsplit(nested, ","), id))[2:1], names(df)))

