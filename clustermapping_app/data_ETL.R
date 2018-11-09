library(jsonlite)
library(rjson)
library(tidyverse)
library(data.table)
library(splitstackshape)
library(plotly)
library(lubridate)
library(qdapTools) # only to use the list2df, which I could use melt instead
library(tesseract) # only to do OCR on the clustermapping.us api documentation manual
library(networkD3)
library(visNetwork)
library(igraph)
library(stringr)

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#####  data_ETL.R                                                                        #####
#####  This script reads data from the basic HTML API provided by clustermapping.us      #####
#####  It then process this data, apply transformations, data cleaning, etc. and saves   #####
#####  the data to Rds files for better performance.                                     #####
#####  Once we move into prodcution, this script will be modified to store that data     #####  
#####  into a  postgresql database which we will host on AWS                             #####
#####  Author: Dr. Aous Abdo <aous.abdo@asetpartners.com                                 #####
##############################################################################################
##############################################################################################
##############################################################################################

# base url for clustermapping.us basic html API
base_url <- "http://54.83.53.228/data"

#============================================================================================#
#======================================= Meta Data ==========================================#
#============================================================================================#
invisible(cat("\tProcessing meta data...\n"))

# get a list of available years
years_avlbl <- jsonlite::fromJSON(paste0(base_url,"/meta/years"))
years_avlbl <- as.integer(years_avlbl)

# get a list of available region types
region_types <- jsonlite::fromJSON(paste0(base_url,"/meta/regions"))

# convert to data.table object
region_types <- as.data.table(melt(region_types))

# set names
setnames(region_types, c("value", "variable"))

# convert character column containing levels to factor
region_types[, variable := factor(variable)]

# get the meta data dictionary
meta_dict <- jsonlite::fromJSON(paste0(base_url,"/meta/dict"))

# put meta data in one list and save it
meta_data <- list(years_avlbl = years_avlbl, region_types = region_types, meta_dict = meta_dict)

# save data to RDS file
invisible(cat("\tSaving meta data...\n"))
saveRDS(object = meta_data, file = "./data/meta_data.Rds")

#============================================================================================#
#==================================== End: Meta Data ========================================#
#============================================================================================#

#============================================================================================#
#======================================== Regions ===========================================#
#============================================================================================#
invisible(cat("\tProcessing data about available regions...\n"))

# get regions data
# we will first get it as a list
regions_lst    <- jsonlite::fromJSON(paste0(base_url,"/region"), simplifyVector = FALSE)

# and now we'll get it as a data.frame
regions_dt <- jsonlite::fromJSON(paste0(base_url,"/region"), simplifyVector = TRUE)
regions_dt <- as.data.table(regions_dt)

# the region data we get from the api has a region type that is custom
# these regiosn don't interest us since they are custom regions made
# by the site users. We nee to filter these out from the list and the data frame

# filter the list
regions_lst <- regions_lst[lapply(regions_lst, function(x) x[["region_type_t"]]) != "custom"]

# now filter the data.table
regions_dt <- regions_dt[region_type_t != "custom"]

# some data transformations
regions_dt[, region_type_t := factor(region_type_t)]
regions_dt[, region_state_code_t := as.integer(region_state_code_t)]
regions_dt[, region_code_t := as.integer(region_code_t)]

# one major issue we have with this dataset is the fact that the data frame produced has
# columns that are lists. We need to convert those into their own columns
# we will deal with each column separately since this is a tideous process

# the first column we will be dealing with is the state_codes_txt column
# let's first split this list column
regions_dt <- cSplit(indt = regions_dt, splitCols = "state_codes_txt", sep = ",", drop = TRUE)

# now we need to build a function to clean the column names
clean_col_names <- function(x){
  gsub("\"|\\)|c\\(|\\(", "", x)
}

# the process above gave us new columns that are mostly NAs
# we need to modify these columns with the function above to get 
# rid of the extra characters so we can convert the columns into 
# integers

# get a list of cols to modify
cols_mod <- grep("state_codes_txt_", names(regions_dt), value = TRUE)

# modify columns
regions_dt[, (cols_mod) := lapply(.SD, clean_col_names), .SDcols = cols_mod]
regions_dt[, (cols_mod) := lapply(.SD, as.integer), .SDcols = cols_mod]

# now we will do the same for the other list column which is regions_txt
regions_dt <- cSplit(indt = regions_dt, splitCols = "regions_txt", sep = ",", drop = TRUE)

# get a list of cols to modify
cols_mod <- grep("regions_txt_", names(regions_dt), value = TRUE)

# modify columns
regions_dt[, (cols_mod) := lapply(.SD, clean_col_names), .SDcols = cols_mod]

# after the transformations some columns are left with a "NULL" character entries
# we need to fix these with the following function
f_dowle3 = function(DT) {
  if(!is.data.table(DT)) stop("\tObject supplied is not a data.table object...\n")
  # this function converts all "NULL" entires in a data.table into NAs
  for (j in seq_len(ncol(DT)))
    set(x = DT
        , i = which(DT[[j]] == "NULL")
        , j = j
        , value = NA)
}


f_dowle3(DT = regions_dt)