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
years <- jsonlite::fromJSON(paste0(base_url,"/meta/years"))
years <- as.integer(years)

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
meta_data <- list(years = years, region_types = region_types, meta_dict = meta_dict)

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
regions    <- jsonlite::fromJSON(paste0(base_url,"/region"))
regions.dt <- as.data.table(regions)

# some data transformations
regions.dt[, region_type_t := factor(region_type_t)]
regions.dt[, region_state_code_t := as.integer(region_state_code_t)]

# one major issue we have with this dataset is the fact that the data frame produced has
# columns that are lists. We need to convert those into their own columns
# we will deal with each column separately since this is a tideous process

# the first column we will be dealing with is the state_codes_txt column
# let's first split this list column
regions.dt <- cSplit(indt = regions.dt, splitCols = "state_codes_txt", sep = ",", drop = FALSE)

# now we need to build a function to clean the column names
clean_col_names <- function(x){
  gsub("\"|\\)|c|\\(", "", x)
}

# the process above gave us new columns that are mostly NAs
# we need to modify these columns with the function above to get 
# rid of the extra characters so we can convert the columns into 
# integers
cols_mod <- grep("state_codes_txt_", names(regions.dt), value = TRUE)

regions.dt[, (cols_mod) := lapply(.SD, clean_col_names), .SDcols = cols_mod]
regions.dt[, (cols_mod) := lapply(.SD, as.integer), .SDcols = cols_mod]

# now get rid of the original state_codes_txt column
regions.dt[, state_codes_txt := NULL]
