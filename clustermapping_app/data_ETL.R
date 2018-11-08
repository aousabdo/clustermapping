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

# some of the regions are custom regions which I don't see us using 
# anytime soon. Thus, I will be excluding these from the analysis
# this will also help with the quereying, since the region_type_t 
# is not a numeric
regions.sub <- regions.dt[region_type_t != "custom"]
regions.sub[, region_code_t := as.integer(region_code_t)]
setkey(regions.sub, region_code_t)

# one major issue we have with this dataset is the fact that the data frame produced has
# columns that are lists. We need to convert those into their own columns

# get names of columns that are lists
list.cols <- regions.sub %>% select_if(is.list) %>% names()

# use cSplit to split these on the comma, in new cols
regions.sub <- cSplit(indt = regions.sub, splitCols = list.cols, sep = ",") 
