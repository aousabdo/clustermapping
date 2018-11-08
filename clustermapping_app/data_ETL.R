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
# get a list of available years
years <- jsonlite::fromJSON(paste0(base_url,"/meta/years"))
years <- as.integer(years)

# get a list of available region types
region_types <- jsonlite::fromJSON(paste0(base_url,"/meta/regions"))
region_types <- as.data.table(melt(region_types))
setnames(region_types, c("value", "variable"))

# get the meta data dictionary
meta_dict <- jsonlite::fromJSON(paste0(base_url,"/meta/dict"))
