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
library(data.table)
library(purrr)
library(scales)
library(pryr)
library(qdap)

# GIS libraries
library(tidycensus)
library(tigris)
library(tmap)
library(sf)

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

# supress warnings
options(warn = -1)

# base url for clustermapping.us basic html API
base_url <- "http://54.83.53.228/data"

#============================================================================================#
#======================================== Regions ===========================================#
#============================================================================================#
invisible(cat("\tProcessing regions data...\n"))

# get regions data
# we will first get it as a list
regions_lst    <- jsonlite::fromJSON(paste0(base_url,"/region"), simplifyVector = FALSE)

# this is an unnamed list. We will extract the ids an assign them as names
# this will help us in parsing the list later on
list_ids <- sapply(regions_lst, function(x) x$id)
names(regions_lst) <- list_ids

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
# don't do the below transformations since these are actually character values which will be misses up 
# if we converth them into integers
# regions_dt[, region_state_code_t := as.integer(region_state_code_t)]
# regions_dt[, region_code_t := as.integer(region_code_t)]

# For the three countries in the list, the region_short_name_t is NA, we need 
# to correct that
regions_dt[region_type_t == "country", region_short_name_t := name_t]

# the character columns might have some extra spaces in them
# we need to remove these out
space_cols <- c("name_t", "region_short_name_t", "key_t")
regions_dt[, (space_cols) := lapply(.SD, str_squish), .SDcols = space_cols]

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

# apply function to our regions_dt data.table
f_dowle3(DT = regions_dt)

# put the reginos data in one list and save it
regions_data <- list(regions_lst = regions_lst, regions_dt = regions_dt)

# save data to RDS file
invisible(cat("\tSaving regions data...\n"))
saveRDS(object = regions_data, file = "./data/regions_data.Rds")

options(warn = 0)
#============================================================================================#
#=================================== End: Regions Data ======================================#
#============================================================================================#

#============================================================================================#
#======================================= Clusters ===========================================#
#============================================================================================#
invisible(cat("\tProcessing cluster data...\n"))
invisible(cat("\tGetting a list of clusters and their subclusters, related clusters etc. \n"))

# get a list of clusters and their subclusters, related clusters etc. 
clusters_list  <- jsonlite::fromJSON(txt = paste0(base_url,"/meta/clusters"), simplifyVector = FALSE)

clusters_ids   <- sapply(clusters_list, function(x) x$id)
clusters_codes <- as.integer(sapply(clusters_list, function(x) x$cluster_code_t))
clusters_names <- sapply(clusters_list, function(x) x$name_t)
clusters_key   <- sapply(clusters_list, function(x) x$key_t)

# put available clusters data in a data.table
# this is just a list of available clusters
clusters_avlbl <- data.table(clusters_ids, clusters_codes, clusters_key, clusters_names)

# the clusters_list object we got from the API is not a named list. To make it useful, we need
# to convert it into a named list. We have several options for the names but we'll use the 
# cluster keys as the names for the clusters
names(clusters_list) <- clusters_avlbl$clusters_key

# put the data in one list and save it
cluster_data <- list(clusters_list = clusters_list, clusters_avlbl = clusters_avlbl)

# save data to RDS file
invisible(cat("\tSaving cluster data...\n"))
saveRDS(object = cluster_data, file = "./data/cluster_data.Rds")
#============================================================================================#
#================================== End: Clusters Data ======================================#
#============================================================================================#

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

# not all regions types are available for querying, please 
# look at manual available: http://clustermapping.us/sites/default/files/files/page/ClusterMapping-API-Docs.pdf
# according to the manual the only available region types are:
region_types_avlbl <- c("country", "state", "economic", "msa", "county")

# subset the region_types data.table to only include the available regions and save it to a new object
region_types_avlbl <- region_types[variable %in% region_types_avlbl]

# get the meta data dictionary
meta_dict <- jsonlite::fromJSON(paste0(base_url,"/meta/dict"))

# put meta data in one list and save it
# we will also add the clusters_avlbl to the meta_data
meta_data <- list(years_avlbl = years_avlbl
                  , region_types = region_types
                  , region_types_avlbl = region_types_avlbl
                  , meta_dict = meta_dict
                  , clusters_avlbl = clusters_avlbl)

# save data to RDS file
invisible(cat("\tSaving meta data...\n"))
saveRDS(object = meta_data, file = "./data/meta_data.Rds")
#============================================================================================#
#==================================== End: Meta Data ========================================#
#============================================================================================#

#============================================================================================#
#======================================== GIS Data ==========================================#
#============================================================================================#
# we will be using the tidycensus and tigris packages to get GIS data related to US counties
# etc. 

# this is Dr. Aous Abdo's census API key. Please use it wisely
census_api_key(key = "00480fb480b6c6a5ebd4cfcb7afa6da946be92e8", install = TRUE)
readRenviron("~/.Renviron")

# Set the tigris_use_cache option. We want to cache the data so we don't end up downloading it
# everytime 
options(tigris_use_cache = TRUE)

# we want the data in simple feature format
# Get data from tigris as simple features
options(tigris_class = "sf")

# download county population along with the geometry/shape files for the counties
county_pop <- tidycensus::get_acs(geography = "county"
                                  , variables = c("population" = "B01003_001")
                                  , geometry = TRUE
                                  , keep_geo_vars = TRUE
                                  , shift_geo = TRUE)

# change state names to state abbreviations 
county_pop <- county_pop %>%
  mutate(NAME = qdap::mgsub(state.name, state.abb, county_pop$NAME))

# download county level data using tigris
counties <- tigris::counties(cb = TRUE)

# add regoin_code_t to match the regions_dt data.table
# this is simply the concatenation of state and county fp
counties <- counties %>% mutate(regoin_code_t = paste0(STATEFP, COUNTYFP))
#============================================================================================#
#===================================== End: GIS Data ========================================#
#============================================================================================#

