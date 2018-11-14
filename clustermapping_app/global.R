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

# supress warnings
options(warn = -1)

# base url for clustermapping.us basic html API
base_url <- "http://54.83.53.228/data"

# load the data 
meta_data     <- readRDS("./data/meta_data.Rds")
regions_data  <- readRDS("./data/regions_data.Rds")
clusters_data <- readRDS("./data/cluster_data.Rds")

regions_dt <- regions_data$regions_dt

# get a list of regoins available, exclude the three countries, the us, mexico and canada
region_names <- regions_dt[region_type_t != "country", unique(region_short_name_t)]

clusters_list   <- clusters_data$clusters_list
clusters_avlble <- clusters_data$clusters_avlble

#========================================================================================#
#=================================== get_strong_clusters ================================#
#========================================================================================#

get_strong_clusters <- function(region_name = NULL
                                , regions_dt = NULL
                                , year_selected = 2016
                                , meta_data = NULL){
  # given a region name, a regions_dt, and a year, this function will return a 
  # data.table object with the strong clusters for that region

  # region_name: valid name for a region
  # regions_dt: a valid data.table regions object. This data.table is produced with the 
  # data_ETL.R code
  # year_selected: a valid year in the form YYYY. Valid values can be found in the 
  # meta_data list which is produced by the data_ETL.R code
  
  if(regions_dt[region_short_name_t == region_name, .N] == 0) stop("\tRegion selected is not valid...\n")
  
  # filter the regions data.table for the selected region
  selected_region <- regions_dt[region_short_name_t == region_name
                                , .(region_type_t, region_code_t, name_t, region_short_name_t)]
  
  # in some cases, the region_code is between 1 and 9, in this case
  # if we call the api with this integer we'll get an error since the 
  # api expects a 01, 02, etc. so we need to fix this
  region_code <- selected_region[, region_code_t]
  if(nchar(region_code) == 1) region_code = paste0("0", region_code)
  
  # get general data about the selected region
  query <- paste(base_url, "region", selected_region[, region_type_t]
                 , region_code, year_selected, sep = "/")
  
  selected_region <- jsonlite::fromJSON(query)
  if(length(selected_region) == 0) stop("\tSorry, the region selected has no data from the source...\n")
  
  # convert to data.table object
  selected_region <- as.data.table(selected_region)
  
  # get a list of strong clusters for selected region
  strong_clusters <- selected_region$strong_clusters
  
  # prepare strong-clusters data
  cluster_name <- sapply(strong_clusters, function(x) x$name)
  cluster_code <- sapply(strong_clusters, function(x) x$code)
  cluster_key  <- sapply(strong_clusters, function(x) x$key)
  cluster_pos  <- sapply(strong_clusters, function(x) x$pos)
  
  # bind strong-cluster data in one data.table object
  strong_clusters <- cbind(cluster_name, cluster_code, cluster_key, cluster_pos)
  strong_clusters <- as.data.table(strong_clusters)
  
  # some data transformations
  strong_clusters[, cluster_code := as.integer(cluster_code)]
  
  # the cluster position vector starts at 0, correct that since in R we start at 1
  strong_clusters[, cluster_pos  := as.integer(cluster_pos) + 1] 
  strong_clusters[, cluster_name := factor(cluster_name)]
  strong_clusters[, cluster_key  := factor(cluster_key)]
  
  strong_clusters <- unique(strong_clusters)
  
  # set data.table key
  setkey(strong_clusters, cluster_pos)
  
  return(strong_clusters)
}
#========================================================================================#
#================================ End: get_strong_clusters ==============================#
#========================================================================================#

#========================================================================================#
#===================================== get_cluster_data =================================#
#========================================================================================#
get_cluster_data <- function(strong_clusters_dt = NULL
                             , strong_clusters_rows_selected = NULL){
  # this function will query the cluster list and cluster data.table
  # for cluster data given a selected cluster
  
  # get the cluster selected by the user
  selected_cluster <- strong_clusters_dt[strong_clusters_rows_selected]
  
  # now we query the cluster list data for the cluster selected by the user
  selected_cluster_data <- clusters_list[[selected_cluster$cluster_key]] 
  
  # let's get the number of sub_clusters as well as the number of related_clusters
  # to the selected cluster by the user
  N_sub_clusters <- selected_cluster_data$sub_clusters %>% length()
  N_rel_clusters <- selected_cluster_data$related_clusters %>% length()
  
  if(N_sub_clusters > 0){
    invisible(cat("\t This cluster has", N_sub_clusters, "subclusters\n"))
    
    sub_clusters_dt <- selected_cluster_data$sub_clusters 
    sub_clusters_dt <- do.call(rbind, sub_clusters_dt) %>% as.data.table()
    
    # add the parent cluster name
    sub_clusters_dt[, parent_cluster_name := selected_cluster[, cluster_name]]
    
    # rearrange column orders to have the parent cluster as the first column
    setcolorder(sub_clusters_dt, c(2, 1))
    setnames(sub_clusters_dt, c("parent_cluster_name", "sub_cluster_name"))
    
  } else{
    invisible(cat("\t This cluster has no subclusters\n"))  
    sub_clusters_dt <- data.table()
  }
  
  if(N_rel_clusters > 0){ 
    invisible(cat("\t This cluster has", N_rel_clusters, "related clusters\n"))
    
    # let's get the data for the related clusters
    related_clusters <- selected_cluster_data$related_clusters
    
    # convert it to a data.table
    do.call(rbind, related_clusters) %>% as.data.table() -> related_clusters_dt
    
    # do some data cleaning etc. 
    # list of numerical columns
    numeric_cols <- c("cluster_code_t", grep("related", names(related_clusters_dt), v = TRUE))
    
    # convert all columns from lists to characters
    related_clusters_dt[, names(related_clusters_dt) := lapply(.SD, as.character)]
    
    # convert the only character column to factor
    related_clusters_dt[, cluster_name_t := factor(cluster_name_t)]
    
    # now convert all numerical cols to numeric
    related_clusters_dt[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
    
    # add the parent cluster name
    related_clusters_dt[, parent_cluster_name := selected_cluster[, cluster_name]]
    
    # rearrange column orders to have the parent cluster as the first column
    setcolorder(related_clusters_dt, c(ncol(related_clusters_dt), 2, 1, 3:(ncol(related_clusters_dt)-1)))
    
    related_clusters_dt <<- related_clusters_dt[, .(parent_cluster_name, cluster_name_t, related_percentage)]
  } else{
    invisible(cat("\t This cluster has no related clusters\n"))
    related_clusters_dt <- data.table()
  }
  
  # get a list of industries, naics, by year
  industries_2012 <- selected_cluster_data$naics_2012 %>% 
    list2df(col1 = "industry", col2 = "naics") %>%
    as.data.table() %>% 
    mutate(year = 2012)
  
  industries_2007 <- selected_cluster_data$naics_2007 %>% 
    list2df(col1 = "industry", col2 = "naics") %>%
    as.data.table() %>% 
    mutate(year = 2007)
  
  industries_2002 <- selected_cluster_data$naics_2002 %>% 
    list2df(col1 = "industry", col2 = "naics") %>%
    as.data.table() %>% 
    mutate(year = 2002)
  
  industries_1997 <- selected_cluster_data$naics_1997 %>% 
    list2df(col1 = "industry", col2 = "naics") %>%
    as.data.table() %>% 
    mutate(year = 1997)
  
  # put them all in one table
  industries <- do.call("rbind", list(industries_2012, industries_2007, industries_2002, industries_1997)) %>%
    as.data.table()
  
  # add the parent cluster name
  industries[, parent_cluster_name := selected_cluster[, cluster_name]]
  
  # rearrange columns
  setcolorder(industries, c(ncol(industries), 1:(ncol(industries)-1)))
  
  # put it all in one list
  cluster_data_list <- list(related_clusters_dt = related_clusters_dt
                            , sub_clusters_dt = sub_clusters_dt
                            , industries = industries)
  return(cluster_data_list)
}
#========================================================================================#
#================================= End: get_cluster_data ================================#
#========================================================================================#

#========================================================================================#
#=================================== build_network_viz ==================================#
#========================================================================================#
build_network_viz <- function(cluster_data = NULL){
  # this function creates several network visulaizations
  # let's make some pretty plots. These will be network and sankey diagrams
  # to create these we need to create nodes and edges as follows
  
  # the data we will be using to create these plots is the cluster_data()$related_clusters_dt
  related_clusters <- cluster_data$related_clusters_dt
  
  # create nodes: nodes should simply be the cluster names we have 
  nodes <- c(as.character(related_clusters[1 , parent_cluster_name])
             , as.character(related_clusters$cluster_name_t)) 
  
  # convert to data.table
  nodes <- data.table(cluster_name = nodes) 
  
  # add id column, we'll use this column in our directional plots
  nodes[, id := 1:.N]
  
  # since the function we are using requires the links and nodes to start at 0 we have to 
  # make sure we do that
  nodes_d3 <- mutate(nodes, id = id - 1)
  
  edges <- data.table(from = 1
                      , to = nodes_d3$id
                      , weight = c(0, scale_fun(related_clusters$related_avg)))
  
  # since the function we are using requires the links and nodes to start at 0 we have to 
  # make sure we do that
  edges_d3 <- mutate(edges, from = from - 1) %>% as.data.table()
  
  # remove the first row which is the parent cluster with itself
  edges_d3 <- edges_d3[2:.N]
  
  # make network graph
  forceNetwork_viz <- forceNetwork(
    Links = edges_d3, Nodes = nodes_d3,  
    Source = "from", Target = "to",      # so the network is directed.
    NodeID = "cluster_name", Group = "id", Value = "weight", 
    opacity = 1, fontSize = 8, zoom = TRUE, opacityNoHover = T, legend = F
  )
  
  # create a sankey network diagram
  sankeyNetwork_viz <- sankeyNetwork(
    Links = edges_d3, Nodes = nodes_d3,
    Source = "from", Target = "to",
    NodeID = "cluster_name", Value = "weight",
    fontSize = 16)
  
  # let's make some network graphs with the vizNetwork library
  # to use the visNetwork package we need to have columns with specifict names
  nodes_d3$label <- nodes_d3$cluster_name
  edges_d3 <- mutate(edges_d3, width = weight)
  
  # make some network graphs
  vizNetwork_basic <- visNetwork(nodes_d3, edges_d3) %>%
    visLayout(randomSeed = 12)
  
  # put all visualizations in one list 
  viz_list <- list(forceNetwork_viz = forceNetwork_viz
                   , sankeyNetwork_viz = sankeyNetwork_viz
                   , vizNetwork_basic = vizNetwork_basic) 
  
  # return the visulaizations list
  return(viz_list)
}
#========================================================================================#
#=================================== build_network_viz ==================================#
#========================================================================================#

#========================================================================================#
#======================================= scale_fun ======================================#
#========================================================================================#
scale_fun <- function(x = NULL){
  # make sure we scale the weight of the edges to reflect the strength of the relationship
  round(x/min(x), 1)
  }
#========================================================================================#
#==================================== End: scale_fun ====================================#
#========================================================================================#