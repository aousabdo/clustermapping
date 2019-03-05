#######################################################################################
#######################################################################################
#######################################################################################
######### clustermapping is a web application built using R. The app queries  #########
######### data from the clustermapping.us site.                               #########
######### Dr. Aous Abdo <aous.abdo@gmail.com>                                 ######### 
#######################################################################################
#######################################################################################
#######################################################################################

library(jsonlite)
library(rjson)
library(tidyverse)
library(data.table)
library(splitstackshape)
library(plotly)
library(lubridate)
library(qdapTools) # only to use the list2df, which we could replace with melt instead
library(tesseract) # only to do OCR on the clustermapping.us api documentation manual
library(networkD3)
library(visNetwork)
library(igraph)
library(stringr)
library(data.table)
library(htmlwidgets)
library(shiny)
library(DT)
library(shiny)
library(purrr)
library(scales)
library(pryr)
library(rrricanes)

# GIS libraries
library(tidycensus)
library(tigris)
library(tmap)
library(sf)

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

clusters_list  <- clusters_data$clusters_list
clusters_avlbl <- clusters_data$clusters_avlbl

#========================================================================================#
#=================================== get_strong_clusters ================================#
#========================================================================================#
get_strong_clusters <- function(region_name = NULL
                                , regions_dt = NULL
                                , year_selected = 2016
                                , meta_data_list = meta_dta
                                , base_url = "http://54.83.53.228/data"
                                , verbose = FALSE
                                , clusters_list_input = clusters_list
                                , by_column = "cluster_code"){
  # given a region name, a regions_dt, and a year, this function will return a 
  # data.table object with the strong clusters for that region
  
  # region_name: valid name for a region
  # regions_dt: a valid data.table regions object. This data.table is produced with the 
  # data_ETL.R code
  # year_selected: a valid year in the form YYYY. Valid values can be found in the 
  # meta_data list which is produced by the data_ETL.R code
  
  if(regions_dt[region_short_name_t == region_name, .N] == 0) stop("\tRegion selected is not valid...\n")
  
  # filter the regions data.table for the selected region
  selected_region_info <- regions_dt[region_short_name_t == region_name
                                     , .(region_type_t, region_code_t, name_t, region_short_name_t)]
  
  # in some cases, the region_code is between 1 and 9, in this case
  # if we call the api with this integer we'll get an error since the 
  # api expects a 01, 02, etc. so we need to fix this
  region_code <- selected_region_info[, region_code_t]
  if(nchar(region_code) == 1) region_code = paste0("0", region_code)
  
  # get general data about the selected region
  query <- paste(base_url, "region", selected_region_info[, region_type_t]
                 , region_code, year_selected, sep = "/")
  
  if(verbose){
    invisible(cat("\tGetting Strong Clusters...\n"))
    invisible(cat("\tRunning the following query: ", query, "\n"))
  }
  
  selected_region_lst <- jsonlite::fromJSON(query)
  
  # in some cases, the region selected would have no strong clusters, in that case
  # we'll just return the region clusters
  
  # convert to data.table object
  selected_region <- as.data.table(selected_region_lst)
  
  # get a list of strong clusters for selected region
  strong_clusters <- selected_region$strong_clusters
  
  if(length(strong_clusters) != 0){    
    if(verbose)invisible(cat("\tStrong clusters found\n"))
    is_strong_cluster <- TRUE
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
    # strong_clusters[, cluster_name := factor(cluster_name)]
    strong_clusters[, cluster_key  := factor(cluster_key)]
    
    strong_clusters <- unique(strong_clusters)
    
    # let's add the number of employments for strong cluster
    strong_cluster_codes <- strong_clusters$cluster_code
    
    # the selected_region table has the employments for all clusters in the region
    # get employments for the strong clusters
    tmp <- selected_region_lst[grep("cluster_([1-9][0-9]{0,2})_emp_tl", names(selected_region_lst), value = TRUE)] %>% 
      list2df(col1 = "emp_tl", "cluster_code") %>% as.data.table()
    tmp[, cluster_code := as.integer(gsub("^cluster_|_emp_tl", "", cluster_code))]
    
    strong_clusters <- left_join(x = strong_clusters, y = tmp, by = "cluster_code") %>% as.data.table()
    
    # set data.table key
    setkey(strong_clusters, cluster_pos)
  }else{
    if(verbose)invisible(cat("\tNo strong clusters found, returning all clusters\n"))
    is_strong_cluster <- FALSE
    # if there are no strong clusters for the selected region, then just 
    # return the region's clusters
    region_clusters <- get_region_clusters(cluster = "all"
                                           , region_name = region_name
                                           , region_type = selected_region_info[, region_type_t]
                                           , regions_dt = regions_dt 
                                           , year_selected = 2016
                                           , cluster_selected = "all"
                                           , meta_data_list = meta_data_list
                                           , base_url = base_url
                                           , verbose = verbose)
    strong_clusters <- region_clusters_to_strong_clusters(region_clusters = region_clusters
                                                          , meta_data_list = meta_data_list)
  }
  
  # add short_names to clusters
  strong_clusters <- add_short_names(clusters_dt = strong_clusters
                                     , clusters_list_input = clusters_list_input
                                     , by_column = by_column)
  
  return(list(strong_clusters = strong_clusters, is_strong_cluster = is_strong_cluster))
}
#========================================================================================#
#================================ End: get_strong_clusters ==============================#
#========================================================================================#

#========================================================================================#
#===================================== get_cluster_data =================================#
#========================================================================================#
get_cluster_data <- function(strong_clusters_dt = NULL
                             , strong_clusters_rows_selected = NULL
                             , clusters_list_input = clusters_list
                             , verbose = FALSE
                             , clean_related_clusters = FALSE){
  # this function will query the cluster list and cluster data.table
  # for cluster data given a selected cluster
  
  # get the cluster selected by the user
  selected_cluster <- strong_clusters_dt[strong_clusters_rows_selected]
  
  # now we query the cluster list data for the cluster selected by the user
  selected_cluster_key <- selected_cluster$cluster_key %>% as.character() 
  # The as.character is crucial, otherwise we'll get the wrong related clusters
  
  selected_cluster_data <- clusters_list_input[[selected_cluster_key]] 
  selected_cluster_data_out <<- copy(selected_cluster_data)
  
  # let's get the number of sub_clusters as well as the number of related_clusters
  # to the selected cluster by the user
  N_sub_clusters <- selected_cluster_data$sub_clusters %>% length()
  N_rel_clusters <- selected_cluster_data$related_clusters %>% length()
  
  selected_cluster_name <- selected_cluster[, cluster_name] %>% as.character()
  
  if(N_sub_clusters > 0){
    if(verbose) invisible(cat("\tThe cluster", selected_cluster_name, "has", N_sub_clusters, "subclusters\n"))
    
    sub_clusters_dt <- selected_cluster_data$sub_clusters 
    sub_clusters_dt <- do.call(rbind, sub_clusters_dt) %>% as.data.table()
    
    # add the parent cluster name
    sub_clusters_dt[, parent_cluster_name := selected_cluster[, cluster_name]]
    
    # rearrange column orders to have the parent cluster as the first column
    setcolorder(sub_clusters_dt, c(2, 1))
    setnames(sub_clusters_dt, c("parent_cluster_name", "sub_cluster_name"))
    
  } else{
    if(verbose) invisible(cat("\tThe cluster", selected_cluster_name, "has no subclusters\n"))  
    sub_clusters_dt <- data.table()
  }
  
  if(N_rel_clusters > 0){ 
    if(verbose) invisible(cat("\tThe cluster", selected_cluster_name, "has", N_rel_clusters, "related clusters\n"))
    
    # let's get the data for the related clusters
    related_clusters <- selected_cluster_data$related_clusters
    related_clusters_out <<- related_clusters
    
    # convert it to a data.table
    related_clusters_dt <- do.call(rbind.data.frame, related_clusters) %>% as.data.table()
    
    # this will resulte in all columns be factors, let's fix that
    integer_cols <- c("cluster_code_t", "related_90", "related_i20_90", "related_i20_90_min", "related_percentage")
    numeric_cols <- c("related_avg", "related_min")
    
    related_clusters_dt[, cluster_name_t := as.character(cluster_name_t)]
    related_clusters_dt[, (integer_cols) := lapply(.SD, function(x) as.integer(levels(x))[x]), .SDcols = integer_cols]
    related_clusters_dt[, (numeric_cols) := lapply(.SD, function(x) as.numeric(levels(x))[x]), .SDcols = numeric_cols]
    
    if(clean_related_clusters){
      # we will only keep tightly related clusters, those for which related_i20_90_min == 1
      related_clusters_dt <- related_clusters_dt[related_i20_90_min == 1]
    }
    
    # add the parent cluster name and code
    related_clusters_dt[, parent_cluster_name := selected_cluster[, cluster_name]]
    related_clusters_dt[, parent_cluster_code := selected_cluster[, cluster_code]]
    related_clusters_dt[, parent_cluster_short_name := selected_cluster[, cluster_short_name]]
    
    # rearrange column orders to have the parent cluster as the first column
    setcolorder(related_clusters_dt, c((ncol(related_clusters_dt_out)-2):ncol(related_clusters_dt_out)
                                       , 2, 1, 3:(ncol(related_clusters_dt)-3)))
    
    # related_clusters_dt_out <<- related_clusters_dt[, .(parent_cluster_name, cluster_name_t, related_percentage)]
  } else{
    if(verbose) invisible(cat("\tThe cluster", selected_cluster_name, "has no related clusters\n"))
    
    # we will still populate the realted_clusters data.table with the parent cluster info
    # and NAs for related clusters data
    related_clusters_dt <- data.table(parent_cluster_name = selected_cluster[, cluster_name])
    related_clusters_dt[, parent_cluster_code := selected_cluster[, cluster_code]]
    related_clusters_dt[, parent_cluster_short_name := selected_cluster[, cluster_short_name]]
    
    related_clusters_dt[, cluster_name_t := NA]
    related_clusters_dt[, cluster_code_t := NA]
    related_clusters_dt[, related_90 := NA]
    related_clusters_dt[, related_i20_90 := NA]
    related_clusters_dt[, related_i20_90_min := NA]
    related_clusters_dt[, related_percentage := NA]
    related_clusters_dt[, related_avg := NA]
    related_clusters_dt[, related_min := NA]
  }
  
  related_clusters_dt_out <<- copy(related_clusters_dt)
  
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
  
  sub_clusters_dt_out <<- copy(sub_clusters_dt)
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
  
  edges_d3_out <<- edges_d3
  nodes_d3_out <<- nodes_d3
  
  # make network graph
  forceNetwork_viz <- forceNetwork(
    Links = edges_d3, Nodes = nodes_d3,  
    Source = "from", Target = "to",      # so the network is directed.
    NodeID = "cluster_name", Group = "id", Value = "weight", 
    opacity = 1, fontSize = 8, zoom = TRUE, opacityNoHover = T, legend = F
  ) %>%
    htmlwidgets::prependContent(htmltools::tags$h1("Related Clusters"))
  
  # create a sankey network diagram
  sankeyNetwork_viz <- sankeyNetwork(
    Links = edges_d3, Nodes = nodes_d3,
    Source = "from", Target = "to",
    NodeID = "cluster_name", Value = "weight",
    fontSize = 16) %>%
    htmlwidgets::prependContent(htmltools::tags$h1("Related Clusters"))
  
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

#========================================================================================#
#=================================== get_region_clusters ================================#
#========================================================================================#
get_region_clusters <- function(cluster = NULL
                                , region_name = NULL
                                , region_type = NULL
                                , regions_dt = NULL
                                , year_selected = 2016
                                , cluster_selected = "all"
                                , meta_data_list = meta_data
                                , base_url = "http://54.83.53.228/data"
                                , verbose = FALSE){
  
  # return cluster-level data including by range of years
  
  # function arguments
  # cluster: a valid cluster code, 6 for example, 
  #          a valid cluster key such as distribution_and_electronic_commerce
  #          "traded" for traded clusters
  #          "local" for local clusters
  #          "all" for all clusters
  # region_name: valid name for a region, this can also be "all" to retrieve data for all
  # regions given a valid region type
  # region_type: a valid region type. valid region types are listed in the meta_data_list$region_types_avlbl
  # regions_dt: a valid data.table regions object. This data.table is produced with the
  #             data_ETL.R code
  # year_selected: a valid year in the form YYYY. Valid values can be found in the
  #                meta_data_list which is produced by the data_ETL.R code.
  #           Valid values:
  #                "all":      to retrieve all years
  #                "earliest": to retrieve data for the earliest year
  #                "latest"  : to retrieve data for the latest year
  #                YYYY: an integer representing a valid year, currently 1998 to 2016
  #                YYYY,YYYY,YYYY: year range, example: 2009,2010,2011,2012
  # Example: to return data available for the Apparel cluster in Wisconsin across all years
  #          /data/cluster/3/all/state/55
  # Example: to return data available for the Apparel cluster across all states from 2009:2011
  #          /data/cluster/3/2009,2010,2011/state/all
  
  #-------------------------------------------------------------------------------------#
  #-------------------------------- perform args checks --------------------------------#
  #-------------------------------------------------------------------------------------#
  
  # check the base_url
  if(is.null(base_url)){
    stop("\tbase_url can't be NULL, you might wanna try setting it to: http://54.83.53.228/data")
  }
  
  # we need the meta_data_list list to do some checks, so make sure we have that object
  if(is.null(meta_data_list)) stop("\tYou must supply a valid meta_data_list object...\n")
  
  # now check the cluster selected
  if(is.null(cluster)){
    stop("\tYou must provide a valid cluster name or a cluster key or just select \"all\" for all clusters, \"traded\" for traded clusters, or \"local\" for local clusters\n")
  }else if(length(cluster) > 1){
    stop("\tSelect only one cluster...\n")
  }else if(is.numeric(cluster)){ 
    if(!(cluster %in% meta_data_list$clusters_avlbl$clusters_codes)){
      stop("\tcluster selected doesn't exist...\n")
    }else{selected_cluster_code <- cluster}
  }else if(is.character(cluster)){
    clusters_names_avlbl <- meta_data_list$clusters_avlbl$clusters_names
    clusters_names_avlbl <- c(clusters_names_avlbl, "traded", "local", "all")
    if(sum(cluster == clusters_names_avlbl) == 0){
      stop("\tcluster selected doesn't exist...\n")
    }else{selected_cluster_code <- clusters_avlbl[clusters_names == cluster, clusters_codes]}
  }
  
  # if the region_name given is "all", then we don't need the regions_dt data.table object 
  # to check information about the region since the user is requesting all regions
  # if the region_name is not equal to "all" then we need to have the regions_dt data.table
  # to get the info about the region
  if(region_name == "all"){
    if(is.null(region_type)){
      stop("\tYou must supply a region type...\n")
    } else if(!(region_type %in% meta_data_list[["region_types_avlbl"]][, variable])){
      stop("\tRegion type selected is not vaild...\n")
    }
  }else{
    # check the regions_dt data.table
    if(is.null(regions_dt)) stop("\tPlease supply a region_dt data.table...\n")
    if(!is.data.table(regions_dt)) stop("\tregions_dt must be a data.table object...\n")
    
    # make sure the regions_name is valid
    if(regions_dt[region_short_name_t == region_name, .N] == 0) stop("\tRegion selected is not valid...\n")
    
    # make sure the region_type is valid
    if(!is.null(region_type)){
      if(!(region_type %in% meta_data_list[["region_types_avlbl"]][, variable])) stop("\tRegion type selected is not vaild...\n")
    }else{region_type <- regions_dt[region_short_name_t == region_name, region_type_t]}
  }
  # now check the year selected
  if(is.numeric(year_selected)){
    if(sum(year_selected %in% meta_data_list$years_avlbl) != length(year_selected))
      stop("\tYear selected is outside of bound...\n")
  }else if(is.character(year_selected)){
    if(!(year_selected %in% c("latest", "earliest", "all")))
      stop("\tYear selected is outside of bound...\n")
    else if(year_selected == "latest"){year_selected <- max(meta_data_list[["years_avlbl"]])}
    else if(year_selected == "earliest"){year_selected <- min(meta_data_list[["years_avlbl"]])}
    else if(year_selected == "all"){year_selected <- as.integer(meta_data_list[["years_avlbl"]])}
  } else {stop("\tYear selected is invalid")}
  
  #-------------------------------------------------------------------------------------#
  #------------------------------ End: perform args checks -----------------------------#
  #-------------------------------------------------------------------------------------#
  
  if(region_name == "all"){
    region_code <- "all"
    selected_region <- data.table(region_type_t = region_type
                                  , region_code_t = region_code
                                  , name_t = region_name
                                  , region_short_name_t = region_name)
  }else{
    # filter the regions data.table for the selected region
    selected_region <- regions_dt[region_short_name_t == region_name
                                  , .(region_type_t, region_code_t, name_t, region_short_name_t)]
    
    # in some cases, the region_code is betewen 1 and 9, in this case
    # if we call the api with this integer we'll get an error since the
    # api expects a 01, 02, etc. so we need to fix this
    region_code <- selected_region[, region_code_t]
    if(nchar(region_code) == 1) region_code = paste0("0", region_code) 
  }
  
  # if mulitple years are given then collaps witn a comma
  if(length(year_selected) > 1){
    year_selected <- paste(year_selected, collapse = ",")
  }
  
  if(cluster == "all") selected_cluster_code <- "all"
  # now that we have the selected region, we can get the region's clusters
  # let's build our query
  query <- paste(base_url, "cluster", selected_cluster_code
                 , year_selected, selected_region[, region_type_t], region_code, sep = "/")
  
  if(verbose){
    invisible(cat("\n\tGetting Region Clusters, this includes related clusters, subclusters etc.\n"))
    invisible(cat("\tRunning the following query: ", query, "\n"))
  }
  
  region_cluster_dt <- jsonlite::fromJSON(query, simplifyVector = TRUE)
  region_cluster_dt <- as.data.table(region_cluster_dt)
  
  # some final data transformations
  region_cluster_dt[, year_t := as.integer(year_t)]
  region_cluster_dt[, cluster_code_t := as.integer(cluster_code_t)]
  
  # add a column specifiying cluster type
  region_cluster_dt[, cluster_type := factor(ifelse(traded_b, "traded", "local"))]
  
  return(region_cluster_dt)
}
#========================================================================================#
#================================ End: get_region_clusters ==============================#
#========================================================================================#

#========================================================================================#
#================================ build_cluster_plots ===================================#
#========================================================================================#
build_cluster_plots <- function(region_clusters_dt = NULL
                                , N_top_clusters = 10
                                , year_selected = 2016
                                , traded_only = TRUE
                                , start_year = 1998
                                , end_year   = 2016
                                , meta_data_list = meta_data
                                , use_short_names = TRUE){
  
  # chech the years given
  if(!(year_selected %in% meta_data_list[["years_avlbl"]])) stop("\tYear selected is out of range...\n")
  if(!(start_year%in% meta_data_list[["years_avlbl"]])) stop("\tstart year is out of range...\n")
  if(!(end_year %in% meta_data_list[["years_avlbl"]])) stop("\tend year is out of range...\n")
  if(start_year >= end_year) stop("\tStart year must be less than end year...\n")
  
  # make a copy of the data.table
  region_cluster <- copy(region_clusters_dt)
  
  # get rid of clusters with no employment
  region_cluster <- region_cluster[emp_tl > 0]
  
  # set proper orders
  setorder(region_cluster,  -year_t, -emp_tl)
  
  # top clusters by year
  if(traded_only){
    top_clusters <- region_cluster[traded_b == TRUE,  head(.SD, N_top_clusters), by = year_t][, .(cluster_name_t, emp_tl, year_t, emp_tl_rank_i)]
  }else{
    top_clusters <- region_cluster[,  head(.SD, N_top_clusters), by = year_t][, .(cluster_name_t, emp_tl, year_t, emp_tl_rank_i)]
  }
  
  p1 <- region_cluster[year_t == year_selected] %>% 
    group_by(cluster_type) %>%
    summarise(count = n()) %>%
    plot_ly(labels = ~ cluster_type, values = ~ count) %>%
    add_pie(hole = 0.6
            # , domain = list(x = c(0, 0.5), y = c(0, 0.9))
    )
  
  p1 <- p1 %>%
    layout(title = paste0("\nTraded vs. Local Clusters, ", year_selected),  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  p1 <- hide_legend(p1)
  
  col_to_plot <- "cluster_name_t"
  if(use_short_names){
    col_to_plot <- "cluster_short_name"
    col_to_plot_2 <- "cluster_short_name_2"
  }
  
  # add us rank to cluster name
  region_cluster[, cluster_name_t_2 := paste0(cluster_name_t, ": ", emp_tl_rank_i)]
  region_cluster[, cluster_short_name_2 := paste0(cluster_short_name, ": ", emp_tl_rank_i)]
  
  p2 <- plot_ly(data = region_cluster[year_t == year_selected & traded_b == TRUE][1:N_top_clusters, ] 
                , x = ~emp_tl
                , y = ~reorder(get(col_to_plot_2), emp_tl)
                , type = 'bar'
                , orientation = "h"
                , source = "barplot"
                , hoverinfo = "text"
                , text = ~paste(reorder(get(col_to_plot), emp_tl), "<br>"
                                , "US rank:", emp_tl_rank_i, "<br>"
                                , "Employment:", emp_tl)
                , color = I("steelblue")) 
  
  p2 <- p2 %>%
    layout(title = paste0("\n Employment by Traded Cluster, ", year_selected, "\n"),  
           xaxis = list(title = paste0("Employment, ", year_selected), showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE),
           margin = list(l = 350, r = 50, b = 50, t = 50, pad = 4))
  
  p2 <- hide_legend(p2)
  
  p2_out <<- p2
  
  p3 <- plot_ly(data = region_cluster[year_t == year_selected & traded_b == TRUE] 
                , x = ~ private_wage_tf
                , y = ~reorder(get(col_to_plot), private_wage_tf)
                , type = 'bar'
                , hoverinfo = "text"
                , text = ~paste(reorder(get(col_to_plot), private_wage_tf), "<br>"
                                , paste("Avg. Wages,", year_t, ":"), paste0("$", round(private_wage_tf, 0)))
                , orientation = "h") %>%
    layout(title = paste0("\nWages by Traded Cluster, ", year_selected),  
           xaxis = list(title = paste0("Wages, ", year_selected), showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           margin = list(l = 350, r = 50, b = 50, t = 50, pad = 4))
  
  # get job creation by cluster by year
  job_creation <- region_clusters_dt[(year_t == start_year | year_t == end_year) & traded_b] %>%   
    group_by_at(col_to_plot) %>%
    summarise(job_creation_numbers = emp_tl[year_t == end_year] - emp_tl[year_t == start_year]) %>%
    arrange(desc(job_creation_numbers)) %>%
    mutate(change = ifelse(job_creation_numbers >= 0, "Increased", "Decreased"))
  
  p4 <- job_creation %>% 
    ggplot(aes(x = reorder(get(col_to_plot), -job_creation_numbers)
               , y = job_creation_numbers
               , fill = change
               , text = paste0(get(col_to_plot), "\nJob Creation:", job_creation_numbers))) 
  
  p4 <- p4 + geom_bar(stat = "identity") + scale_fill_manual(values=c("red", "blue")) +
    theme_minimal() + theme(legend.position="none", axis.text.x = element_text(angle = 80, hjust = 1)) + 
    ylab(paste('Job Creation', start_year, "to", end_year, sep = " ")) + xlab("Cluster")
  
  gg <- ggplotly(p4, tooltip = c("text")) 
  gg <- gg %>% layout(title = paste0("Job Creation by Traded Cluster, ", start_year, "-", end_year),
                      xaxis = list(title = "Clusters", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
                      yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
                      margin = list(l = 50, r = 50, b = 350, t = 50, pad = 4))
  
  list_to_return <- list(top_clusters = top_clusters
                         , donut_chart = p1
                         , cluster_emp = p2
                         , cluster_wages = p3
                         , cluster_job_creation = gg)
}
#========================================================================================#
#============================= End: build_cluster_plots =================================#
#========================================================================================#

#========================================================================================#
#========================= region_clusters_to_strong_clusters ===========================#
#========================================================================================#
region_clusters_to_strong_clusters <- function(region_clusters = NULL
                                               , meta_data_list = meta_data){
  # convert a region_clusters table to a strong cluster table
  
  # for the position of the clusters we'll just order the clusters by 
  # the number of employments
  
  # copy the region cluster data to be the strong clusters object
  strong_clusters <- region_clusters
  
  # subset the data columns needed
  strong_clusters <- strong_clusters[, .(cluster_name_t, cluster_code_t, emp_tl)]
  
  setnames(strong_clusters, c("cluster_name", "cluster_code", "emp_tl"))
  
  # get the clusters available data since we'll need to merge it with our table to get additional data columns
  clusters_avlbl <- meta_data_list$clusters_avlbl
  setnames(clusters_avlbl, c("cluster_id", "cluster_code", "cluster_key", "cluster_name"))
  
  # add the two missing columns using the meta_data object
  strong_clusters <- merge(strong_clusters, clusters_avlbl, by = c("cluster_code", "cluster_name"))
  
  # filter out clusters with no employments
  strong_clusters <- strong_clusters[emp_tl > 0]
  
  # set key and reorder by employment numbers
  setkey(strong_clusters, emp_tl)
  setorder(strong_clusters, -emp_tl)
  
  # add cluster_pos
  strong_clusters[, cluster_pos := 1:.N]
  
  # only keep columns that we need
  strong_clusters <- strong_clusters[, .(cluster_name, cluster_code, cluster_key, cluster_pos, emp_tl)]
  
  return(strong_clusters)
}
#========================================================================================#
#======================= End: region_clusters_to_strong_clusters ========================#
#========================================================================================#

#========================================================================================#
#=================================== get_circle_coord ===================================#
#========================================================================================#
# function to calculate coords of a circle
get_circle_coord <- function(center, radius, res = 1000) {
  # we will calculate the coords of a circle using the parametric equations of a circle
  # take a look at https://en.wikipedia.org/wiki/Circle#Equations
  
  # calcualte theta, the parametric angle
  th <- seq(0, 2*pi, len = res)
  
  # coordinates of the circle
  DT <- data.table(x = center[1] + radius*cos(th), y = center[2] + radius*sin(th))
  return(DT)
}
#========================================================================================#
#================================= End: get_circle_coord ================================#
#========================================================================================#

#========================================================================================#
#================================ build_horiz_bubble_chart ==============================#
#========================================================================================#
build_horiz_bubble_chart <- function(data = NULL
                                     , center_aligned = TRUE
                                     , fill = "seagreen"
                                     , scale_value = 0.4){
  # make sure the data given is a data.table 
  if(!is.data.table(data)){
    tmp <- as.data.table(data)
  }else{ tmp <- copy(data) }
  
  # check to make sure the data.table has the needed clumns
  if(sum(c("cat_var", "value", "pos") %in% names(tmp), na.rm = TRUE) != 3) 
    stop("\t I am expecting three columns with the following names: \"cat_var\", \"value\", \"pos\"")
  
  # order data by pos column
  setkey(tmp, pos)
  setorder(tmp, pos)
  
  # get maxiumum value
  max       <- max(tmp$value)
  
  # get number of bubbles
  n.bubbles <- nrow(tmp)
  
  # scale
  scale_value     <- scale_value/sum(sqrt(tmp$value))
  
  # calculate scaled centers and radii of bubbles           
  radii <- scale_value*sqrt(tmp$value)
  ctr.x <- cumsum(c(radii[1], head(radii, -1) + tail(radii, -1) + .01))
  
  # starting (larger) bubbles
  if(center_aligned){
    gg.1   <- do.call(rbind, lapply(1:n.bubbles, function(i) cbind(group = i, get_circle_coord(c(ctr.x[i], radii[1]), radii[i]))))
    text.1 <- data.frame(x = ctr.x, y = radii[1], label = paste(tmp$cat_var, tmp$value, sep = "\n"))
  }else{
    gg.1   <- do.call(rbind, lapply(1:n.bubbles, function(i) cbind(group = i, get_circle_coord(c(ctr.x[i], radii[i]), radii[i]))))
    text.1 <- data.frame(x = ctr.x, y = radii, label = paste(tmp$cat_var, tmp$value, sep = "\n"))
  }
  
  # make the plot
  p <- ggplot() + geom_polygon(data = gg.1, aes(x, y, group = group), fill = fill) +
    geom_path(data = gg.1, aes(x, y, group = group), color = fill) +
    geom_text(data = text.1, aes(x, y, label = label), col = "white") +
    labs(x = "", y = "") + scale_y_continuous(limits = c(-0.1, 2.5*scale_value*sqrt(max(tmp$value)))) +
    coord_fixed() +
    theme(axis.text = element_blank()
          , axis.ticks = element_blank()
          , panel.grid = element_blank()
          , panel.background = element_blank())
  
  return(p)
  
}
#========================================================================================#
#============================= End: build_horiz_bubble_chart ============================#
#========================================================================================#

#========================================================================================#
#==================================== add_short_names ===================================#
#========================================================================================#
add_short_names <- function(clusters_dt = NULL
                            , clusters_list_input = clusters_list
                            , by_column = "cluster_code"
                            , short_col_name = NULL){
  # this function takes a data.table which contains some clusters and adds the 
  # short name of these clusters
  if(!is.data.table(clusters_dt)) tmp <- as.data.table(clusters_dt)
  else tmp <- copy(clusters_dt)
  
  if(is.null(by_column)) stop("\tI need a column name to merge by...")
  # if(sum(by_column %in% c("cluster_code", "cluster_key")) == 0) stop("\tby_column can be one of: \"cluster_code\" or \"cluster_key\" ")
  
  # first we will take the clusters_list and get a data.table out of it
  # we will get the by_column plus the cluster_short_name columns
  
  if(by_column %like% "cluster_code"){
    by_column_tmp  <- sapply(clusters_list_input, function(x) x$cluster_code_t) %>% unname() %>% as.integer()
  }else if(by_column == "cluster_key"){
    by_column_tmp  <- sapply(clusters_list_input, function(x) x$key_t) %>% unname()
  }
  
  # get the short names
  cluster_short_name    <- sapply(clusters_list_input, function(x) x$short_name_t) %>% unname()
  
  # some clusters have a shorter short name
  cluster_short_name_2  <- sapply(clusters_list_input, function(x) x$short_name2_t) %>% unname()
  
  # now put the two short names in one data table for merging
  cluster_short_name_tmp <- cbind(cluster_short_name, cluster_short_name_2) %>% as.data.table()
  
  cluster_short_name_tmp[, short_name := ifelse(cluster_short_name_2 == "0" | 
                                                  cluster_short_name_2 == "Hospitality"
                                                , cluster_short_name, cluster_short_name_2)]
  
  cluster_short_name <- cluster_short_name_tmp[, short_name]
  
  # bind strong-cluster data in one data.table object
  tmp2 <- cbind(by_column_tmp, cluster_short_name)
  tmp2 <- as.data.table(tmp2)
  
  if(by_column %like% "cluster_code"){
    tmp2[, by_column_tmp := as.integer(by_column_tmp)]
  }
  setnames(tmp2, c(by_column, "cluster_short_name"))
  
  # now we will perform the merge
  error_catch <- try(tmp <- merge(tmp, tmp2, by = by_column), TRUE)
  if(class(error_catch) == "try-error"){
    cat("Caught an error in the column names. will try adding a \"_t\" to column names.\n")
    try(tmp <- merge(tmp, tmp2, by.x = paste0(by_column, "_t"), by.y = by_column, TRUE))
  }
  
  
  # Some of the short names contain the & sign, this will cause us problems and we need to fix it
  tmp[, cluster_short_name := gsub("\\&", "and", cluster_short_name)]
  
  tmp_out <<- copy(tmp)
  # change name of short column that we just added if user elected to do so
  if(!is.null(short_col_name)){
    setnames(tmp, names(tmp), c(names(tmp)[1:(ncol(tmp) - 1)], short_col_name))
  }
  
  return(tmp)
}
#========================================================================================#
#================================= End: add_short_names =================================#
#========================================================================================#

#========================================================================================#
#=============================== get_all_related_clusters ===============================#
#========================================================================================#
get_all_related_clusters <- function(clusters_list_input = clusters_list
                                     , verbose = FALSE){
  # this function takes as an input the clusters_list list and 
  # returns a data.table which contains all of the related clusters
  
  if(is.null(clusters_list_input)) stop("\tI need a valid clusters_list_input to work with. Quitting...\n")
  
  # first we need to filter our clusters, only keep traded clusters
  traded <- sapply(clusters_list_input, function(x) x$traded_b)
  clusters_list_input <- clusters_list_input[traded]
  
  # get a list that only contains all of the related clusters
  all_related_clusters <- sapply(clusters_list_input, function(x) x$related_clusters)
  
  # declare an empty data.table to hold the related clusters data
  tmpdt <- data.table()
  
  # this empty data.table will be used to hold rows of data for 
  # clusters with no related clusters
  empty_dt <- data.table(parent_cluster_name = NA
                         , parent_cluster_code = NA
                         , cluster_code_t = NA
                         , cluster_name_t = NA
                         , related_90 = NA
                         , related_i20_90 = NA
                         , related_i20_90_min = NA 
                         , related_percentage = NA
                         , related_avg = NA
                         , related_min = NA)
  
  # loop over the nested lists, convert most inner list to data.table and append
  for(i in 1:length(all_related_clusters)){
    # i goes over the number of items in our clusters_list_input
    k <- length(all_related_clusters[[i]])
    # k is the number of items in a given item, so it is a sub of the sub of the list
    if(k > 0){
      # if k = 0, that list item has no related clusters
      for(j in 1:k){
        # now that we are down to the sub item of the sub item, we need to get 
        # data for that related cluster
        tmp <- all_related_clusters[[i]][[j]] %>% as.data.table()
        
        # now add the name of the parent cluster
        tmp[, parent_cluster_name := names(all_related_clusters)[i]]
        
        # and now add the cluster code for the parent cluster
        tmp[, parent_cluster_code := clusters_list_input[[parent_cluster_name]]$cluster_code_t]
        
        # now bind to the tmpdt data.table object
        tmpdt <- rbind(tmpdt, tmp)
      }
    }else{
      # if no related cluster then tell the user if verbose is true
      if(verbose) invisible(cat("\tcluster", names(all_related_clusters)[i], "has no related clusters\n"))
      
      # add name of cluster to the empty data.table
      empty_dt[, parent_cluster_name := names(all_related_clusters)[i]]
      
      # and now add the cluster code for the parent cluster
      empty_dt[, parent_cluster_code := clusters_list_input[[parent_cluster_name]]$cluster_code_t]
      
      # rbind the empty_dt to the tmpdt
      # note that empty_dt will reset everytime
      tmpdt <- rbind(tmpdt, empty_dt)
    }
  }
  
  # fix the order to have the parent cluster as the first column
  setcolorder(tmpdt, c(length(tmpdt), length(tmpdt) - 1, 1:(length(tmpdt)-2)))
  
  # fix column types
  integer_cols <- c("parent_cluster_code", "cluster_code_t", "related_90", "related_i20_90", "related_i20_90_min", "related_percentage")
  numeric_cols <- c("related_avg", "related_min")
  
  tmpdt[, (integer_cols) := lapply(.SD, as.integer), .SDcols = integer_cols]
  tmpdt[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
  
  # clean the names
  setnames(tmpdt, old = names(tmpdt), new = c(names(tmpdt)[1:2], "related_cluster_code", "related_cluster_name", names(tmpdt)[5:ncol(tmpdt)]))
  
  # and we are done!
  return(tmpdt)
}
#========================================================================================#
#============================= End: get_all_related_clusters ============================#
#========================================================================================#

#========================================================================================#
#==================================== build_graph_vis ===================================#
#========================================================================================#
build_graph_vis <- function(related_cluster_input = NULL
                            , clusters_avlbl_input = clusters_avlbl
                            , apply_filters = TRUE
                            , add_dashes = FALSE
                            , navigation_controls = FALSE
                            , add_node_position = TRUE
                            , remvoe_empty_nodes = FALSE
                            , selected_cluster = 3
                            , visManipulation = FALSE
                            , cluster_network_positions_file = "./data/cluster_network_positions.Rds"
                            , add_custom_edges = TRUE
                            , custom_edges_file = "./data/cluster_edges.Rds"
                            , region_name = NULL
                            , year_selected = 2016){
  # this function takes as an input a data.table with the cluster linkeages 
  # and produces a visNetwork plot
  
  if(is.null(related_cluster_input)) stop("\tI need a related_cluster_input data.table to work with...\n")
  
  # build the edges of the graph
  edges <- copy(related_cluster_input)
  
  # start by adding the from and to columns
  edges[, from := parent_cluster_code]
  edges[, to := related_cluster_code]
  
  # add a column which would tell us if there is a duplicate. 
  # this netowrk is a one-mode network, so we need to get rid of the duplicate edges
  edges[, dup_col := ifelse(from < to, paste(from, to, sep = "_"), paste(to, from, sep = "_"))]
  
  # before we apply the unique function, we need to get out the NA's rows, we might just need them later 
  # temporarily store the NAs rows in their own table
  tmp <- edges[is.na(to)]
  
  # now get the unique non-NA rows
  # the unique command gets rid of the duplicate entris
  edges <- unique(edges[complete.cases(edges)], by = "dup_col")
  
  # and now bind them together
  edges <- rbind(edges, tmp)
  
  # and set the table keys
  setkeyv(edges, c("from", "to")) 
  
  # now we need to take care of the connections, some of them will be dashed, this depends on the strength of the connection
  edges[, sum_relatedness := related_90 + related_i20_90 + related_i20_90_min]
  
  if(apply_filters){
    edges <- edges[sum_relatedness == 3]
  }
  
  # now we'll deal with the dashes and edge colors. According to clustermapping.us:
  # dark-color edge connection: if the BCR >= 95th percentile & RI >= 20%
  # light-color edge connection: if the BCR in the 90-94 percentile & RI >= 20%
  edges[, width := ifelse(related_percentage >= 95, 3, 1)]
  
  # add dashes column by default, and default it to FALSE
  edges[, dashes := FALSE]
  
  if(add_dashes) edges[, dashes := ifelse(related_percentage >= 95, FALSE, TRUE)]
  edges[, color := ifelse(related_percentage >= 95, "steelblue", "steelblue")]
  
  # add custom edges 
  if(add_custom_edges){
    # check to make sure the custom edges files exists
    if(!file.exists(custom_edges_file)) stop("\tCustom edges file doesn't exist, qutting...\n")
    else{edges_data <- readRDS(custom_edges_file)}
    
    # the custom edges file contain all edges
    # we only need the from and to since we'll merge this file with the edges table we already have
    from <- sapply(edges_data, function(x) x$from)
    to   <- sapply(edges_data, function(x) x$to)
    
    edges_tmp <- data.table(from, to)
    
    # now merge the two tables on the two columns, from and to, and keep all rows with all = TRUE
    edges_tmp <- merge(edges, edges_tmp, by = c("from", "to"), all = TRUE)
    
    # now we need to set the dashes to TRUE for the new edges
    
    edges_tmp[is.na(dashes), dashes := TRUE]
    edges_tmp[is.na(parent_cluster_name), width := 1]
    # edges_tmp[is.na(parent_cluster_name), color := "grey30"]
    
    # now copy the new edges table to be used in our plots
    edges <- copy(edges_tmp)
  }
  
  # work on the nodes now
  IDs <- related_cluster_input[, unique(parent_cluster_code)]
  
  nodes <- data.table(cluster_code = IDs
                      , cluster_name = clusters_avlbl_input[clusters_codes %in% IDs, clusters_names])
  
  nodes <- add_short_names(clusters_dt = nodes
                           , by_column = "cluster_code"
                           , short_col_name = "label")
  
  setnames(nodes, c("id", names(nodes)[2:ncol(nodes)]))
  nodes[ , label := gsub(" \\& ", " ", label)]
  nodes[ , label := gsub(" and ", "\n", label)]
  nodes[ , label := gsub(" ", "\n", label)]
  
  nodes[, title := paste0("<p><b>", label,"</b></p>") ]
  nodes[, shadows := TRUE]
  nodes[, shape := 'circle']
  
  #=====================================================================================#
  #=====================================================================================#
  #=====================================================================================#
  # some necessary custom changes to make sure the nodes are esthetically clean and clear
  
  # apply the get_longest_string function to get the number of characters in the label column row-wise
  nodes[ , longest_nchar := sapply(label, function(x) get_longest_string(x))]
  
  # Now get the nuber of n_spaces, which is just the difference between the maximum number of characters
  # for any row in label (this happens to be for connections) and the number of characters in the longest 
  # element in a row label
  nodes[ , n_spaces := max(longest_nchar) - longest_nchar]
  
  # we would like this number to be even, so we add 1 for odd cases of this n_spaces
  nodes[(n_spaces %% 2) != 0, n_spaces := n_spaces + 1]
  
  # some custom, manual, interventions :)
  # in somce cases we need to increase n_spaces to have a better looking node
  
  # n_spaces_8 means we will be adding 8 extra spaces to n_spaces, etc. 
  n_spaces_inc_8 <- "^IT|^Texti"
  nodes[label %like% n_spaces_inc_8, n_spaces := n_spaces + 8]
  
  n_spaces_inc_4 <- "^Distri|^Light|^Water|^Business|^Furn|^Jewel|^Education|^Wood|^Appar|^Medica|^Leather|^Finan|^Hosp|^Market|^Footw|Metal\nMining|^Vulc|^Live|^Agric|^Toba|^Insu|^Production|^Trailer|^Upstream|^Food|^Paper|^Appa|^Performing|^Video|^Constru|^Recrea"
  nodes[label %like% n_spaces_inc_4, n_spaces := n_spaces + 4]
  
  n_spaces_inc_2 <- "^Automo|Nonmetal\nMining|^Transpo"
  nodes[label %like% n_spaces_inc_2, n_spaces := n_spaces + 2]
  
  n_spaces_inc_6 <- "^Music|^Fish|Oil|Coal|^Plastic|^Electric|^Printi|^Fores"
  nodes[label %like% n_spaces_inc_6, n_spaces := n_spaces + 6]
  
  # now we need to split the label column in two since there are labels with 
  # two names, i.e. Perfomring Arts 
  nodes[, c("label_1", "label_2") := tstrsplit(label, "\n", fixed = T)]
  
  # now we need to add the spaces
  nodes[nchar(label_1) >= nchar(label_2) | is.na(label_2), label_1 := paste0(strrep("_", n_spaces/2), label_1, strrep("_", n_spaces/2))]
  nodes[nchar(label_2) > nchar(label_1), label_2 := paste0(strrep("_", n_spaces/2), label_2, strrep("_", n_spaces/2))]
  
  # and now we merge the labels again to give us the label column
  nodes[ , label := ifelse(test = is.na(label_2), yes = label_1, no = paste(label_1, label_2, sep = " "))]
  
  # we need to convert the spaces to new lines
  nodes[ , label := gsub(" ", "\n", label)]
  
  # and now convert the under scores to spaces
  nodes[ , label := gsub("_", " ", label)]
  
  #==================================================================================================#
  #==================================================================================================#
  #==================================================================================================#
  # start working on the network visualization
  if(add_node_position){
    # add x and y coordinates
    if(file.exists(cluster_network_positions_file)){
      # first read the file containing the positions of nodes
      coords <- readRDS(cluster_network_positions_file)
      
      # add positions
      nodes[, x := coords$x]
      nodes[, y := coords$y]
    }
  }
  
  # remove nodes with no connections
  if(remvoe_empty_nodes){
    nodes_w_edges <- c(edges[, from], edges[, to]) %>% unique()
    nodes <- nodes[id %in% nodes_w_edges]
  }
  # if(!(selected_cluster %in% edges[, from])) {stop("Cluster selected is not in our database...\n")}
  
  # selected_cluster <- edges[sample(unique(from), 1), from]
  selected_nodes <- c(selected_cluster, edges[from == selected_cluster, to])
  
  p <- visNetwork(nodes, edges, height = "700px", width = "1000px"
                  , main = "Cluster Linkages"
                  , submain = paste(region_name, year_selected, sep = ", ")) %>% 
    visNodes(size = 50
             , physics = FALSE
             , fixed = TRUE) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 1, labelOnly = FALSE, algorithm="hierarchical")
               , nodesIdSelection = list(enabled = TRUE, selected=selected_cluster)
               , collapse = TRUE)
  
  if(navigation_controls){
    p <- p %>% visInteraction(dragNodes = TRUE
                              , dragView = TRUE
                              , zoomView = TRUE
                              , hoverConnectedEdges = TRUE
                              , navigationButtons = TRUE)
  }
  
  if(visManipulation){
    p <- p %>% visOptions(manipulation = TRUE)
  }
  
  nodes[, font.size := 50]
  
  edges_out <<- copy(edges)
  nodes_out <<- copy(nodes)
  
  return(list(edges = edges, nodes = nodes, visGraph = p))
}
#========================================================================================#
#================================= End: build_graph_vis =================================#
#========================================================================================#

#========================================================================================#
#================================== get_longest_string ==================================#
#========================================================================================#
get_longest_string <- function(x){
  # Function to get the longest string in a character string vector
  
  # split string vector on new line or space
  data <- unlist(str_split(x, "\n| ", n = Inf))
  
  # get the number of characters for all vector elements
  index <- sapply(data, nchar)
  
  # keep the element with longest nchar
  data <- data[which.max(index)]
  
  # return the number of characters in the longest element
  return(as.integer(nchar(data)))
}
#========================================================================================#
#================================ End: get_longest_string ===============================#
#========================================================================================#

#========================================================================================#
#===================================== metro_tracts =====================================#
#========================================================================================#
# this function was taken from https://walkerke.github.io/2017/05/tigris-metros/
metro_tracts <- function(metro_name) {
  
  # First, identify which states intersect the metro area using the
  # `states` function in tigris
  st <- states(cb = TRUE)
  cb <- core_based_statistical_areas(cb = TRUE)
  metro <- filter(cb, grepl(metro_name, NAME))
  
  stcodes <- st[metro,]$STATEFP
  
  # Then, fetch the tracts, using rbind_tigris if there is more
  # than one state
  if (length(stcodes) > 1) {
    tr <- rbind_tigris(
      map(stcodes, function(x) {
        tracts(x, cb = TRUE)
      })
    )
  } else {
    tr <- tracts(x, cb = TRUE)
  }
  
  # Now, find out which tracts are within the metro area
  within <- st_within(tr, metro)
  
  within_lgl <- map_lgl(within, function(x) {
    if (length(x) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  # Finally, subset and return the output
  output <- tr[within_lgl,]
  
  return(output)
}

#========================================================================================#
#================================== End: metro_tracts ===================================#
#========================================================================================#

#========================================================================================#
#=================================== pull_storm_data ====================================#
#========================================================================================#
pull_storm_data <- function(storm_name = NULL
                            , storm_df = NULL
                            , products_vec = c("discus", "fstadv")){
  # simple function to get storms data from the National Hurricane Center databases
  
  # function arguments
  # storm_name: exact storm name as in the National Hurricane Center database. Exampels are Hurricane Irma, 
  #             Hurricane Harvey, etc.
  # storm_df: a data frame containing a data about storms available in the NHC databases. This dataframe 
  #           would contain the following columns: "Year"  "Name"  "Basin" "Link" 
  # products_vec: products vector, this is the product argument for the get_storm_data from the rrricanes pacakge
  
  require(rrricanes)
  
  # make sure we have the list of storms from the NHC
  if(is.null(storm_df)){
    invisible(cat("\tDataframe listing storms available on NHC was not given, getting list of storms form NHC\n"))
    storm_df <- rrricanes::get_storms(years = 1998:2018, basins = c("AL", "EP"))
  }
  
  # storm name must be exact
  if(!(storm_name %in% storm_df$Name)) stop("Storm requested doens't existed in database. Make sure spelling is correct!")
  
  invisible(cat("\tGetting storm data ...\n"))
  storm_data <- storm_df %>%
    filter(Name == storm_name) %>%
    pull(Link) %>%
    get_storm_data(products = products_vec)
  
  return(list(storm_data = storm_data))
}
#========================================================================================#
#================================ End: pull_storm_data ==================================#
#========================================================================================#

#========================================================================================#
#============================== pull_storm_gis_advisories ===============================#
#========================================================================================#
pull_storm_gis_advisories <- function(storm_data_obj = NULL
                                      , advisory_num = NULL
                                      , download_all_advisories = FALSE){
  # Function to get gis storm advisories from the National Hurricane Center
  
  # fuction arguments
  # storm_data_obj: an object that contains data about a given storm. This is usually the output of the 
  # get_storm_data from the rrricanes package or the pull_storm_data function. This object must contain
  # the "fstadv" product
  # advisory_num: the advisory number to download
  # download_all_advisories: download all available advisories. If this is set to FALSE, the function 
  # will download the latest advisory
  
  require(rrricanes)
  
  # get the key for the storm
  key <- storm_data_obj[["fstadv"]] %>% pull(Key) %>% first()
  
  # get a list of the available advisories 
  gis_advisories <- gis_advisory(key = key)
  
  
  if(length(gis_advisories) == 0){
    # this gis advisory data is not available for all storms. 
    stop("\tStorm has no gis advisories, quitting...\n")
  }else{
    invisible(cat("\tGetting advisories for storm key", key,"\n"))
  }
  
  # get the names of advisories
  gis_advisories <- str_split_fixed(gis_advisories, "day_", n = 2)[, 2] 
  gis_advisories <- str_split_fixed(string = gis_advisories, pattern = ".zip", n = 2)[, 1]%>%
    unique()
  
  # if the user specified an advisory number, then just download that advisory GIS files
  if(!is.null(advisory_num)){
    # make sure the advisory number given is valid
    if(sum(advisory_num %in% gis_advisories) == 0){
      invisible(cat("\tadvisory number supplied is not valid for selected storm...\n"))
      adv_num_inv <- TRUE
    }else{
      gis_advisories_list <- gis_advisory(key = key, advisory = advisory_num) %>%
        gis_download()
    }
    # if he instead wants to download all advisories, then do so
  }
  
  # if the advisory number given is found to be invalid, then we need to see if the 
  # user has asked for all the advisory GIS data to be downloaded and if not then 
  # just download the latest advisory data
  
  if(adv_num_inv){
    if(download_all_advisories){
      invisible(cat("\tGetting data for all advisoris, this might take a while...\n"))
      # create an empty list to fill it later with the gis advisory data
      gis_advisories_list <- list()
      for(i in 1:length(gis_advisories)){
        invisible(cat("\tGetting data for advisory ", gis_advisories[i],"\n"))
        gis_advisories_list[[i]] <- gis_advisory(key = key, advisory = gis_advisories[i]) %>%
          gis_download()
      }
    }else{
      # if the user is not requesting to download all advisories, nor did he specify an advisory number, then 
      # just download the latest advisory
      invisible(cat("\tGetting data for latest advisory...\n"))
      gis_advisories_list <- gis_advisory(key = key, advisory = gis_advisories[length(gis_advisories)]) %>%
        gis_download()
    }
  }
  return(list(gis_advisories = gis_advisories, gis_advisories_data = gis_advisories_list))
}
#========================================================================================#
#============================ End: pull_storm_gis_advisories ============================#
#========================================================================================#