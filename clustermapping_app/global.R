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
clusters_avlbl <- clusters_data$clusters_avlbl

#========================================================================================#
#=================================== get_strong_clusters ================================#
#========================================================================================#
get_strong_clusters <- function(region_name = NULL
                                , regions_dt = NULL
                                , year_selected = 2016
                                , meta_data_list = meta_dta
                                , base_url = "http://54.83.53.228/data"
                                , verbose = TRUE){
  # given a region name, a regions_dt, and a year, this function will return a 
  # data.table object with the strong clusters for that region
  
  # region_name: valid name for a region
  # regions_dt: a valid data.table regions object. This data.table is produced with the 
  # data_ETL.R code
  # year_selected: a valid year in the form YYYY. Valid values can be found in the 
  # meta_data list which is produced by the data_ETL.R code
  
  if(regions_dt[region_short_name_t == region_name, .N] == 0) stop("\tRegion selected is not valid...\n")
  
  # filter the regions data.table for the selected region
  selected_region_info <<- regions_dt[region_short_name_t == region_name
                                      , .(region_type_t, region_code_t, name_t, region_short_name_t)]
  
  # in some cases, the region_code is between 1 and 9, in this case
  # if we call the api with this integer we'll get an error since the 
  # api expects a 01, 02, etc. so we need to fix this
  region_code <- selected_region_info[, region_code_t]
  if(nchar(region_code) == 1) region_code = paste0("0", region_code)
  
  # get general data about the selected region
  query <- paste(base_url, "region", selected_region_info[, region_type_t]
                 , region_code, year_selected, sep = "/")
  
  if(verbose) invisible(cat("\tRunning the following query: ", query, "\n"))
  
  selected_region <<- jsonlite::fromJSON(query)
  
  # in some cases, the region selected would have no strong clusters, in that case
  # we'll just return the region clusters
  
  # convert to data.table object
  selected_region <- as.data.table(selected_region)
  
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
    
    strong_clusters <<- unique(strong_clusters)
    
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
  return(list(strong_clusters = strong_clusters, is_strong_cluster = is_strong_cluster))
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
  
  selected_cluster_name <- selected_cluster[, cluster_name] %>% as.character()
  if(N_sub_clusters > 0){
    invisible(cat("\tThe cluster", selected_cluster_name, "has", N_sub_clusters, "subclusters\n"))
    
    sub_clusters_dt <- selected_cluster_data$sub_clusters 
    sub_clusters_dt <- do.call(rbind, sub_clusters_dt) %>% as.data.table()
    
    # add the parent cluster name
    sub_clusters_dt[, parent_cluster_name := selected_cluster[, cluster_name]]
    
    # rearrange column orders to have the parent cluster as the first column
    setcolorder(sub_clusters_dt, c(2, 1))
    setnames(sub_clusters_dt, c("parent_cluster_name", "sub_cluster_name"))
    
  } else{
    invisible(cat("\tThe cluster", selected_cluster_name, "has no subclusters\n"))  
    sub_clusters_dt <- data.table()
  }
  
  if(N_rel_clusters > 0){ 
    invisible(cat("\tThe cluster", selected_cluster_name, "has", N_rel_clusters, "related clusters\n"))
    
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
    # related_clusters_dt[, cluster_name_t := factor(cluster_name_t)]
    
    # now convert all numerical cols to numeric
    related_clusters_dt[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]
    
    # add the parent cluster name
    related_clusters_dt[, parent_cluster_name := selected_cluster[, cluster_name]]
    
    # rearrange column orders to have the parent cluster as the first column
    setcolorder(related_clusters_dt, c(ncol(related_clusters_dt), 2, 1, 3:(ncol(related_clusters_dt)-1)))
    
    related_clusters_dt <<- related_clusters_dt[, .(parent_cluster_name, cluster_name_t, related_percentage)]
  } else{
    invisible(cat("\tThe cluster", selected_cluster_name, "has no related clusters\n"))
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
                                , verbose = TRUE){
  
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
  
  if(verbose) invisible(cat("\tRunning the following query: ", query, "\n"))
  region_cluster_dt <- jsonlite::fromJSON(query, simplifyVector = TRUE)
  region_cluster_dt <- as.data.table(region_cluster_dt)
  
  foo2 <<- copy(region_cluster_dt)
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
                                , meta_data_list = meta_data){
  
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
    add_pie(hole = 0.6) %>%
    layout(title = paste0("\nTraded vs. Local Clusters, ", year_selected),  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  p2 <- plot_ly(data = region_cluster[year_t == year_selected & traded_b == TRUE] 
                , x = ~ emp_tl
                , y = ~reorder(cluster_name_t, emp_tl)
                , type = 'bar'
                , orientation = "h") %>%
    layout(title = paste0("\nEmployment by Traded Cluster, ", year_selected),  
           xaxis = list(title = paste0("Employment, ", year_selected), showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           yaxis = list(title = "",showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           margin = list(l = 350, r = 50, b = 50, t = 50, pad = 4))
  
  p3 <- plot_ly(data = region_cluster[year_t == year_selected & traded_b == TRUE] 
                , x = ~ private_wage_tf
                , y = ~reorder(cluster_name_t, private_wage_tf)
                , type = 'bar'
                , orientation = "h") %>%
    layout(title = paste0("\nWages by Traded Cluster, ", year_selected),  
           xaxis = list(title = paste0("Wages, ", year_selected), showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           yaxis = list(title = "", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE),
           margin = list(l = 350, r = 50, b = 50, t = 50, pad = 4))
  
  # get job creation by cluster by year
  job_creation <- region_clusters_dt[(year_t == start_year | year_t == end_year) & traded_b] %>%   
    group_by(cluster_name_t) %>%
    summarise(job_creation_numbers = emp_tl[year_t == end_year] - emp_tl[year_t == start_year]) %>%
    arrange(desc(job_creation_numbers)) %>%
    mutate(change = ifelse(job_creation_numbers >= 0, "Increased", "Decreased"))
  
  p4 <- job_creation %>% 
    ggplot(aes(x = reorder(cluster_name_t, -job_creation_numbers)
               , y = job_creation_numbers
               , fill = change
               , text = paste0(cluster_name_t, "\nJob Creation:", job_creation_numbers))) 
  
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
  # we'll just order the clusters by the number of employments
  
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
  strong_clusters <- strong_clusters[, .(cluster_name, cluster_code, cluster_key, cluster_pos)]
  
  return(strong_clusters)
}
#========================================================================================#
#======================= End: region_clusters_to_strong_clusters ========================#
#========================================================================================#