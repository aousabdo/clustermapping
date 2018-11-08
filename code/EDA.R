library(jsonlite)
library(rjson)
library(tidyverse)
library(data.table)
library(splitstackshape)
library(plotly)
library(lubridate)
library(qdapTools) # only to use the list2df, which I could use melt instead
# library(tesseract) # only to do OCR on the clustermapping.us api documentation manual
library(networkD3)
library(visNetwork)
library(igraph)

# setwd("/media/sf_VBox_Shared_Folder/DHS/clustermapping/code/")

base_url <- "http://54.83.53.228/data"

#============================================================================================#
#=================================== Read Manual Data =======================================#
#============================================================================================#
manual_link <- "http://clustermapping.us/sites/default/files/files/page/ClusterMapping-API-Docs.pdf"

# tesseract needs an image, convert the manaual, which is in pdf format, to 
# png and then run OCR on it
# pngfile <- pdftools::pdf_convert(manual_link, dpi = 600)
# 
# text <- tesseract::ocr(pngfile)
# cat(text)

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

#============================================================================================#
#======================================== Regions ===========================================#
#============================================================================================#

regions    <- jsonlite::fromJSON(paste0(base_url,"/region"))
regions.dt <- as.data.table(regions)

# the regions table contains all of the regions available
regions.dt[, table(region_type_t)]

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

# Retrieve non-cluster specific economic data for all indicators for an entire region 
# given the type and code, we can do this by combining the region_type_t and 
# the region_code_t from the orginal regions.sub query
random_region <- regions.sub[name_t == "Michigan" & region_type_t == "state", .(region_type_t, region_code_t)]
year <- max(years)

region_name <- "Fairfax County"
region_name <- "Michigan"
region_name <- "Cook County, IL"
region_name <- "Ingham County, MI"
region_name <- "Puerto Rico"

random_region <- regions.sub[region_short_name_t %like% region_name, .(region_type_t, region_code_t, name_t, region_short_name_t)]
print(random_region)

year <- max(years)

if(nrow(random_region > 1)) random_region <- random_region[1]
random_region <- jsonlite::fromJSON(paste(base_url, "region", random_region[, region_type_t], random_region[, region_code_t], "2016", sep = "/"))
random_region <- as.data.table(random_region)

strong_clusters <- random_region$strong_clusters
cluster_name <- sapply(strong_clusters, function(x) x$name)
cluster_code <- sapply(strong_clusters, function(x) x$code)
cluster_key  <- sapply(strong_clusters, function(x) x$key)
cluster_pos  <- sapply(strong_clusters, function(x) x$pos)

strong_clusters <- cbind(cluster_name, cluster_code, cluster_key, cluster_pos)

strong_clusters <- as.data.table(strong_clusters)
strong_clusters[, cluster_code := as.integer(cluster_code)]
strong_clusters[, cluster_pos := as.integer(cluster_pos)]
strong_clusters[, cluster_name := factor(cluster_name)]
strong_clusters[, cluster_key := factor(cluster_key)]

strong_clusters <- unique(strong_clusters)
setkey(strong_clusters, cluster_key)

print(strong_clusters)

#============================================================================================#
#======================================== Clusters ==========================================#
#============================================================================================#
# get a list of clusters and their subclusters, related clusters etc. 
clusters_list  <- jsonlite::fromJSON(txt = paste0(base_url,"/meta/clusters"), simplifyVector = FALSE)

clusters_ids   <- sapply(clusters_list, function(x) x$id)
clusters_codes <- as.integer(sapply(clusters_list, function(x) x$cluster_code_t))
clusters_names <- sapply(clusters_list, function(x) x$name_t)
clusters_key   <- sapply(clusters_list, function(x) x$key_t)

# put available clusters data in a data.table
clusters_avlble <- data.table(clusters_ids, clusters_codes, clusters_key, clusters_names)

# the clusters_list object we got from the API is not a named list. To make it useful, we need
# to convert it into a named list. We have several options for the names but we'll use the 
# cluster keys as the names for the clusters
names(clusters_list) <- clusters_avlble$clusters_key

# let's pick a random cluster and retrieve the info available about that cluster
random_cluster <- clusters_avlble[sample(.N, 1)]
random_cluster <- clusters_avlble[clusters_key %like% "inan"]

if(nrow(random_cluster) > 1) random_cluster <- random_cluster[1]

invisible(cat("\tWe have randomly selected the", random_cluster[, clusters_key],"cluster\n"))

random_cluster_data <- clusters_list[[random_cluster$clusters_key]] 
N_sub_clusters <- random_cluster_data$sub_clusters %>% length()
N_rel_clusters <- random_cluster_data$related_clusters %>% length()

if(N_sub_clusters > 0){
  invisible(cat("\t This cluster has", N_sub_clusters, "subclusters\n"))
} else invisible(cat("\t This cluster has no subclusters\n"))  

if(N_rel_clusters > 0){ 
  invisible(cat("\t This cluster has", N_rel_clusters, "related clusters\n"))
} else invisible(cat("\t This cluster has no related clusters\n"))  

# get a list of industries, naics, by year
industries_2012 <- random_cluster_data$naics_2012 %>% 
  list2df(col1 = "industry", col2 = "naics") %>%
  as.data.table() %>% 
  mutate(year = 2012)

industries_2007 <- random_cluster_data$naics_2007 %>% 
  list2df(col1 = "industry", col2 = "naics") %>%
  as.data.table() %>% 
  mutate(year = 2007)

industries_2002 <- random_cluster_data$naics_2002 %>% 
  list2df(col1 = "industry", col2 = "naics") %>%
  as.data.table() %>% 
  mutate(year = 2002)

industries_1997 <- random_cluster_data$naics_1997 %>% 
  list2df(col1 = "industry", col2 = "naics") %>%
  as.data.table() %>% 
  mutate(year = 1997)

# put them all in one table
industries <- do.call("rbind", list(industries_2012, industries_2007, industries_2002, industries_1997)) %>%
  as.data.table()

# industries[, cluster := random_cluster[, clusters_key]]

# let's get the data for the related clusters
related_clusters <- random_cluster_data$related_clusters

# convert it to a data.table
do.call(rbind, related_clusters) %>% as.data.table() -> related_clusters

# do some data cleaning etc. 
# list of numerical columns
numeric_cols <- c("cluster_code_t", grep("related", names(related_clusters), v = TRUE))

# convert all columns from lists to characters
related_clusters[, names(related_clusters) := lapply(.SD, as.character)]

# convert the only character column to factor
related_clusters[, cluster_name_t := factor(cluster_name_t)]

# now convert all numerical cols to numeric
related_clusters[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

# add the parent cluster name
related_clusters[, parent_cluster_name := random_cluster[, clusters_names]]

# rearrange column orders to have the parent cluster as the first column
setcolorder(related_clusters, c(ncol(related_clusters), 2, 1, 3:(ncol(related_clusters)-1)))

related_clusters_sub <- related_clusters[, .(parent_cluster_name, cluster_name_t, related_percentage)]

# let's make some pretty plots. These will be network and sankey diagrams
# to create these we need to create nodes and edges as follows

# create nodes: nodes should simply be the cluster names we have 
nodes <- c(related_clusters[1 , parent_cluster_name]
           , as.character(related_clusters$cluster_name_t)) 
nodes <- data.table(cluster_name = nodes) 
nodes[, id := 1:.N]

# since the function we are using requires the links and nodes to start at 0 we have to 
# make sure we do that
nodes_d3 <- mutate(nodes, id = id - 1)

# make sure we scale the weight of the edges to reflect the strength of the relationship
scale_fun <- function(x = NULL){round(x/min(x), 1)}

edges <- data.table(from = 1
                    , to = nodes_d3$id
                    , weight = c(0, scale_fun(related_clusters$related_avg)))

# since the function we are using requires the links and nodes to start at 0 we have to 
# make sure we do that
edges_d3 <- mutate(edges, from = from - 1) %>% as.data.table()

# remove the first row which is the parent cluster with itself
edges_d3 <- edges_d3[2:.N]

# make network graph
forceNetwork(
  Links = edges_d3, Nodes = nodes_d3,  
  Source = "from", Target = "to",      # so the network is directed.
  NodeID = "cluster_name", Group = "id", Value = "weight", 
  opacity = 1, fontSize = 8, zoom = TRUE, opacityNoHover = T
)

# create a sankey network diagram
sankeyNetwork(
  Links = edges_d3, Nodes = nodes_d3, 
  Source = "from", Target = "to", 
  NodeID = "cluster_name", Value = "weight", 
  fontSize = 16)

# let's make some network graphs with the vizNetwork library
# to use the visNetwork package we need to have columns with specifict names
nodes_d3$label <- nodes_d3$cluster_name
edges_d3 <- mutate(edges_d3, width = weight)

# make some network graphs
visNetwork(nodes_d3, edges_d3) %>%
  visLayout(randomSeed = 12)

visNetwork(nodes_d3, edges_d3) %>% 
  # visEdges(arrows = "middle") %>%
  visNodes(color = list(background = "lightblue", 
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10)) %>%
  visLayout(randomSeed = 1234)


#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#

clusters <- jsonlite::fromJSON(paste0(base_url,"/cluster"), flatten = TRUE)
clusters <- jsonlite::flatten(clusters)
clusters <- as.data.table(clusters)
clusters[, cluster_code_t := as.integer(cluster_code_t)]

clusters <- clusters %>% 
  select(name_t, cluster_code_t) %>% 
  unique() %>% 
  arrange(cluster_code_t) %>%
  as.data.table()

# get the clusters for Puerto Rico
# first get the region ids etc from the region table
region_name <- "Puerto Rico"

random_region <- regions.sub[region_short_name_t %like% region_name, .(region_type_t, region_code_t, name_t, region_short_name_t)]
print(random_region)

if(nrow(random_region > 1)) random_region <- random_region[1]

region_cluster <- jsonlite::fromJSON(paste(base_url, "cluster", "all", "all", random_region[, region_type_t], random_region[, region_code_t], sep = "/"))
region_cluster <- as.data.table(region_cluster)

# for any region this retursn all of the clusters including those that the region
# doesn't even have. To exclude these look at the emp_tl > 0
region_cluster[emp_tl > 0, .N]

# for example for Puerto Rico, for which we have almost no data we get no clusters at all

# let's try Loudoun County, VA
region_name <- "Loudoun County, VA"

random_region <- regions.sub[region_short_name_t %like% region_name, .(region_type_t, region_code_t, name_t, region_short_name_t)]
print(random_region)

if(nrow(random_region > 1)) random_region <- random_region[1]

region_cluster <- jsonlite::fromJSON(paste(base_url, "cluster", "all", "all", random_region[, region_type_t], random_region[, region_code_t], sep = "/"))
region_cluster <- as.data.table(region_cluster)

region_cluster[emp_tl > 0, .N]

# amd we have 1047 clusters, most of which are not significant, take a look at the histogram
region_cluster[, hist(emp_tl)]
region_cluster[, hist(log10(emp_tl))]

region_cluster[, year_t := as.integer(year_t)]

setorder(region_cluster,  -year_t, -emp_tl)
region_cluster[traded_b == TRUE, head(.SD, 5), by = year_t][, .(cluster_name_t, emp_tl, year_t)]

# traded vs local clusters
region_cluster[, table(traded_b)]

p <- region_cluster[year_t == 2016] %>% 
  group_by(traded_b) %>%
  summarise(count = n()) %>%
  plot_ly(labels = ~ traded_b, values = ~ count) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Traded vs. Local Clusters",  showlegend = T,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
print(p)

p <- plot_ly(data = region_cluster[year_t == 2016 & traded_b == TRUE] 
             , x = ~ emp_tl
             , y = ~reorder(cluster_name_t, emp_tl)
             , type = 'bar'
             , orientation = "h")
print(p)

p <- plot_ly(data = region_cluster[year_t == 2016 & traded_b == TRUE] 
             , x = ~ private_wage_tf
             , y = ~reorder(cluster_name_t, private_wage_tf)
             , type = 'bar'
             , orientation = "h")
print(p)

p <- plot_ly(data = region_cluster[year_t == 2016 & traded_b == TRUE] 
             , x = ~ region_cluster$
             , y = ~reorder(cluster_name_t, private_wage_tf)
             , type = 'bar'
             , orientation = "h")
print(p)

# get job creation by cluster by year
# first we have to select two years to compare
start_year = min(years)
end_year   = max(years)

job_creation <- region_cluster[(year_t == start_year | year_t == end_year) & traded_b] %>%   
  group_by(cluster_name_t) %>%
  summarise(job_creation_numbers = emp_tl[year_t == end_year] - emp_tl[year_t == start_year]) %>%
  arrange(desc(job_creation_numbers)) %>%
  mutate(change = ifelse(job_creation_numbers >= 0, "Increased", "Decreased"))

p <- plot_ly(data = job_creation
             , y = ~ job_creation_numbers
             , x = ~ reorder(cluster_name_t, -job_creation_numbers)
             , type = "bar") %>%
  layout(margin = list(b = 450), xaxis = list(tickangle = 0))
p

p <- job_creation %>% ggplot(aes(x = reorder(cluster_name_t, -job_creation_numbers), y = job_creation_numbers, fill = change)) 
p <- p + geom_bar(stat = "identity") + scale_fill_manual(values=c("red", "blue")) +
  theme_minimal() + theme(legend.position="none", axis.text.x = element_text(angle = 80, hjust = 1)) + ylab(paste('Job Creation', start_year, "to", end_year, sep = " ")) + xlab("Cluster")
print(p)

gg <- ggplotly(p) 
layout(gg, margin = list(b = 0), xaxis = list(tickangle = 0))

#============================================================================================#
#======================================= NAICS data =========================================#
#============================================================================================#
naics_api <- function(year = 2012
                      , code = NULL
                      , terms = NULL
                      , parent = FALSE
                      , descendant = FALSE
                      , ...){
  if(year != 2012 & year !=2007) stop("\tYear must be either 2012 or 2007\n")
  if(parent & descendant) stop("\tYou can either select a parent or a descendant\n")

  naics_base_url <- "http://api.naics.us/v0/q?year="
  
  query_url      <- paste0(naics_base_url, year) 
  
  if(is.null(code) & is.null(terms)){
    final_query <- query_url
  }else if(!is.null(code) & !is.null(terms)){
    invisible(cat("\tYou supplied both a code and a term for search, term takes precedense.\nIgnoring code supplied\n"))
    code_or_terms <- paste0("&terms=", terms)
    final_query <- paste0(query_url, code_or_terms)
  }else if(!is.null(code)){
    code_or_terms <- paste0("&code=", code)
    final_query <- paste0(query_url, code_or_terms)
  }else{
    code_or_terms <- paste0("&terms=", terms)
    final_query <- paste0(query_url, code_or_terms)
  } 

  query <- jsonlite::fromJSON(final_query, ...)
}
