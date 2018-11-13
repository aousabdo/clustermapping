library(shiny)

source("./global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$text <- renderDataTable({
    
    # get region's name from input
    region_name <- input$region_name
    
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
                   , region_code, input$year, sep = "/")
    
    selected_region <- jsonlite::fromJSON(query)
    
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
    
    strong_clusters[, cluster_code := as.integer(cluster_code)]
    # the cluster position vector starts at 0, correct that since in R we start at 1
    strong_clusters[, cluster_pos  := as.integer(cluster_pos) + 1] 
    strong_clusters[, cluster_name := factor(cluster_name)]
    strong_clusters[, cluster_key  := factor(cluster_key)]
    
    strong_clusters <- unique(strong_clusters)
    
    # set data.table key
    setkey(strong_clusters, cluster_pos)
    
    # let's only return what we really care about
    strong_clusters <- strong_clusters[, .(cluster_name, cluster_pos)]
    setnames(strong_clusters, c("Cluster Name", "Cluster Rank"))
    
    return(strong_clusters)
  })
  
})
