#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("./global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$text <- renderDataTable({
    # get regions name from input
    region_name <- input$region_name
    
    selected_region <- regions_dt[region_short_name_t %like% region_name
                                  , .(region_type_t, region_code_t, name_t, region_short_name_t)]
  
    selected_region <- jsonlite::fromJSON(paste(base_url, "region", selected_region[, region_type_t], selected_region[, region_code_t], input$year, sep = "/"))
    selected_region <- as.data.table(selected_region)
    
    strong_clusters <- selected_region$strong_clusters

    cluster_name <- sapply(strong_clusters, function(x) x$name)
    cluster_code <- sapply(strong_clusters, function(x) x$code)
    cluster_key  <- sapply(strong_clusters, function(x) x$key)
    cluster_pos  <- sapply(strong_clusters, function(x) x$pos)
    
    strong_clusters <- cbind(cluster_name, cluster_code, cluster_key, cluster_pos)
    
    strong_clusters <- as.data.table(strong_clusters)
    strong_clusters[, cluster_code := as.integer(cluster_code)]
    strong_clusters[, cluster_pos  := as.integer(cluster_pos)]
    strong_clusters[, cluster_name := factor(cluster_name)]
    strong_clusters[, cluster_key  := factor(cluster_key)]
    
    strong_clusters <- unique(strong_clusters)
    setkey(strong_clusters, cluster_key)
    return(strong_clusters)
  })
  
})
