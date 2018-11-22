library(shiny)
library(DT)

rm(list = ls())

source("./global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$text_1 <- renderText({ 
    paste0("Strong Clusters in ", input$region_name, ", ", input$year)
  })
  
  strong_clusters_dt <- reactive({
    # call the function that gets the strong clusters for a given region and year
    get_strong_clusters(region_name = input$region_name
                        , regions_dt = regions_dt
                        , year_selected = input$year
                        , meta_data_list = meta_data)
  })
  
  cluster_data <- reactive({
    strong_clusters <- strong_clusters_dt()
    
    # if the user hasn't yet selected a cluster, pick the first one
    if(is.null(input$strong_clusters_rows_selected)) strong_clusters_rows_selected <- 1
    else strong_clusters_rows_selected <- input$strong_clusters_rows_selected
    
    # call the function that gets the cluster data
    get_cluster_data(strong_clusters_dt = strong_clusters
                     , strong_clusters_rows_selected = strong_clusters_rows_selected)
  })
  
  region_clusters <- reactive({
    cluster_name <- cluster_data()$related_clusters_dt$parent_cluster_name %>% unique() %>% as.character()
    region_type <- regions_dt[region_short_name_t == input$region_name, region_type_t]
    region_clusters <- get_region_clusters(cluster = "all"
                                           , region_name = input$region_name
                                           , region_type = region_type
                                           , regions_dt = regions_dt
                                           , year_selected = "all"
                                           , cluster_selected = "all"
                                           , meta_data_list = meta_data)
    return(region_clusters)
  })
  
  cluster_plots <- reactive({
    region_clusters <- region_clusters()
    foo3 <<- region_clusters
    build_cluster_plots(region_clusters_dt = region_clusters
                        , N_top_clusters = 100
                        , year_selected = 2016
                        , traded_only = F
                        , start_year = 1998
                        , end_year = 2016
                        , meta_data_list = meta_data)
  })
  
  output$top_clusters <- DT::renderDataTable(cluster_plots()$top_clusters)
  output$donut_chart <- plotly::renderPlotly(cluster_plots()$donut_chart)
  output$cluster_emp <- plotly::renderPlotly(cluster_plots()$cluster_emp)
  output$cluster_wages <- plotly::renderPlotly(cluster_plots()$cluster_wages)
  output$cluster_job_creation <- plotly::renderPlotly(cluster_plots()$cluster_job_creation)  
    
  network_viz <- reactive({
    # call the function which builds the network visulizations
    build_network_viz(cluster_data = cluster_data())
  })
  
  output$vizNetwork_basic <- renderVisNetwork({network_viz()$vizNetwork_basic}) 
  output$forceNetwork_Viz <- renderForceNetwork({network_viz()$forceNetwork_viz})
  output$sankeyNetwork_Viz <- renderSankeyNetwork({network_viz()$sankeyNetwork_viz})
  
  output$strong_clusters <- DT::renderDataTable(expr = {
    strong_clusters <- strong_clusters_dt()
    strong_clusters[, .(cluster_name)]
  }, server = FALSE, selection = 'single')
  
  output$related_clusters <- shiny::renderDataTable({cluster_data()$related_clusters_dt})
  
  output$sub_clusters <- shiny::renderDataTable({cluster_data()$sub_clusters_dt})
  
  output$industries <- shiny::renderDataTable({cluster_data()$industries})
})
