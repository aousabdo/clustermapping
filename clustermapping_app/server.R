library(shiny)
library(DT)

# start with a clean slate as always
rm(list = ls())

# most of the heavy lifting is in the global.R file, source that to get access to all the 
# functions in that file
source("./global.R")

shinyServer(function(input, output) {
  
  #===================================================================================#
  #================================ Reactive Functions ===============================#
  #===================================================================================#
  # declare and build some reactive functions that we'll use later 
  strong_clusters_dt <- reactive({
    # call the function that gets the strong clusters for a given region and year
    get_strong_clusters(region_name = input$region_name
                        , regions_dt = regions_dt
                        , year_selected = input$year
                        , meta_data_list = meta_data)
  })
  
  cluster_data <- reactive({
    strong_clusters <- strong_clusters_dt()[["strong_clusters"]]
    strong_clusters_out <<- copy(strong_clusters)
    
    # if the user hasn't yet selected a cluster, pick the first one
    if(is.null(input$strong_clusters_rows_selected)) strong_clusters_rows_selected <- 1
    else strong_clusters_rows_selected <- input$strong_clusters_rows_selected
    
    s <- event_data("plotly_click", source = "strong_clusters_barplot")
    
    if(is.null(s)) strong_clusters_rows_selected <- 1
    else strong_clusters_rows_selected <- s$pointNumber + 1
    
    # call the function that gets the cluster data
    get_cluster_data(strong_clusters_dt = strong_clusters
                     , strong_clusters_rows_selected = strong_clusters_rows_selected
                     , clusters_list_input = clusters_list)
  })  
  
  region_clusters <- reactive({
    foo <<- cluster_data()
    cluster_name  <- cluster_data()$related_clusters_dt$parent_cluster_name %>% unique() %>% as.character()
    region_type   <- regions_dt[region_short_name_t == input$region_name, region_type_t]
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
    
    # add short names
    region_clusters <- add_short_names(clusters_dt = region_clusters
                                       , clusters_list_input = clusters_list
                                       , by_column = "cluster_code")
    
    build_cluster_plots(region_clusters_dt = region_clusters
                        , N_top_clusters = 10
                        , year_selected = 2016
                        , traded_only = F
                        , start_year = 1998
                        , end_year = 2016
                        , meta_data_list = meta_data
                        , use_short_names = TRUE)
  })
  
  cluster_emp <- reactive({
    cluster_plots()$cluster_emp
  })
  
  network_viz <- reactive({
    # call the function which builds the network visulizations
    cluster_data <- cluster_data()
    
    cluster_data_out <<- copy(cluster_data)
    # don't return an error if there is no data in the related clusters table
    if(nrow(cluster_data$related_clusters_dt) == 0) return(NULL)
    build_network_viz(cluster_data = cluster_data)
  })
  
  strong_clusters_plot <- reactive({
    strong_clusters <- strong_clusters_dt()[["strong_clusters"]]
    
    # strong_clusters[, cluster_name_2 := paste0(cluster_name, ", Rank: ", cluster_pos)]
    # It is better to use the short names since it will help us with the real-estate on the plots
    strong_clusters[, cluster_name_2 := paste0(cluster_short_name, ", Rank: ", cluster_pos)]
    
    plot_ly(data = strong_clusters 
            , x = ~emp_tl
            , y = ~reorder(cluster_name_2, -cluster_pos)
            , type = 'bar'
            , orientation = "h"
            , source = "strong_clusters_barplot") %>%
      layout(title = paste0("Strong Clusters in ", input$region_name, ", ", input$year)  
             , xaxis = list(title = "Employment", showgrid = TRUE, zeroline = TRUE, showticklabels = TRUE)
             , yaxis = list(title = "",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE)
             , margin = list(l = 350, r = 50, b = 50, t = 50, pad = 4))
  })
  #===================================================================================#
  #============================= End: Reactive Functions =============================#
  #===================================================================================#
  
  #===================================================================================#
  #===================================== Outputs =====================================#
  #===================================================================================#
  output$text_1 <- renderText({ 
    is_strong_cluster <- strong_clusters_dt()[["is_strong_cluster"]]
    if(is_strong_cluster) {
      paste0("Strong Clusters in ", input$region_name, ", ", input$year)
    }else{
      paste0("Clusters in ", input$region_name, ", ", input$year)
    }
  })
  
  output$top_clusters <- DT::renderDataTable(cluster_plots()$top_clusters)
  
  output$donut_chart <- plotly::renderPlotly(cluster_plots()$donut_chart)
  donut_chart <- reactive({cluster_plots()$donut_chart})
  
  output$cluster_emp <- plotly::renderPlotly({
    # s <- event_data("plotly_click", source = "barplot")
    # print(as.list(s))
    cluster_plots()$cluster_emp
  })
  
  output$cluster_wages <- plotly::renderPlotly(cluster_plots()$cluster_wages)
  output$cluster_job_creation <- plotly::renderPlotly(cluster_plots()$cluster_job_creation)  
  
  
  output$vizNetwork_basic <- renderVisNetwork({
    network_viz()$vizNetwork_basic
  }) 
  
  output$vizNetwork_advanced <- renderVisNetwork({
    vis <- build_graph_vis(related_cluster_input = all_related_clusters
                           , clusters_avlbl_input = clusters_avlbl
                           , apply_filters = T)[[3]]
  }) 
  
  output$forceNetwork_Viz <- renderForceNetwork({
    network_viz()$forceNetwork_viz
  })
  
  output$sankeyNetwork_Viz <- renderSankeyNetwork({
    network_viz()$sankeyNetwork_viz
  })
  
  output$strong_clusters <- DT::renderDataTable(expr = {
    strong_clusters <- strong_clusters_dt()[["strong_clusters"]]
    strong_clusters[, .(cluster_name, emp_tl)]
  }, server = FALSE, selection = 'single')
  
  
  output$strong_clusters_plot <- plotly::renderPlotly({
    strong_clusters_plot()
  })
  output$related_clusters <- shiny::renderDataTable({
    cluster_data()$related_clusters_dt
  })
  
  output$sub_clusters <- shiny::renderDataTable({cluster_data()$sub_clusters_dt})
  
  output$industries <- shiny::renderDataTable({cluster_data()$industries})
  
  output$combined_plots_1 <- plotly::renderPlotly({
    p <- plotly::subplot(nrows = 1
                         , donut_chart()
                         , cluster_emp()
                         , widths = c(0.5, 0.4)
                         )
  })

  output$network <- renderVisNetwork({
    nodes <- data.frame(id = 1:3); edges <- data.frame(from = c(1,2), to = c(1,3))
    visNetwork(nodes, edges) %>% visNodes(color = "green")
  })
  output$test <- renderPrint({input$vizNetwork_advanced_positions})
  observe({
    input$getNodes
    visNetworkProxy("vizNetwork_advanced") %>%
      visGetPositions()
  })
  #===================================================================================#
  #================================== End: Outputs ===================================#
  #===================================================================================#
})
