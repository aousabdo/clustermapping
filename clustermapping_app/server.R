#######################################################################################
#######################################################################################
#######################################################################################
######### clustermapping is a web application built using R. The app queries  #########
######### data from the clustermapping.us site.                               #########
######### Dr. Aous Abdo <aous.abdo@gmail.com>                                 ######### 
#######################################################################################
#######################################################################################
#######################################################################################

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
  
  #-----------------------------------------------------------------------------------#
  #-------------------------------- strong_clusters_fun ------------------------------#
  #-----------------------------------------------------------------------------------#
  
  strong_clusters_fun <- reactive({
    # this reactive function calls the function that gets the strong clusters for a given region and year
    get_strong_clusters(region_name = input$region_name
                        , regions_dt = regions_dt
                        , year_selected = input$year
                        , meta_data_list = meta_data)
  })
  
  #-----------------------------------------------------------------------------------#
  #---------------------------- End: strong_clusters_fun -----------------------------#
  #-----------------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------------#
  #---------------------------------- cluster_data_fun -------------------------------#
  #-----------------------------------------------------------------------------------#

  cluster_data_fun <- reactive({
    # reactive function to call the get_cluster_data for a given cluster
    
    # get strong clusters 
    strong_clusters <- strong_clusters_fun()[["strong_clusters"]]
    
    # if the user hasn't yet selected a cluster, pick the first one
    if(is.null(input$strong_clusters_rows_selected)) strong_clusters_rows_selected <- 1
    else strong_clusters_rows_selected <- input$strong_clusters_rows_selected
    
    # get the cluster the user clicks on from the strong cluster barplot
    s <- event_data("plotly_click", source = "strong_clusters_barplot")
    
    # if the user hasn't selected a cluster yet, just return the first cluster
    # in the strong cluster barplot for that region, otherwise return cluster 
    # selected by the user
    
    if(is.null(s)) strong_clusters_rows_selected <- 1
    else strong_clusters_rows_selected <- s$pointNumber + 1
    
    # The selection in event_data above doesn't reset when new region is loaded
    # this will cause some problems if the user selectes a cluster with row value
    # greater than the maximum for the next loaded region. To fix this we need to 
    # check the value of the selected row as below
    if(strong_clusters_rows_selected > nrow(strong_clusters)) strong_clusters_rows_selected <- 1
    
    # now we can call the function that gets the cluster data
    get_cluster_data(strong_clusters_dt = strong_clusters
                     , strong_clusters_rows_selected = strong_clusters_rows_selected
                     , clusters_list_input = clusters_list)
  })  
  
  #-----------------------------------------------------------------------------------#
  #------------------------------- End: cluster_data_fun -----------------------------#
  #-----------------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------------#
  #-------------------------------- region_clusters_fun ------------------------------#
  #-----------------------------------------------------------------------------------#
  
  region_clusters_fun <- reactive({
    # reactive function to call the get_region_clusters function which is a function 
    # to get cluster level data including by range of years. See global.R for more 
    # details
    
    # no need for the line below, commenting it out but keeping it just in case I need it later
    # cluster_name       <- cluster_data_fun()$related_clusters_dt$parent_cluster_name %>% unique() %>% as.character()
    
    # get region type
    region_type        <- regions_dt[region_short_name_t == input$region_name, region_type_t]
    
    # call the get_region_clusters function
    region_clusters_dt <- get_region_clusters(cluster = "all"
                                              , region_name = input$region_name
                                              , region_type = region_type
                                              , regions_dt = regions_dt
                                              , year_selected = "all"
                                              , cluster_selected = "all"
                                              , meta_data_list = meta_data)
    return(region_clusters_dt)
  })
  
  #-----------------------------------------------------------------------------------#
  #---------------------------- End: region_clusters_fun -----------------------------#
  #-----------------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------------#
  #--------------------------------- cluster_plots_fun -------------------------------#
  #-----------------------------------------------------------------------------------#
  
  cluster_plots_fun <- reactive({
    # this is the reactive function that calls the functino which makes cluster plots
    # mainly build_cluster_plots
    
    # get regions clusters table
    region_clusters_dt <- region_clusters_fun()
    
    # add short names to the region clusters table
    region_clusters_dt <- add_short_names(clusters_dt = region_clusters_dt
                                          , clusters_list_input = clusters_list
                                          , by_column = "cluster_code")
    
    # now call the build_cluster_plots function
    build_cluster_plots(region_clusters_dt = region_clusters_dt
                        , N_top_clusters = 10
                        , year_selected = input$year
                        , traded_only = FALSE
                        , start_year = 1998
                        , end_year = input$year
                        , meta_data_list = meta_data
                        , use_short_names = TRUE)
  })
  
  #-----------------------------------------------------------------------------------#
  #------------------------------- End: cluster_plots_fun ----------------------------#
  #-----------------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------------#
  #---------------------------------- cluster_emp_fun --------------------------------#
  #-----------------------------------------------------------------------------------#
  
  cluster_emp_fun <- reactive({
    # reactive function to build the cluster employment barplot
    cluster_plots_fun()$cluster_emp
  })
  
  #-----------------------------------------------------------------------------------#
  #-------------------------------- End: cluster_emp_fun -----------------------------#
  #-----------------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------------#
  #---------------------------------- network_viz_fun --------------------------------#
  #-----------------------------------------------------------------------------------#
  
  network_viz_fun <- reactive({
    # reactive function to build network visulaizations
    
    # get cluster data
    cluster_data <- cluster_data_fun()
    
    cluster_data_out <<- copy(cluster_data)
    
    # don't return an error if there is no data in the related clusters table
    if(nrow(cluster_data$related_clusters_dt) == 0) return(NULL)

    # call the function which builds the network visulizations    
    build_network_viz(cluster_data = cluster_data)
  })
  
  #-----------------------------------------------------------------------------------#
  #-------------------------------- End: network_viz_fun -----------------------------#
  #-----------------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------------#
  #------------------------------ strong_clusters_plot_fun ---------------------------#
  #-----------------------------------------------------------------------------------#
  
  strong_clusters_plot_fun <- reactive({
    
    # function to build strong cluster barplot
    
    # get a table of strong clusters for the selected region
    strong_clusters <- strong_clusters_fun()[["strong_clusters"]]
    
    # It is better to use the short names since it will help us with the real-estate on the plots
    strong_clusters[, cluster_name_2 := paste0(cluster_short_name, ", Rank: ", cluster_pos)]
    
    # make a barplot of the strong clusters
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
  
  #-----------------------------------------------------------------------------------#
  #-------------------------- End: strong_clusters_plot_fun --------------------------#
  #-----------------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------------#
  #------------------------------------ donut_chart ----------------------------------#
  #-----------------------------------------------------------------------------------#

  donut_chart <- reactive({
    # reactive function to create donut chart
    cluster_plots_fun()$donut_chart
    })
  
  #-----------------------------------------------------------------------------------#
  #--------------------------------- End: donut_chart --------------------------------#
  #-----------------------------------------------------------------------------------#
  #===================================================================================#
  #============================= End: Reactive Functions =============================#
  #===================================================================================#
  
  #===================================================================================#
  #===================================== Outputs =====================================#
  #===================================================================================#
  
  #-----------------------------------------------------------------------------------#
  #------------------------------ region_cluster_header ------------------------------#
  #-----------------------------------------------------------------------------------#
  
  output$region_cluster_header <- renderText({ 
    # some regions have no strong clusters, this is a custom text to display
    # if the region has a list of strong clusters or non-strong clusters
    
    # find out if the clusters returned from strong_clusters_fun is a strong 
    # cluster or not
    is_strong_cluster <- strong_clusters_fun()[["is_strong_cluster"]]
    
    # return appropriate text
    if(is_strong_cluster) {
      paste0("Strong Clusters in ", input$region_name, ", ", input$year)
    }else{
      paste0("Clusters in ", input$region_name, ", ", input$year)
    }
  })
  
  #-----------------------------------------------------------------------------------#
  #--------------------------- End: region_cluster_header ----------------------------#
  #-----------------------------------------------------------------------------------#
  
  # top clusters table
  output$top_clusters <- DT::renderDataTable(cluster_plots_fun()$top_clusters)
  
  # donut chart
  output$donut_chart <- plotly::renderPlotly(cluster_plots_fun()$donut_chart)
  
  # cluster employement plot
  output$cluster_emp_plot <- plotly::renderPlotly({
    cluster_plots_fun()$cluster_emp
  })
  
  # second employment plot. This is the same as the previous one, we are only doing it cause
  # shiny doesn't allow more than one call to an output
  output$cluster_emp_plot_2 <- plotly::renderPlotly({
    cluster_plots_fun()$cluster_emp
  })

  # cluster wages plot  
  output$cluster_wages <- plotly::renderPlotly(cluster_plots_fun()$cluster_wages)
  
  # job creation over time by cluster
  output$cluster_job_creation <- plotly::renderPlotly(cluster_plots_fun()$cluster_job_creation)  
  
  # basic vis network
  output$vizNetwork_basic <- renderVisNetwork({
    network_viz_fun()$vizNetwork_basic
  }) 
  
  # final network visualization we will use, a more advanced vis network which I worked hard to achieve
  output$vizNetwork_advanced <- renderVisNetwork({
    # get table for all related clusters
    all_related_clusters <- get_all_related_clusters(clusters_list_input = clusters_list)
    
    # get the selected cluster by the user
    selected_cluster <- cluster_data_fun()$related_clusters_dt$parent_cluster_code %>% unique()

    # build the network visulaization
    vis <- build_graph_vis(related_cluster_input = all_related_clusters
                           , clusters_avlbl_input = clusters_avlbl
                           , apply_filters = T
                           , selected_cluster = selected_cluster
                           , visManipulation = F
                           , cluster_network_positions_file = "./data/cluster_network_positions.Rds"
                           , region_name = input$region_name)[[3]]
    return(vis)
  }) 
  
  # focenetwork viz
  output$forceNetwork_Viz <- renderForceNetwork({
    network_viz_fun()$forceNetwork_viz
  })
  
  # sankey diagram
  output$sankeyNetwork_Viz <- renderSankeyNetwork({
    network_viz_fun()$sankeyNetwork_viz
  })
  
  # strong clusters table
  output$strong_clusters <- DT::renderDataTable(expr = {
    strong_clusters <- strong_clusters_fun()[["strong_clusters"]]
    strong_clusters[, .(cluster_name, emp_tl)]
  }, server = FALSE, selection = 'single')
  
  
  # strong cluster plot
  output$strong_clusters_plot <- plotly::renderPlotly({
    strong_clusters_plot_fun()
  })
  
  # table showing related clusters
  output$related_clusters <- shiny::renderDataTable({
    cluster_data_fun()$related_clusters_dt
  })
  
  # table showing sub clusters
  output$sub_clusters <- shiny::renderDataTable({cluster_data_fun()$sub_clusters_dt})
  
  # industries for a given clusters table
  output$industries <- shiny::renderDataTable({cluster_data_fun()$industries})
  
  # combined plots showing the donut charts and employment barplot
  output$combined_plots_1 <- plotly::renderPlotly({
    # make a subplot with one row
    p <- plotly::subplot(nrows = 1
                         , donut_chart()
                         , cluster_emp()
                         , widths = c(0.5, 0.4)
    )
  })
  
  #=========================================================================#
  #=========================================================================#
  #=========================================================================#
  
  # code below was developed to extract the positions of the nodes
  # This code won't be part of the app, but I used it initially 
  # to extract the node positions and manual edges 
  
  vals <- reactiveValues(coords=NULL)
  
  observeEvent(input$getNodes, {
    visNetworkProxy("vizNetwork_advanced") %>% visGetPositions()
    vals$coords <- if(!is.null(input$vizNetwork_advanced_positions)){ 
      do.call(rbind, input$vizNetwork_advanced_positions)
    }
  })
  
  output$test2 <- renderDataTable({
    coords <- vals$coords %>% as.data.table()
    coords_out <- copy(coords)
    coords_out[, x := as.integer(x)]
    coords_out[, y := as.integer(y)]
    saveRDS(coords_out, paste0("./data/cluster_network_positions_", gsub(" |:|-", "", Sys.time()), ".Rds"))
    return(coords)
  })
  
  output$edges_data_from_shiny_text <- renderPrint({
    if(!is.null(input$vizNetwork_advanced_edges)){
      edge_data <- input$vizNetwork_advanced_edges
      saveRDS(edge_data, paste0("./data/cluster_edges_data_", gsub(" |:|-", "", Sys.time()), ".Rds"))
      input$vizNetwork_advanced_edges
    }
  })
  
  observeEvent(input$getEdges, {
    visNetworkProxy("vizNetwork_advanced") %>%
      visGetEdges()
  })
  
  #=========================================================================#
  #=========================================================================#
  #=========================================================================#
  
  #===================================================================================#
  #================================== End: Outputs ===================================#
  #===================================================================================#
})