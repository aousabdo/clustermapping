library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(data.table)
library(leaflet.extras)

source('global.R')

function(input, output, session) {
  
  #################################################################################################
  ################################# build initial leaflet map #####################################
  #################################################################################################
  
  #-----------------------------------------------------------------------------------------------#
  #-------------------------------- strom_data reactive function ---------------------------------#
  #-----------------------------------------------------------------------------------------------#
  storm_data <- reactive({
    # this reactive function will return a list which gives storm data from NHC
    # and a list of affected area
    
    # get information about the advisory
    storm <- irma_gis_advisories_data_all[[input$advisory_number]]
    
    # get the polygons for the advisory
    storm_polygon_name <- names(storm)[str_detect(names(storm), "pgn")]
    
    # conver the advisory polygons to sf object
    irma_pgn_sf <- storm[[storm_polygon_name]] %>% st_as_sf()
    
    # now we call the get_affected_areas function which will return a list containing the
    # storms info and a list of affected areas
    affected <- get_affected_areas(storm_polygon_sf = irma_pgn_sf
                                   , counties_sf_obj = counties_sf
                                   , states_sf_obj = states_sf
                                   , msa_sf_obj = msa_sf
                                   , economic_areas_sf_obj = economic_areas_sf
                                   , verbose = TRUE)
    
    return(list(storm = storm, affected = affected))
  })
  
  #-----------------------------------------------------------------------------------------------#
  #---------------------------------- create initial leaflet map ---------------------------------#
  #-----------------------------------------------------------------------------------------------#
  # Create the map
  output$map <- renderLeaflet({
    
    # get storm data
    tmp <- storm_data()
    storm    <<- tmp$storm # storm data
    affected <<- tmp$affected # affected areas list
    
    m <- build_storm_map(gis_adv_obj = storm
                         , counties_affected = affected$counties_affected
                         , counties_within = affected$counties_within
                         , center_on_storm = input$center_on_storm)
    # if(input$center_on_storm) m <- m %>% clearBounds %>% setView(lng = -93.85, lat = 37.45, zoom = 4)
    return(m)
  })
  #################################################################################################
  ############################## End: build initial leaflet map ###################################
  #################################################################################################
  
  
  #===================================================================================#
  #================================ Reactive Functions ===============================#
  #===================================================================================#
  # declare and build some reactive functions that we'll use later 
  
  #-----------------------------------------------------------------------------------#
  #-------------------------------- strong_clusters_fun ------------------------------#
  #-----------------------------------------------------------------------------------#
  
  strong_clusters_fun <- reactive({
    # region_name_reactive <- storm_data()$affected$counties_affected$region_short_name_t
    # 
    # # just select the first region for now
    # region_name_reactive <- region_name_reactive[1]
    
    region_name_reactive <- input$affected_counties_dynamic

    # if the storm hasn't made land fall, there won't be any affected areas and the region_name_reactive
    # will be an NA. If so, we need to handle this "error"
    
    if(is.na(region_name_reactive) | region_name_reactive == "" | is.null(region_name_reactive)) region_name_reactive <- "Fairfax County, VA"
    
    # this reactive function calls the function that gets the strong clusters for a given region and year
    strong_clusters <<- get_strong_clusters(region_name = region_name_reactive 
                                            , regions_dt = regions_dt
                                            , year_selected = input$year
                                            , meta_data_list = meta_data)
    
    return(strong_clusters)
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
                     , clusters_list_input = clusters_list
                     , verbose = TRUE)
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
    affected <<- storm_data()$affected 
    
    # region_name_reactive <- affected$counties_affected$region_short_name_t
    # region_name_reactive <- region_name_reactive[1]
    
    region_name_reactive <- input$affected_counties_dynamic
    
    if(is.na(region_name_reactive) | region_name_reactive == "" | is.null(region_name_reactive)) region_name_reactive <- "Fairfax County, VA"
    region_type          <- regions_dt[region_short_name_t == region_name_reactive, region_type_t]
    
    # call the get_region_clusters function
    region_clusters_dt <- get_region_clusters(cluster = "all"
                                              , region_name = region_name_reactive
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
    # this is the reactive function that calls the function which makes cluster plots
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
  #------------------------------ query_critical_clusters ----------------------------#
  #-----------------------------------------------------------------------------------#
  query_critical_clusters <- reactive({
    # reactive function to query the critical economic clusters
    parsed_affected_regions <- parse_affected_regions(affected_areas_obj = storm_data()$affected
                                                      , within_ = TRUE)
    
    critical_clusters_by_county <- get_critical_clusters(parsed_affected_regions_list = parsed_affected_regions
                                                         , all_region_clusters_dt = all_region_clusters
                                                         , region_type = "county"
                                                         , filter_emp_tl = TRUE
                                                         , top_N = 10)
    
    critical_clusters_by_msa <- get_critical_clusters(parsed_affected_regions_list = parsed_affected_regions
                                                         , all_region_clusters_dt = all_region_clusters
                                                         , region_type = "msa"
                                                         , filter_emp_tl = TRUE
                                                         , top_N = 10)
    
    critical_clusters_by_economic_area <- get_critical_clusters(parsed_affected_regions_list = parsed_affected_regions
                                                         , all_region_clusters_dt = all_region_clusters
                                                         , region_type = "economic"
                                                         , filter_emp_tl = TRUE
                                                         , top_N = 10)
    
    critical_clusters <- rbind(critical_clusters_by_county
                               , critical_clusters_by_msa
                               , critical_clusters_by_economic_area)
    
    critical_clusters[emp_tl_rank_i < 3, .(region_short_name_t, cluster_name_t, emp_tl, emp_tl_rank_i)]
    
    return(list(critical_clusters_by_county = critical_clusters_by_county
                , critical_clusters_by_msa = critical_clusters_by_msa
                , critical_clusters_by_economic_area = critical_clusters_by_economic_area
                , critical_clusters = critical_clusters))
  })
  #-----------------------------------------------------------------------------------#
  #---------------------------- End: query_critical_clusters -------------------------#
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
    
    # region_name_reactive <- storm_data()$affected$counties_affected$region_short_name_t
    # region_name_reactive <- region_name_reactive[1]
    
    region_name_reactive <- input$affected_counties_dynamic
    
    if(is.na(region_name_reactive) | region_name_reactive == "" | is.null(region_name_reactive)) region_name_reactive <- "Fairfax County, VA"
    
    # make a barplot of the strong clusters
    plot_ly(data = strong_clusters 
            , x = ~emp_tl
            , y = ~reorder(cluster_name_2, -cluster_pos)
            , type = 'bar'
            , orientation = "h"
            , hoverinfo = "text"
            , text = ~paste(reorder(cluster_name, -cluster_pos), "<br>"
                            , "Local Rank:", cluster_pos, "<br>"
                            , "Employment:", emp_tl)
            , source = "strong_clusters_barplot") %>%
      layout(title = paste0("Strong Clusters in ", region_name_reactive, ", ", input$year)  
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
    
    strong_clusters <- strong_clusters_fun()
    
    if(!is.null(strong_clusters)){
      is_strong_cluster <- strong_clusters_fun()[["is_strong_cluster"]]
      
      # region_name_reactive <- storm_data()$affected$counties_affected$region_short_name_t
      # region_name_reactive <- region_name_reactive[1]
      
      region_name_reactive <- input$affected_counties_dynamic
      
      if(is.na(region_name_reactive) | region_name_reactive == "" | is.null(region_name_reactive)) region_name_reactive <- "Fairfax County, VA"
        
      # return appropriate text
      if(is_strong_cluster) {
        paste0("Strong Clusters in ", region_name_reactive, ", ", input$year)
      }else{
        paste0("Clusters in ", region_name_reactive, ", ", input$year)
      }
    }else{
      paste0("No regions are affected")
    }
  })
  
  #-----------------------------------------------------------------------------------#
  #--------------------------- End: region_cluster_header ----------------------------#
  #-----------------------------------------------------------------------------------#
  
  # top clusters table
  output$top_clusters <- DT::renderDataTable(cluster_plots_fun()$top_clusters)
  
  # critical clusters table
  output$critical_clusters <- DT::renderDataTable(query_critical_clusters()$critical_clusters)
  
  output$critical_clusters_text <- renderText({
    tmp <<- query_critical_clusters()$critical_clusters
    # tmp <- setorder(tmp, emp_tl_rank_i)
    foo <- tmp[, head(.SD, 1), by = region_short_name_t][, .(region_short_name_t, cluster_name_t, emp_tl, emp_tl_rank_i)]
    print(foo)
    # tmp <- tmp[emp_tl_rank_i < 3, .(region_short_name_t, cluster_name_t, emp_tl, emp_tl_rank_i)]
    paste0(foo[1, region_short_name_t]
           , " has a critical economic cluster: "
           , foo[1, cluster_name_t]
           , " which is ranked "
           , foo[1, emp_tl_rank_i]
           , " in the US"
           , " with "
           , foo[1, emp_tl], " employment")
  })
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
    
    # region_name_reactive <- storm_data()$affected$counties_affected$region_short_name_t
    # region_name_reactive <- region_name_reactive[1]
    
    region_name_reactive <- input$affected_counties_dynamic
    
    if(is.na(region_name_reactive) | region_name_reactive == "" | is.null(region_name_reactive)) region_name_reactive <- "Fairfax County, VA"
    
    # build the network visulaization
    vis <- build_graph_vis(related_cluster_input = all_related_clusters
                           , clusters_avlbl_input = clusters_avlbl
                           , apply_filters = T
                           , selected_cluster = selected_cluster
                           , visManipulation = F
                           , cluster_network_positions_file = "./data/cluster_network_positions.Rds"
                           , region_name = region_name_reactive)[[3]]
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
  
  output$affected_economic_areas <- renderUI({
    # get storm data
    affected <- storm_data()$affected # affected areas list
    mydata <- parse_affected_regions(affected_areas_obj = affected)$unique_econ_areas$economic_area
    
    selectInput("affected_economic_areas_dynamic", "Affected Economic Areas", mydata)
  })
  
  output$affected_msas <- renderUI({
    # get storm data
    affected <- storm_data()$affected # affected areas list
    mydata <- parse_affected_regions(affected_areas_obj = affected)$unique_msa$region_short_name_t
    
    selectInput("affected_msas_dynamic", "Affected Micropolitan Statistical Areas", mydata)
  })
  
  output$affected_counties <- renderUI({
    # get storm data
    affected <- storm_data()$affected # affected areas list
    mydata <- parse_affected_regions(affected_areas_obj = affected)$unique_counties$region_short_name_t
    
    selectInput("affected_counties_dynamic", "Affected Counties", mydata)
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
}
