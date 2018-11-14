library(shiny)
library(DT)

rm(list = ls())

source("./global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  strong_clusters_dt <- reactive({
    # call the function that gets the strong clusters for a given region and year
    get_strong_clusters(region_name = input$region_name
                        , regions_dt = regions_dt
                        , year_selected = input$year)
    })

  cluster_data <- reactive({
    # call the function that gets the cluster data
    get_cluster_data(strong_clusters_dt = strong_clusters_dt()
                     , strong_clusters_rows_selected = input$strong_clusters_rows_selected)
  })
  
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
