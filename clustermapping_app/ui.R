library(shiny)

source('global.R')
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cluster Mapping"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(fluid = TRUE, 
                sidebarPanel(width= 2,
                             selectInput(inputId = "region_name"
                                         , label = "Select a Region"
                                         , choices = region_names), 
                             selectInput(inputId = "year"
                                         , label = "Select a Year"
                                         , choices = meta_data$years_avlbl
                                         , selected = max(meta_data$years_avlbl))
                             , actionButton("getNodes", "Get nodes")
                             , actionButton("getEdges", "Get Edges")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(tabsetPanel(
                  tabPanel("Related Clusters",
                           h1(textOutput("text_1"))
                           , hr()
                           , plotly::plotlyOutput("strong_clusters_plot", height = "auto")
                           , plotly::plotlyOutput("combined_plots_1", height = "auto", width = "1400px")
                           # , visNetworkOutput("vizNetwork_advanced", height = "800px", width = "1000px")
                           # , DT::dataTableOutput("strong_clusters")
                           # , sankeyNetworkOutput("sankeyNetwork_Viz")
                           # , visNetworkOutput("vizNetwork_basic")
                           # , textOutput("region_clusters")
                           
                  ),
                  tabPanel("Region Figures"
                           # , plotly::plotlyOutput("donut_chart", width = "500px", height = "500px")
                           , plotly::plotlyOutput("cluster_emp", width = "1000px", height = "600px")
                           # , verbatimTextOutput("selection")
                           , plotly::plotlyOutput("cluster_wages", width = "1000px", height = "600px")
                           , plotly::plotlyOutput("cluster_job_creation", width = "1000px", height = "1000px")
                           , DT::dataTableOutput("top_clusters")
                  ),
                  tabPanel("Tables",
                           shiny::dataTableOutput("related_clusters"),
                           shiny::dataTableOutput("sub_clusters"),
                           shiny::dataTableOutput("industries")
                           # , forceNetworkOutput("forceNetwork_Viz")
                  ) 
                  , tabPanel("test"
                             , visNetworkOutput("vizNetwork_advanced", height = "800px", width = "1000px")
                             , dataTableOutput("test2")
                             ,   verbatimTextOutput("edges_data_from_shiny_text")
                           )
                  ))
  )
))
