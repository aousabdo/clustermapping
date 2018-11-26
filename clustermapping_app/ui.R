library(shiny)

source('global.R')
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Cluster Mapping"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel( 
      selectInput(inputId = "region_name"
                  , label = "Select a Region"
                  , choices = region_names), 
      selectInput(inputId = "year"
                  , label = "Select a Year"
                  , choices = meta_data$years_avlbl
                  , selected = max(meta_data$years_avlbl))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(h1(textOutput("text_1"))
              , hr()
              , DT::dataTableOutput("strong_clusters"),
              tabsetPanel(
                tabPanel("Related Clusters",
                         sankeyNetworkOutput("sankeyNetwork_Viz"),
                         visNetworkOutput("vizNetwork_basic")
                         # , textOutput("region_clusters")
                ),
                tabPanel("Region Figures"
                         , plotly::plotlyOutput("donut_chart", width = "500px", height = "500px")
                         , plotly::plotlyOutput("cluster_emp", width = "1000px", height = "600px")
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
              )
    )
  )
))
