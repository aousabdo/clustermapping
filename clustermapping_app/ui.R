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
    mainPanel(
      h1('Strong Clusters in selected region'), hr(), 
      DT::dataTableOutput("strong_clusters"), 
      DT::dataTableOutput("related_clusters")
    )
  )
))
