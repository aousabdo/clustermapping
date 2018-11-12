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
                   , choices = region_names)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
