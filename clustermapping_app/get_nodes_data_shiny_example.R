server <- function(input, output) {
  output$network <- renderVisNetwork({
    nodes <- data.frame(id = 1:3); edges <- data.frame(from = c(1,2), to = c(1,3))
    visNetwork(nodes, edges) %>% visNodes(color = "green")
  })
  output$test <- renderPrint({input$network_positions})
  observe({
    input$goButton
    visNetworkProxy("network") %>%
      visGetPositions()
  }) }
ui                                                                                                                                                                                                                                                        <- fluidPage(
  fluidRow(
    column(10,visNetworkOutput("network", height = "100%"),
           verbatimTextOutput("test")),
    column(2, actionButton("goButton", "Go!"))
  )
)
shinyApp(ui = ui, server = server)
