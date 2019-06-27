library(leaflet)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)

navbarPage("EconClust", id="nav",

  tabPanel("Storm Map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      absolutePanel(id = "controls"
                    , class = "panel panel-default"
                    , fixed = TRUE
                    , draggable = TRUE
                    , top = 60
                    , left = "auto"
                    , right = 80
                    , bottom = "auto"
                    , width = 390
                    , height = "auto"

        , h2("EconClust")
        # , selectInput(inputId = "region_name"
        #             , label = "Select a Region"
        #             , choices = region_names)
        , selectInput("storm_name", "Storm", c("Hurricane Irma"))
        , selectInput("advisory_number", "Advisory", irma_gis_advisories_avlbl, selected = "045A")
        , checkboxInput("center_on_storm", "Center Map on Storm", value = TRUE)
        , selectInput(inputId = "year"
                    , label = "Select a Year"
                    , choices = meta_data$years_avlbl
                    , selected = max(meta_data$years_avlbl))
        , htmlOutput("critical_clusters_text")
      ),

      tags$div(id="cite"
        # , 'Data compiled for '
        # , tags$em('Coming Apart: The State of White America, 1960–2010')
        # , ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  tabPanel("Affected Counties"
           , uiOutput('affected_counties')
           , h1(textOutput("region_cluster_header"))
           , br()
           , br()
           , br()
           , plotly::plotlyOutput("strong_clusters_plot", height = "auto")
           , br()
           , br()
           , br()
           , br()
           , visNetworkOutput("vizNetwork_advanced", height = "900px", width = "1000px")
           , br()
           , br()
           , br()
           , br()
           , plotly::plotlyOutput("donut_chart")
           , plotly::plotlyOutput("cluster_emp_plot", height = "1200px")
  ),
  tabPanel("Affected Economic Areas"
           , uiOutput('affected_economic_areas')
  ),
  tabPanel("Affected Micropolitan Statistical Areas"
           , uiOutput('affected_msas')
  ),
  tabPanel("Region Figures"
           # , plotly::plotlyOutput("donut_chart", width = "500px", height = "500px")
           , plotly::plotlyOutput("cluster_emp_plot_2", width = "1000px", height = "600px")
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
  ),
  tabPanel("EconClust Explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
)
