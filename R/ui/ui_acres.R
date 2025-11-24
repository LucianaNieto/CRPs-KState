acres_by_year_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("year1"), "Select Year for Map", choices = year_choices, selected = min_year),
      selectInput(ns("year2"), "Select Year to Compare", choices = year_choices, selected = max_year),
      helpText("Map 1: Acres for the selected year. Graph: Acres over time. Map 2: Change between the two years.")
    ),
    mainPanel(
      leafletOutput(ns("yearMap"), height = 400),
      downloadButton(ns("downloadYearMap"), "Download Map 1", class = "download-btn"),
      br(),
      plotlyOutput(ns("acresGraph"), height = 300),
      downloadButton(ns("downloadAcresGraph"), "Download Graph", class = "download-btn"),
      br(),
      leafletOutput(ns("changeMap"), height = 400),
      downloadButton(ns("downloadChangeMap"), "Download Map 2", class = "download-btn"),
      br(),
      uiOutput(ns("changeIndicator"))
    )
  )
}