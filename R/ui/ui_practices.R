library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)

practices_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("county"), "Select County",
                  choices = c("All", kansas_names),
                  selected = "All"),
      selectInput(ns("practice"), "Select Practice Type",
                  choices = c("All", unique(practices$PracticeType)),
                  selected = "All"),
      selectInput(ns("climate_variable"), "Select Climate Variable",
                  choices = c("Maximum Temperature (°C)" = "tmax",
                              "Mean Temperature (°C)" = "tmean",
                              "Minimum Temperature (°C)" = "tmin",
                              "Precipitation (mm)" = "ppt",
                              "Min Vapor Pressure Deficit (hPa)" = "vpdmin",
                              "Max Vapor Pressure Deficit (hPa)" = "vpdmax"),
                  selected = "tmax"),
      checkboxInput(ns("climate_smooth"), "Apply Smoothing", value = FALSE),
      conditionalPanel(
        condition = "input.climate_smooth == true",
        ns = ns,
        selectInput(
          ns("climate_smooth_method"),
          "Smoothing Method",
          choices = c("Binomial" = "binomial", "LOESS" = "loess", "12-Month Mean" = "twelve_month")
        )
      ),
      selectInput(ns("soil_property"), "Soil Property for Map",
                  choices = c("Soil Type" = "soil_type",
                              "Texture" = "texture",
                              "pH" = "ph",
                              "Organic Matter (%)" = "organic_matter",
                              "Hydrologic Group" = "hydrologic_group",
                              "Drainage Class" = "drainage_class",
                              "Erosion Hazard" = "erosion_hazard")),
      sliderInput(ns("soil_opacity"), "Soil Layer Opacity",
                  min = 0, max = 1, value = 0.7, step = 0.1),
      sliderInput(ns("crp_opacity"), "CRP Acres Opacity",
                  min = 0, max = 1, value = 0.5, step = 0.1),
      progressBar(id = ns("soil_progress"), value = 0, total = 1, title = "Loading soils..."),

      actionButton(ns("reset_view"), "Reset to Statewide View", class = "btn btn-primary"),
      tags$div(
        style = "margin-top: 15px; padding: 10px; background-color: #FFFFFF; border: 1px solid #512888; border-radius: 5px;",
        tags$p("Downloads", style = "font-weight: bold; color: #512888; margin-bottom: 10px;"),
        downloadButton(ns("downloadSoilData"), "Download Soil Data", class = "download-btn"),
        tags$div(style = "margin-top: 5px;"),
        downloadButton(ns("downloadPracticePlot"), "Download CRP Plot", class = "download-btn"),
        tags$div(style = "margin-top: 5px;"),
        downloadButton(ns("downloadClimatePlot"), "Download Climate Plot", class = "download-btn")
      )
    ),
    
    mainPanel(
      tags$div(
        tags$p(
          "For illustrative purposes only, soil data from Riley, Coffey, and Pratt counties were preloaded to speed up loading. ",
          "The same soil dataset exists for all Kansas counties; the number of vector features was reduced to keep the app lightweight."
        ),
        
        style = "margin-bottom: 40px; padding: 10px;",
        leafletOutput(ns("practiceMap"), height = 500)
      ),
      tags$div(
        style = "margin-bottom: 40px; padding: 10px;",
        uiOutput(ns("selectedCountyInfo"))
      ),
      tags$div(
        style = "margin-bottom: 40px; padding: 10px;",
        plotlyOutput(ns("practicePlot"), height = 400)
      ),
      tags$div(
        style = "margin-bottom: 40px; padding: 10px;",
        plotlyOutput(ns("climatePlot"), height = 400)
      ),
      tags$div(
        style = "margin-top: 40px; margin-bottom: 40px; padding: 10px;",
        dataTableOutput(ns("practiceTable"))
      )
    )
  )
}