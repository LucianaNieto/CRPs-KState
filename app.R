# ====================================================================
#  Project:        CRPs in Kansas
#  Author:         Luciana Nieto PhD
#  Organization:   Kansas State University 
#  Created:        2025-02-17
#  Last Updated:   2025-12-01
#
#  Notes:
#    - for questions related to this project please contact Luciana at luciananieto@icloud.com
#  --------------------------------------------------------------------
#  License: MIT License
#  Copyright (c) 2025
#
#  This script is part of the CRPs porject  and is intended
#  for internal and research use. Redistribution or modification is
#  permitted under the terms of the MIT License.
# ====================================================================


library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(DT)
library(MASS)
library(shinybusy)
library(shinyWidgets)

# Verify dependencies
required_packages <- c("shiny", "shinydashboard", "leaflet", "sf", "dplyr", "plotly", "DT", "shinybusy", "shinyWidgets")
missing_packages <- required_packages[!required_packages %in% installed.packages()]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

# Source global settings and data
source("R/global.R")

# Source UI and server modules
source("R/ui/ui_home.R")
source("R/ui/ui_acres.R")
source("R/ui/ui_practices.R")
source("R/ui/ui_expiration.R")
source("R/server/server_home.R")
source("R/server/server_acres.R")
source("R/server/crp_module.R")
source("R/server/soil_module.R")
source("R/server/climate_module.R") # New climate module
source("R/server/server_expiration.R")

# Define UI
ui <- fluidPage(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  div(class = "navbar", "CRP County-Level Analysis for Kansas"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Home", home_ui("home")),
    tabPanel("Acres by Year", acres_by_year_ui("acres")),
    tabPanel("Practices", practices_ui("practices"))
    #,
#    tabPanel("Expiration", expiration_ui("expiration"))
  )
)

# Define Server
server <- function(input, output, session) {
  home_server("home")
  acres_by_year_server("acres")
  crp_module("practices")
  soil_module("practices")
  climate_module("practices") # Call climate module
#  expiration_server("expiration")
}

# Run the app
shinyApp(ui, server)
