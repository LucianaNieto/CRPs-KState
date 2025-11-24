library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(tidyr)
library(plotly)
library(htmlwidgets)
library(webshot)
library(shinydashboard)
library(DT)
library(soilDB)

# Source data loading functions
source("R/data/load_crp_data.R")
source("R/data/load_spatial_data.R")
source("R/data/load_soils_sda.R")

# Define file paths
data_path <- "data/raw/"
spatial_path <- "data/spatial/"

# Load and preprocess data
crp_history <- load_crp_history(data_path)
practices <- load_crp_practices(data_path)
expire_data <- load_crp_expiration(data_path)
kansas_counties <- load_kansas_counties(spatial_path)

# Load climate data
climate_data <- readRDS("data/processed/prism_kansas_counties_1985_2024.rds") %>%
  mutate(county_name = toupper(trimws(county_name))) # Adjust based on kansas_counties$name

# Global variables
join_field <- "name"
kansas_names <- unique(kansas_counties[[join_field]])
min_year <- min(crp_history$Year, na.rm = TRUE)
max_year <- max(crp_history$Year, na.rm = TRUE)
year_choices <- as.character(min_year:max_year)

county_to_areasymbol <- data.frame(
  County = kansas_names,
  Areasymbol = paste0("KS", sprintf("%03d", seq(1, by = 2, length.out = length(kansas_names)))),
  stringsAsFactors = FALSE
)

county_to_areasymbol$Areasymbol[county_to_areasymbol$County == "FINNEY"] <- "KS055"
county_to_areasymbol$Areasymbol[county_to_areasymbol$County == "RILEY"] <- "KS161"
county_to_areasymbol$Areasymbol[county_to_areasymbol$County == "WYANDOTTE"] <- "KS209"

# ========================================================================
#  DEPLOY vs LOCAL

DEPLOY_MODE <- FALSE  # change to TRUE to run all in deploy  mode 

if (DEPLOY_MODE) {
  # Just Riley to run in deploy mode
  AVAILABLE_SOIL_COUNTIES <- c("RILEY", "COFFEY", "PRATT")
  message("Running in DEPLOY MODE - Only RILEY county soil data will be available")
} else {
  # all counties - for local use without app size restrictions 
  AVAILABLE_SOIL_COUNTIES <- c("RILEY", "WYANDOTTE") #add more here if needed 
  message("Running in LOCAL MODE - Multiple counties available")
}

# Reactive to track selected county
selected_county <- reactiveVal("All")