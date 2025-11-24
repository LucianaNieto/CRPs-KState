library(shiny)
library(leaflet)
library(sf)
library(dplyr)

soil_module <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Fetch soil data from .gpkg and .rds
    fetch_soil_data <- function(county) {

      if (!county %in% AVAILABLE_SOIL_COUNTIES) {
        message("Soil data not available for ", county)
        showNotification(
          paste0("Soil data not available for ", county, ". ",
                 "Available: ", paste(AVAILABLE_SOIL_COUNTIES, collapse = ", ")),
          type = "warning",
          duration = 5
        )
        return(NULL)
      }
      
      areasymbol <- county_to_areasymbol$Areasymbol[county_to_areasymbol$County == county]
      if (length(areasymbol) == 0) {
        message("No areasymbol found for ", county)
        showNotification(paste("No areasymbol found for", county), type = "error")
        return(NULL)
      }
      
      # Define file paths
      gpkg_file <- paste0("data/processed/soils_kansas_", tolower(county), "_clean.gpkg")
      rds_file <- paste0("data/processed/soils_kansas_", tolower(county), "_clean.rds")
      
      # Check if files exist
      if (!file.exists(gpkg_file) || !file.exists(rds_file)) {
        message("Missing files for ", county, ": ", gpkg_file, " or ", rds_file)
        showNotification(paste("Missing soil data files for", county), type = "error")
        return(NULL)
      }
      
      # Update progress
      output$progress_message <- renderText({ paste("Loading soil data for ", county, "...") })
      updateProgressBar(session, "soil_progress", value = 0, total = 1)
      
      tryCatch({
        # Read geometries from .gpkg
        layer_name <- st_layers(gpkg_file)$name[1]
        soil_spatial <- st_read(gpkg_file, layer = layer_name, quiet = TRUE) %>%
          st_transform(4326) %>%
          filter(!st_is_empty(.))
        
        # Read tabular data from .rds
        soil_tabular <- readRDS(rds_file)
        
        # Join data
        soil_data <- soil_spatial %>% 
          left_join(soil_tabular, by = "mukey") %>%
          filter(!is.na(soil_type))
        
        # Check if any data is available
        if (nrow(soil_data) == 0) {
          message("No matching soil data for ", county)
          showNotification("No soil data available after joining", type = "warning")
          return(NULL)
        }
        
        # Update progress
        output$progress_message <- renderText({ 
          paste("Soil data loaded for ", county, " (", nrow(soil_data), " polygons)") 
        })
        updateProgressBar(session, "soil_progress", value = 1, total = 1)
        
        soil_data
      }, error = function(e) {
        message("Error loading soil data for ", county, ": ", e$message)
        showNotification(paste("Error loading soil data for", county, ": ", e$message), type = "error")
        NULL
      })
    }
    
    # Reactive to load soil data when county changes
    soil_data <- reactive({
      county <- selected_county()
      if (county == "All") {
        output$progress_message <- renderText({ 
          paste("Select a county to view soil data. Available: ", 
                paste(AVAILABLE_SOIL_COUNTIES, collapse = ", "))
        })
        updateProgressBar(session, "soil_progress", value = 0, total = 1)
        return(NULL)
      }
      
      fetch_soil_data(county)
    })
    
    # Update map with soil layer and zoom to county
    observe({
      if (is.null(soil_data())) {
        leafletProxy("practiceMap") %>%
          clearGroup("Soils") %>%
          clearControls()
        return()
      }
      
      soil <- soil_data()
      county <- selected_county()
      
      # Zoom to the selected county
      county_geom <- kansas_counties %>% filter(name == county)
      bounds <- st_bbox(county_geom)
      leafletProxy("practiceMap") %>%
        fitBounds(bounds["xmin"], bounds["ymin"], bounds["xmax"], bounds["ymax"])
      
      # Add soil layer
      prop <- input$soil_property
      vals <- soil[[prop]]
      
      # Handle numeric vs. categorical properties
      if (is.numeric(vals)) {
        pal <- colorNumeric("viridis", domain = vals, na.color = "transparent")
        labels <- sprintf("%s: %.1f", prop, vals)
      } else {
        pal <- colorFactor("viridis", domain = vals, na.color = "transparent")
        labels <- sprintf("%s: %s", prop, vals)
      }
      
      message("Rendering ", nrow(soil), " soil geometries for ", county)
      leafletProxy("practiceMap") %>%
        clearGroup("Soils") %>%
        clearControls() %>%
        addPolygons(
          data = soil,
          group = "Soils",
          fillColor = ~pal(vals),
          fillOpacity = input$soil_opacity %||% 0.7,
          weight = 0.5,
          color = "grey",
          label = labels
        ) %>%
        showGroup("Soils") %>%
        addLegend(
          pal = pal,
          values = vals,
          title = prop,
          group = "Soils",
          position = "bottomright"
        )
    })
    
    # Download soil data
    output$downloadSoilData <- downloadHandler(
      filename = function() {
        paste("soil_data_", selected_county(), ".csv", sep = "")
      },
      content = function(file) {
        if (is.null(soil_data())) {
          write.csv(data.frame(Error = "No soil data available"), file, row.names = FALSE)
        } else {
          soil_data_df <- soil_data() %>% st_drop_geometry()
          write.csv(soil_data_df, file, row.names = FALSE)
        }
      }
    )
  })
}