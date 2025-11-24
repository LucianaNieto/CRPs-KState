library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(DT)
library(shinybusy)

practices_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Cache for soil data
    soils_cache <- reactiveVal(list())
    
    # Initialize default county selection
    observeEvent(input$tabs, {
      message("Current tab:", input$tabs)
      if (!is.null(input$tabs) && input$tabs == "Practices") {
        message("Practices tab activated, setting default county to All")
        updateSelectInput(session, "county", selected = "All")
        showNotification("Loading data for Riley and Wyandotte...", type = "message", duration = NULL, id = "loading")
      }
    })
    
    # Load soil data
    soils_sf <- reactive({
      selected_county <- input$county %||% "All"
      message("Loading soil data for ", selected_county)
      
      # Check cache
      cache <- soils_cache()
      if (selected_county %in% names(cache) && nrow(cache[[selected_county]]) > 0) {
        message("Using cached soil data for ", selected_county)
        return(cache[[selected_county]])
      }
      
      # Show loading spinner
      add_busy_spinner(spin = "cube", color = "#000000", margins = c(40, 40), height = "120px", width = "120px")
      
      if (selected_county != "All") {
        # Load single county
        county_file <- tolower(gsub("[^a-zA-Z0-9]", "", selected_county))
        file_path <- paste0("data/processed/soils_kansas_", county_file, "_clean.gpkg")
        message("Attempting to load ", file_path)
        
        if (!file.exists(file_path)) {
          message("File not found: ", file_path)
          showNotification(paste("Soil data file not found for", selected_county), type = "error", id = "loading")
          return(st_sf(data.frame(county = selected_county), geometry = st_sfc()))
        }
        
        soils <- tryCatch({
          mapunits <- st_read(file_path, quiet = TRUE, columns = c("mukey"))
          message("Loaded ", nrow(mapunits), " mapunits for ", selected_county)
          if (nrow(mapunits) == 0) {
            message("Empty GPKG file for ", selected_county)
            showNotification(paste("Empty soil geometry file for", selected_county), type = "warning", id = "loading")
            return(st_sf(data.frame(county = selected_county), geometry = st_sfc()))
          }
          
          data <- readRDS(paste0("data/processed/soils_kansas_", county_file, "_clean.rds"))
          message("Loaded ", nrow(data), " data rows for ", selected_county)
          if (nrow(data) == 0) {
            message("Empty RDS file for ", selected_county)
            showNotification(paste("Empty soil data file for", selected_county), type = "warning", id = "loading")
            return(st_sf(data.frame(county = selected_county), geometry = st_sfc()))
          }
          
          # Use left_join to keep all geometries
          soils <- left_join(mapunits, data, by = "mukey")
          message("Joined to ", nrow(soils), " rows for ", selected_county)
          if (nrow(soils) == 0) {
            message("No geometries after join for ", selected_county)
            showNotification(paste("No soil geometries after join for", selected_county), type = "warning", id = "loading")
            return(st_sf(data.frame(county = selected_county), geometry = st_sfc()))
          }
          
          soils
        }, error = function(e) {
          message("Error loading soil data for ", selected_county, ": ", e$message)
          showNotification(paste("Error loading soil data for ", selected_county, ": ", e$message), type = "error", id = "loading")
          st_sf(data.frame(county = selected_county), geometry = st_sfc())
        })
        
        cache[[selected_county]] <- soils
        soils_cache(cache)
        showNotification(paste("Soil data loaded for", selected_county), type = "message", id = "loading")
        soils
      } else {
        combined_soils <- st_sf(data.frame(), geometry = st_sfc())
        errors <- character()
        
        # Limit to two counties for testing
        counties_to_load <- c("RILEY", "WYANDOTTE")
        message("Loading soils for test counties:", paste(counties_to_load, collapse = ", "))
        
        for (i in seq_along(counties_to_load)) {
          county <- counties_to_load[i]
          output$progress_message <- renderText({
            paste("Loading soil data for", county, "...")
          })
          updateProgressBar(session, "soil_progress", value = i, total = length(counties_to_load))
          
          county_file <- tolower(gsub("[^a-zA-Z0-9]", "", county))
          file_path <- paste0("data/processed/soils_kansas_", county_file, "_clean.gpkg")
          message("Attempting to load ", file_path)
          
          if (!file.exists(file_path)) {
            message("File not found: ", file_path)
            errors <- c(errors, paste("Missing file for", county))
            showNotification(paste("Soil data file not found for", county), type = "error", id = "loading")
            next
          }
          
          soils <- tryCatch({
            mapunits <- st_read(file_path, quiet = TRUE, columns = c("mukey"))
            message("Loaded ", nrow(mapunits), " mapunits for ", county)
            if (nrow(mapunits) == 0) {
              message("Empty GPKG file for ", county)
              errors <- c(errors, paste("Empty GPKG file for", county))
              showNotification(paste("Empty soil geometry file for", county), type = "warning", id = "loading")
              return(st_sf(data.frame(county = county), geometry = st_sfc()))
            }
            
            data <- readRDS(paste0("data/processed/soils_kansas_", county_file, "_clean.rds"))
            message("Loaded ", nrow(data), " data rows for ", county)
            if (nrow(data) == 0) {
              message("Empty RDS file for ", county)
              errors <- c(errors, paste("Empty RDS file for", county))
              showNotification(paste("Empty soil data file for", county), type = "warning", id = "loading")
              return(st_sf(data.frame(county = county), geometry = st_sfc()))
            }
            
            # Use left_join to keep all geometries
            soils <- left_join(mapunits, data, by = "mukey")
            message("Joined to ", nrow(soils), " rows for ", county)
            if (nrow(soils) == 0) {
              message("No geometries after join for ", county)
              errors <- c(errors, paste("No geometries after join for", county))
              showNotification(paste("No soil geometries after join for", county), type = "warning", id = "loading")
              return(st_sf(data.frame(county = county), geometry = st_sfc()))
            }
            
            soils
          }, error = function(e) {
            message("Error loading soil data for ", county, ": ", e$message)
            errors <- c(errors, paste("Error for", county, ":", e$message))
            showNotification(paste("Error loading soil data for", county, ": ", e$message), type = "error", id = "loading")
            st_sf(data.frame(county = county), geometry = st_sfc())
          })
          
          cache[[county]] <- soils
          soils_cache(cache)
          
          if (nrow(soils) > 0) {
            combined_soils <- rbind(combined_soils, soils)
          }
          
          output$global_progress <- renderText({
            loaded_counties <- sum(names(cache) %in% counties_to_load)
            paste("Soil data loaded for", loaded_counties, "of", length(counties_to_load), "test counties")
          })
          
          leafletProxy("practiceMap") %>%
            clearGroup("Soils") %>%
            addPolygons(
              data = combined_soils,
              group = "Soils",
              fillColor = ~colorFactor("viridis", combined_soils[[input$soil_property %||% "texture"]], na.color = "transparent")(combined_soils[[input$soil_property %||% "texture"]]),
              fillOpacity = input$soil_opacity %||% 0.7,
              weight = 0.5,
              color = "grey",
              label = ~paste0(input$soil_property %||% "texture", ": ", combined_soils[[input$soil_property %||% "texture"]])
            ) %>%
            showGroup("Soils")
          
          Sys.sleep(0.1)
        }
        
        if (length(errors) > 0) {
          showNotification(paste("Errors:", paste(errors, collapse = "; ")), type = "error", duration = 10)
        }
        
        if (nrow(combined_soils) > 0) {
          cache[["All"]] <- combined_soils
          soils_cache(cache)
          showNotification("Soil data loaded for test counties", type = "message", id = "loading")
        } else {
          showNotification("Failed to load soil data for test counties. Please check files.", type = "error", id = "loading")
        }
        
        combined_soils
      }
    })
    
    # Per-county progress message
    output$progress_message <- renderText({
      selected_county <- input$county %||% "All"
      cache <- soils_cache()
      if (selected_county %in% names(cache) && nrow(cache[[selected_county]]) > 0) {
        return(paste("Soil data loaded for", selected_county))
      }
      paste("Loading soil data for", selected_county, "...")
    })
    
    # Global progress indicator
    output$global_progress <- renderText({
      cache <- soils_cache()
      loaded_counties <- sum(names(cache) %in% c("RILEY", "WYANDOTTE"))
      if ("All" %in% names(cache)) {
        loaded_counties <- max(loaded_counties, 2)
      }
      paste("Soil data loaded for", loaded_counties, "of 2 test counties")
    })
    
    # Prepare CRP data aggregated by county
    crp_data <- reactive({
      message("Preparing CRP data for ", input$county %||% "All")
      if (!exists("practices") || nrow(practices) == 0) {
        message("No practices data available")
        showNotification("No CRP data available", type = "error")
        return(data.frame(COUNTY = character(), PracticeType = character(), Acres = numeric()))
      }
      
      data <- practices %>%
        mutate(COUNTY = toupper(trimws(COUNTY)))
      message("Available counties in practices: ", paste(unique(data$COUNTY), collapse = ", "))
      
      if (!is.null(input$county) && input$county != "All") {
        selected_county <- toupper(input$county)
        data <- data %>% filter(COUNTY == selected_county)
        if (nrow(data) == 0) {
          message("No CRP data for ", selected_county)
          showNotification(paste("No CRP data for", input$county), type = "warning")
        }
      }
      if (!is.null(input$practice) && input$practice != "All") {
        data <- data %>% filter(PracticeType == input$practice)
      }
      message("CRP data has ", nrow(data), " rows")
      data
    })
    
    # Join CRP data with county geometries
    crp_sf <- reactive({
      agg_data <- crp_data() %>%
        group_by(COUNTY) %>%
        summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop")
      message("Aggregated CRP data has ", nrow(agg_data), " counties")
      
      result <- kansas_counties %>%
        mutate(name = toupper(trimws(name))) %>%
        left_join(agg_data, by = c("name" = "COUNTY")) %>%
        mutate(Acres = ifelse(is.na(Acres), 0, Acres))
      message("CRP sf object has ", nrow(result), " geometries with ", sum(result$Acres > 0), " non-zero Acres")
      result
    })
    
    # Initial map with hybrid base and CRP layer
    output$practiceMap <- renderLeaflet({
      message("Rendering initial map with CRP data")
      
      # Prepare initial CRP data
      agg_data <- practices %>%
        mutate(COUNTY = toupper(trimws(COUNTY))) %>%
        group_by(COUNTY) %>%
        summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop")
      
      crp_initial <- kansas_counties %>%
        mutate(name = toupper(trimws(name))) %>%
        left_join(agg_data, by = c("name" = "COUNTY")) %>%
        mutate(Acres = ifelse(is.na(Acres), 0, Acres))
      
      vals <- crp_initial$Acres
      pal <- colorNumeric("Blues", domain = vals, na.color = "transparent")
      
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Labels", options = providerTileOptions(opacity = 0.5)) %>%
        setView(lng = -98.5, lat = 38.5, zoom = 7) %>%
        addPolygons(
          data = kansas_counties,
          fill = FALSE,
          color = "black",
          weight = 1,
          group = "Counties",
          layerId = ~name,
          highlightOptions = highlightOptions(
            color = "blue",
            weight = 2,
            bringToFront = TRUE
          )
        ) %>%
        addPolygons(
          data = crp_initial,
          group = "CRP",
          fillColor = ~pal(vals),
          fillOpacity = 0.5,
          weight = 0.5,
          color = "grey",
          label = ~paste0("CRP Acres: ", round(vals, 2)),
          layerId = ~name
        ) %>%
        addLegend(
          pal = pal,
          values = vals,
          title = "CRP Acres",
          group = "CRP",
          position = "bottomleft"
        ) %>%
        addLayersControl(
          baseGroups = c("Satellite", "Labels"),
          overlayGroups = c("Counties", "Soils", "CRP"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        showGroup("Satellite") %>%
        showGroup("Labels") %>%
        showGroup("CRP")
    })
    
    # Update soil layer
    observe({
      sl <- soils_sf()
      leafletProxy("practiceMap") %>%
        clearGroup("Soils") %>%
        clearControls()
      
      if (nrow(sl) == 0) {
        message("No soil data to render for ", input$county %||% "All")
        showNotification("No soil data available", type = "warning")
        return()
      }
      
      prop <- input$soil_property %||% "texture"
      vals <- sl[[prop]]
      
      if (prop == "organic_matter") {
        valid_vals <- vals[!is.na(vals) & suppressWarnings(!is.nan(as.numeric(vals)))]
        if (length(valid_vals) > 0 && all(!is.na(as.numeric(valid_vals)))) {
          valid_vals <- as.numeric(valid_vals)
          pal <- colorNumeric("YlOrRd", domain = valid_vals, na.color = "transparent")
        } else {
          vals[is.na(vals)] <- "Unknown"
          pal <- colorFactor("Greys", domain = vals, na.color = "transparent")
        }
      } else {
        vals[is.na(vals)] <- "Unknown"
        pal <- colorFactor("viridis", domain = vals, na.color = "transparent")
      }
      
      message("Rendering ", nrow(sl), " soil geometries for ", input$county %||% "All")
      leafletProxy("practiceMap") %>%
        addPolygons(
          data = sl,
          group = "Soils",
          fillColor = ~pal(vals),
          fillOpacity = input$soil_opacity %||% 0.7,
          weight = 0.5,
          color = "grey",
          label = ~paste0(prop, ": ", vals)
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
    
    # Update CRP layer
    observe({
      crp <- crp_sf()
      if (nrow(crp) == 0) {
        message("No CRP data to render for ", input$county %||% "All")
        showNotification("No CRP data available", type = "warning")
        return()
      }
      
      vals <- crp$Acres
      if (all(vals == 0)) {
        message("All CRP Acres are zero for ", input$county %||% "All")
        showNotification("No non-zero CRP data available", type = "warning")
        return()
      }
      
      pal <- colorNumeric("Blues", domain = vals, na.color = "transparent")
      
      message("Rendering CRP data with ", nrow(crp), " geometries for ", input$county %||% "All")
      leafletProxy("practiceMap") %>%
        clearGroup("CRP") %>%
        addPolygons(
          data = crp,
          group = "CRP",
          fillColor = ~pal(vals),
          fillOpacity = input$crp_opacity %||% 0.5,
          weight = 0.5,
          color = "grey",
          label = ~paste0("CRP Acres: ", round(vals, 2)),
          layerId = ~name
        ) %>%
        showGroup("CRP") %>%
        addLegend(
          pal = pal,
          values = vals,
          title = "CRP Acres",
          group = "CRP",
          position = "bottomleft"
        )
    })
    
    # Update county selection on map click
    observeEvent(input$practiceMap_shape_click, {
      click <- input$practiceMap_shape_click
      if (!is.null(click$id)) {
        updateSelectInput(session, "county", selected = click$id)
        showNotification(paste("Selected", click$id), type = "message")
      }
    })
    
    # Practice Type plot
    output$practicePlot <- renderPlotly({
      plot_data <- crp_data() %>%
        group_by(PracticeType) %>%
        summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
        mutate(PracticeType = ifelse(is.na(PracticeType), "Unknown", PracticeType))
      
      message("Rendering plot with ", nrow(plot_data), " practice types")
      if (nrow(plot_data) == 0) {
        showNotification("No CRP data available for plot", type = "warning")
        return(plot_ly() %>% layout(title = "No CRP data available", xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      
      plot_ly(
        data = plot_data,
        x = ~PracticeType,
        y = ~Acres,
        type = "bar",
        marker = list(color = "skyblue")
      ) %>%
        layout(
          title = paste("CRP Practices for", input$county %||% "All", "(Jan 2024)"),
          xaxis = list(title = "Practice Type", tickangle = 45),
          yaxis = list(title = "Acres"),
          showlegend = FALSE
        )
    })
    
    # Practices table
    output$practiceTable <- renderDataTable({
      datatable(
        crp_data() %>%
          group_by(COUNTY, PracticeType) %>%
          summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop"),
        options = list(pageLength = 5)
      )
    })
    
    # Selected county info
    output$selectedCountyInfo <- renderUI({
      HTML(paste("<p>Showing data for:", input$county %||% "All", "</p>"))
    })
    
    # Download plot
    output$downloadPracticePlot <- downloadHandler(
      filename = function() {
        paste("practice_plot_", input$county %||% "All", ".png", sep = "")
      },
      content = function(file) {
        plotly_obj <- plotly_build(output$practicePlot())
        save_image(plotly_obj, file)
      }
    )
  })
}