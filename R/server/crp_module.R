library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(DT)

crp_module <- function(id) {
  moduleServer(id, function(input, output, session) {
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
        data <- data %>% filter(PracticeType == input$practice | PracticeType == "Total")
      }
      message("CRP data has ", nrow(data), " rows")
      data
    })
    
    # Join CRP data with county geometries
    crp_sf <- reactive({
      agg_data <- crp_data() %>%
        filter(PracticeType != "Total") %>% # Exclude Total for map aggregation
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
        filter(PracticeType != "Total") %>%
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
        selected_county(click$id)
      }
    })
    
    # Practice Type plot
    output$practicePlot <- renderPlotly({
      plot_data <- crp_data() %>%
        group_by(PracticeType) %>%
        summarise(Acres = sum(Acres, na.rm = TRUE), .groups = "drop") %>%
        mutate(PracticeType = ifelse(is.na(PracticeType), "Unknown", PracticeType))
      
      # Separate Total and other practice types
      total_data <- plot_data %>% filter(PracticeType == "Total")
      other_data <- plot_data %>% filter(PracticeType != "Total")
      
      # Select top 10 practice types (or all if fewer)
      top_data <- other_data %>%
        arrange(desc(Acres)) %>%
        slice_head(n = 10)
      
      # Combine top types and Total
      plot_data <- bind_rows(top_data, total_data) %>%
        mutate(PracticeType = factor(PracticeType, levels = c(setdiff(PracticeType, "Total"), "Total")))
      
      message("Rendering plot with ", nrow(plot_data), " practice types")
      if (nrow(plot_data) == 0) {
        showNotification("No CRP data available for plot", type = "warning")
        return(plot_ly() %>% layout(title = "No CRP data available", xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
      }
      
      # Assign colors
      colors <- rep("#7B5AA6", nrow(plot_data))
      colors[plot_data$PracticeType == "Total"] <- "#512888"
      
      plot_ly(
        data = plot_data,
        x = ~PracticeType,
        y = ~Acres,
        type = "bar",
        marker = list(color = colors),
        hoverinfo = "y+text",
        text = ~paste(round(Acres, 2), "acres")
      ) %>%
        layout(
          title = list(
            text = paste("CRP Practices for", input$county %||% "All", "(Jan 2024)"),
            font = list(family = "Arial, Helvetica, sans-serif", size = 16, color = "#512888")
          ),
          xaxis = list(
            title = "Practice Type",
            tickangle = 45,
            tickfont = list(family = "Arial, Helvetica, sans-serif", size = 12),
            automargin = TRUE
          ),
          yaxis = list(
            title = "Acres",
            tickfont = list(family = "Arial, Helvetica, sans-serif", size = 12)
          ),
          bargap = 0.2,
          plot_bgcolor = "#F5F5F5",
          paper_bgcolor = "#F5F5F5",
          font = list(family = "Arial, Helvetica, sans-serif", size = 12),
          showlegend = FALSE,
          margin = list(b = 100, t = 50)
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
    
    # Reset view
    observeEvent(input$reset_view, {
      updateSelectInput(session, "county", selected = "All")
      selected_county("All")
      leafletProxy("practiceMap") %>%
        clearGroup("Soils") %>%
        clearControls() %>%
        setView(lng = -98.5, lat = 38.5, zoom = 7)
    })
  })
}