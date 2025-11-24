library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(webshot)

acres_by_year_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Validate inputs
    observe({
      if (!is.numeric(as.numeric(input$year1)) || !is.numeric(as.numeric(input$year2))) {
        showNotification("Please select valid years.", type = "error")
      }
    })
    
    crp_year1 <- reactive({
      req(input$year1)
      year <- as.numeric(input$year1)
      if (!year %in% crp_history$Year) {
        showNotification(paste("No data for year", year), type = "warning")
        return(data.frame())
      }
      message("Filtering crp_history for year ", year)
      crp_history %>% filter(Year == year)
    })
    
    crp_change <- reactive({
      req(input$year1, input$year2)
      year1 <- as.numeric(input$year1)
      year2 <- as.numeric(input$year2)
      if (!year1 %in% crp_history$Year || !year2 %in% crp_history$Year) {
        showNotification("Selected years have no data.", type = "warning")
        return(kansas_counties)
      }
      message("Computing change for years ", year1, " to ", year2)
      year1_data <- crp_history %>% filter(Year == year1)
      year2_data <- crp_history %>% filter(Year == year2)
      merged_data <- left_join(year1_data, year2_data, by = c("STATE", "COUNTY"), suffix = c("_y1", "_y2"))
      message("Merged data has ", nrow(merged_data), " rows")
      if (nrow(merged_data) == 0) {
        showNotification("No matching data for selected years.", type = "warning")
        return(kansas_counties)
      }
      merged_data <- merged_data %>%
        mutate(Change = Acres_y2 - Acres_y1) %>%
        dplyr::select(COUNTY, Change, Acres_y1, Acres_y2)
      message("Selected columns: ", paste(colnames(merged_data), collapse = ", "))
      result <- left_join(kansas_counties, merged_data, by = c("name" = "COUNTY"))
      message("Final crp_change has ", nrow(result), " rows")
      result
    })
    
    yearMap <- reactive({
      data <- left_join(kansas_counties, crp_year1(), by = c("name" = "COUNTY"))
      message("yearMap data has ", nrow(data), " rows")
      pal <- colorNumeric("viridis", domain = data$Acres, na.color = "transparent")
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(Acres),
          weight = 2,
          opacity = 0.5,
          color = "gray",
          dashArray = "1",
          fillOpacity = 0.7,
          layerId = ~name,
          label = ~paste0(name, ": ", Acres, " acres")
        ) %>%
        addLegend(pal = pal, values = ~Acres, opacity = 0.7, title = paste("CRP Acres", input$year1), position = "bottomright") %>%
        addControl(html = paste("<h4>CRP Acres in Kansas -", input$year1, "</h4>"), position = "topright")
    })
    
    output$yearMap <- renderLeaflet({
      yearMap()
    })
    
    output$downloadYearMap <- downloadHandler(
      filename = function() { paste("CRP_Acres_", input$year1, ".png", sep = "") },
      content = function(file) {
        map <- yearMap()
        saveWidget(map, "temp.html", selfcontained = TRUE)
        webshot("temp.html", file = file, vwidth = 800, vheight = 400)
      }
    )
    
    selectedCountyAcres <- reactiveVal(NULL)
    observeEvent(input$yearMap_shape_click, {
      click <- input$yearMap_shape_click
      if (!is.null(click$id)) {
        selectedCountyAcres(click$id)
      } else {
        selectedCountyAcres(NULL)
      }
    })
    
    acresGraph <- reactive({
      if (is.null(selectedCountyAcres())) {
        data <- crp_history %>%
          group_by(Year) %>%
          summarize(Acres = sum(Acres, na.rm = TRUE)) %>%
          mutate(Color = ifelse(Year == as.numeric(input$year1), "#512888", "#A9A9A9"))
        plot_title <- "Total CRP Acres in Kansas Over Time"
      } else {
        data <- crp_history %>%
          filter(COUNTY == selectedCountyAcres()) %>%
          dplyr::select(Year, Acres) %>%
          mutate(Color = ifelse(Year == as.numeric(input$year1), "#512888", "#A9A9A9"))
        plot_title <- paste("CRP Acres in", selectedCountyAcres(), "Over Time")
      }
      message("Rendering acresGraph for ", selectedCountyAcres() %||% "All")
      plot_ly(data, x = ~Year, y = ~Acres, type = "bar", marker = list(color = ~Color),
              hoverinfo = "y+text", text = ~paste(Acres, "acres")) %>%
        layout(
          title = plot_title,
          xaxis = list(title = "Year"),
          yaxis = list(title = "Acres"),
          barmode = "group",
          showlegend = FALSE
        )
    })
    
    output$acresGraph <- renderPlotly({
      acresGraph()
    })
    
    output$downloadAcresGraph <- downloadHandler(
      filename = function() { paste("CRP_Acres_Graph_", input$year1, ".png", sep = "") },
      content = function(file) {
        p <- acresGraph()
        saveWidget(p, "temp_graph.html", selfcontained = TRUE)
        webshot("temp_graph.html", file = file, vwidth = 800, vheight = 300)
      }
    )
    
    changeMap <- reactive({
      data <- crp_change()
      message("changeMap data has ", nrow(data), " rows")
      pal <- colorNumeric(c("#D6604D", "white", "#548235"), domain = c(-max(abs(data$Change), na.rm = TRUE), max(abs(data$Change), na.rm = TRUE)), na.color = "transparent")
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(Change),
          weight = 2,
          opacity = 0.5,
          color = "gray",
          dashArray = "1",
          fillOpacity = 0.7,
          layerId = ~name,
          label = ~paste0(name, ": Change ", Change, " acres (", Acres_y1, " to ", Acres_y2, ")")
        ) %>%
        addLegend(pal = pal, values = ~Change, opacity = 0.7, title = paste("Change in CRP Acres", input$year1, "to", input$year2), position = "bottomright") %>%
        addControl(html = paste("<h4>Change in CRP Acres:", input$year1, "to", input$year2, "</h4>"), position = "topright")
    })
    
    output$changeMap <- renderLeaflet({
      changeMap()
    })
    
    output$downloadChangeMap <- downloadHandler(
      filename = function() { paste("CRP_Change_", input$year1, "_to_", input$year2, ".png", sep = "") },
      content = function(file) {
        map <- changeMap()
        saveWidget(map, "temp.html", selfcontained = TRUE)
        webshot("temp.html", file = file, vwidth = 800, vheight = 400)
      }
    )
    
    selectedCountyChange <- reactiveVal(NULL)
    observeEvent(input$changeMap_shape_click, {
      click <- input$changeMap_shape_click
      if (!is.null(click$id)) {
        selectedCountyChange(click$id)
      } else {
        selectedCountyChange(NULL)
      }
    })
    
    output$changeIndicator <- renderUI({
      if (is.null(selectedCountyChange())) {
        year1_data <- crp_history %>% filter(Year == as.numeric(input$year1)) %>% summarize(Acres = sum(Acres, na.rm = TRUE))
        year2_data <- crp_history %>% filter(Year == as.numeric(input$year2)) %>% summarize(Acres = sum(Acres, na.rm = TRUE))
        if (nrow(year1_data) == 0 || nrow(year2_data) == 0) {
          change_text <- "Insufficient data to compute change for Kansas."
          color <- "black"
        } else {
          acres1 <- year1_data$Acres[1]
          acres2 <- year2_data$Acres[1]
          diff <- acres2 - acres1
          if (diff > 0) {
            change_text <- paste("Kansas: Increase of", format(diff, big.mark = ","), "acres from", input$year1, "to", input$year2)
            color <- "#548235"
          } else if (diff < 0) {
            change_text <- paste("Kansas: Decrease of", format(abs(diff), big.mark = ","), "acres from", input$year1, "to", input$year2)
            color <- "#D6604D"
          } else {
            change_text <- paste("Kansas: No change in acres between", input$year1, "and", input$year2)
            color <- "black"
          }
        }
      } else {
        county_name <- selectedCountyChange()
        year1_data <- crp_history %>% filter(COUNTY == county_name, Year == as.numeric(input$year1))
        year2_data <- crp_history %>% filter(COUNTY == county_name, Year == as.numeric(input$year2))
        if (nrow(year1_data) == 0 || nrow(year2_data) == 0) {
          change_text <- paste(county_name, ": Insufficient data to compute change.")
          color <- "black"
        } else {
          acres1 <- year1_data$Acres[1]
          acres2 <- year2_data$Acres[1]
          diff <- acres2 - acres1
          if (diff > 0) {
            change_text <- paste(county_name, ": Increase of", format(diff, big.mark = ","), "acres from", input$year1, "to", input$year2)
            color <- "#548235"
          } else if (diff < 0) {
            change_text <- paste(county_name, ": Decrease of", format(abs(diff), big.mark = ","), "acres from", input$year1, "to", input$year2)
            color <- "#D6604D"
          } else {
            change_text <- paste(county_name, ": No change in acres between", input$year1, "and", input$year2)
            color = "black"
          }
        }
      }
      tags$div(style = paste("font-size: 16px; font-weight: bold; color:", color), change_text)
    })
  })
}