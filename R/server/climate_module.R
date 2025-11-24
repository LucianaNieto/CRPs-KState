library(shiny)
library(plotly)
library(dplyr)
library(zoo)

climate_module <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive climate data for selected county
    climate_filtered <- reactive({
      county <- selected_county()
      if (county == "All") {
        return(NULL)
      }
      climate_data %>%
        filter(county_name == toupper(county)) %>%
        arrange(date)
    })
    
    # Smoothing function
    smooth_data <- function(data, variable, method) {
      y <- data[[variable]]
      if (method == "binomial") {
        weights <- c(1, 4, 6, 4, 1) / 16
        y_smooth <- stats::filter(y, weights, sides = 2)
      } else if (method == "loess") {
        x <- seq_along(y)
        fit <- loess(y ~ x, span = 0.2)
        y_smooth <- predict(fit, x)
      } else if (method == "twelve_month") {
        y_smooth <- rollmean(y, k = 12, fill = NA, align = "center")
      }
      return(y_smooth)
    }
    
    # Render climate plot
    output$climatePlot <- renderPlotly({
      data <- climate_filtered()
      if (is.null(data) || nrow(data) == 0) {
        return(plot_ly() %>% layout(title = "Select a county to view climate data"))
      }
      
      variable <- input$climate_variable
      y_label <- switch(variable,
                        "tmax" = "Maximum Temperature (°C)",
                        "tmean" = "Mean Temperature (°C)",
                        "tmin" = "Minimum Temperature (°C)",
                        "ppt" = "Precipitation (mm)",
                        "vpdmin" = "Min Vapor Pressure Deficit (hPa)",
                        "vpdmax" = "Max Vapor Pressure Deficit (hPa)")
      
      p <- plot_ly(data, x = ~date, y = ~.data[[variable]], type = "scatter", mode = "lines",
                   name = "Original Data", line = list(color = "#7B5AA6"))
      
      if (input$climate_smooth && nrow(data) > 0) {
        y_smooth <- smooth_data(data, variable, input$climate_smooth_method)
        p <- p %>% add_trace(x = ~date, y = y_smooth, type = "scatter", mode = "lines",
                             name = paste("Smoothed", input$climate_smooth_method),
                             line = list(color = "#512888", dash = "dash"))
      }
      
      p %>% layout(
        title = list(
          text = paste(y_label, "for", selected_county()),
          font = list(family = "Arial, Helvetica, sans-serif", size = 16, color = "#512888")
        ),
        xaxis = list(
          title = "Date",
          tickfont = list(family = "Arial, Helvetica, sans-serif", size = 12),
          automargin = TRUE
        ),
        yaxis = list(
          title = y_label,
          tickfont = list(family = "Arial, Helvetica, sans-serif", size = 12)
        ),
        plot_bgcolor = "#F5F5F5",
        paper_bgcolor = "#F5F5F5",
        font = list(family = "Arial, Helvetica, sans-serif", size = 12),
        showlegend = TRUE,
        margin = list(b = 100, t = 50)
      )
    })
    
    # Download climate plot
    output$downloadClimatePlot <- downloadHandler(
      filename = function() {
        paste("climate_plot_", selected_county(), "_", input$climate_variable, ".png", sep = "")
      },
      content = function(file) {
        plotly_obj <- plotly_build(output$climatePlot())
        save_image(plotly_obj, file)
      }
    )
  })
}