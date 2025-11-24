home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$total_acres_1986_ui <- renderUI({
      acres_1986 <- crp_history %>%
        filter(Year == 1986) %>%
        summarize(total = sum(Acres, na.rm = TRUE)) %>%
        pull(total)
      div(
        id = "total_acres_1986",
        div(
          class = "value-box",
          div(class = "icon", icon("hourglass-start")),
          div(
            class = "content",
            h3(format(acres_1986, big.mark = ",")),
            p("Total Acres in 1986")
          )
        )
      )
    })
    
    output$total_acres_2024_ui <- renderUI({
      acres_2024 <- crp_history %>%
        filter(Year == 2024) %>%
        summarize(total = sum(Acres, na.rm = TRUE)) %>%
        pull(total)
      div(
        id = "total_acres_2024",
        div(
          class = "value-box",
          div(class = "icon", icon("hourglass-end")),
          div(
            class = "content",
            h3(format(acres_2024, big.mark = ",")),
            p("Total Acres in 2024")
          )
        )
      )
    })
    
    output$most_common_practice_ui <- renderUI({
      top_practices <- practices %>%
        filter(PracticeType != "TOTAL") %>%
        group_by(PracticeType) %>%
        summarize(total_acres = sum(Acres, na.rm = TRUE)) %>%
        arrange(desc(total_acres)) %>%
        slice_head(n = 3) %>%
        pull(PracticeType)
      top_practices_text <- paste(top_practices, collapse = ", ")
      div(
        id = "most_common_practice",
        div(
          class = "value-box",
          div(class = "icon", icon("leaf")),
          div(
            class = "content",
            h3(top_practices_text),
            p("Top 3 Most Common Practices in Kansas")
          )
        )
      )
    })
    
    output$total_counties_ui <- renderUI({
      total_counties <- crp_history %>%
        pull(COUNTY) %>%
        unique() %>%
        length()
      div(
        id = "total_counties",
        div(
          class = "value-box",
          div(class = "icon", icon("map")),
          div(
            class = "content",
            h3(total_counties),
            p("Total Counties with CRP Data")
          )
        )
      )
    })
    
    output$year_min_acres_ui <- renderUI({
      year_min <- crp_history %>%
        group_by(Year) %>%
        summarize(total_acres = sum(Acres, na.rm = TRUE)) %>%
        arrange(total_acres) %>%
        slice(1) %>%
        pull(Year)
      div(
        id = "year_min_acres",
        div(
          class = "value-box",
          div(class = "icon", icon("caret-square-down")),
          div(
            class = "content",
            h3(year_min),
            p("Year with Least Acres")
          )
        )
      )
    })
    
    output$year_max_acres_ui <- renderUI({
      year_max <- crp_history %>%
        group_by(Year) %>%
        summarize(total_acres = sum(Acres, na.rm = TRUE)) %>%
        arrange(desc(total_acres)) %>%
        slice(1) %>%
        pull(Year)
      div(
        id = "year_max_acres",
        div(
          class = "value-box",
          div(class = "icon", icon("caret-square-up")),
          div(
            class = "content",
            h3(year_max),
            p("Year with More Acres")
          )
        )
      )
    })
  })
}