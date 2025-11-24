home_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "tab-panel",
    fluidRow(
      column(
        12,
        h3("CRP Summary Statistics"),
        div(
          class = "summary-container",
          style = "display: flex; flex-wrap: wrap; gap: 15px; justify-content: space-around;",
          uiOutput(ns("total_acres_1986_ui")),
          uiOutput(ns("total_acres_2024_ui")),
          uiOutput(ns("year_max_acres_ui")),
          uiOutput(ns("year_min_acres_ui")),
          uiOutput(ns("total_counties_ui")),
          uiOutput(ns("most_common_practice_ui"))
        )
      )
    ),
    fluidRow(
      column(
        12,
        h3("About the Project"),
        div(
          class = "project-container",
          p(
            "This project analyzes Conservation Reserve Program (CRP) data at the county level in Kansas. ",
            "It provides insights into historical trends, current practices, and opportunities."
          ),

          tags$dl(

            tags$dt("Principal Investigator"),
            tags$dd("Dr. Charles Rice, University Distinguished Professor, Department of Agronomy, Kansas State University"),

            tags$dt("Research Team"),
            tags$dd("Dr. Luciana Nieto, Postdoctoral Fellow, Department of Agronomy, Kansas State University"),

            tags$dt("Website"),
            tags$dd(
              tags$a(
                href = "https://www.ksusoilmicrobes.com",
                target = "_blank", rel = "noopener",
                "ksusoilmicrobes.com"
              )
            )
            #,
            
           # tags$dt("Funding"),
          #  tags$dd("[Donor Names/Organizations]")
          ),
          
          br(),
          div(
            style = "display: flex; gap: 20px; align-items: center; flex-wrap: wrap;",
            img(src = "ksu-purple.svg", height = 50, alt = "Kansas State University"),
            img(src = "logoevergy.png", height = 50, alt = "Evergy"),
            img(src = "tnc-logo-primary-registered-dark-text.svg", height = 50, alt = "The Nature Conservancy")
          ),
          br(),
          

          tags$dl(
            tags$dt("Contact"),
            tags$dd("Nieto, Luciana: luciananieto@icloud.com ,  Rice, Charles: cwrice@k-state.edu" )
          )
        )
      )
    ),
    
    tags$style(HTML("
      .project-container dl { margin: 0; }
      .project-container dt {
        font-weight: 700;
        margin-top: 8px;
      }
      .project-container dd {
        margin-left: 0;
        margin-bottom: 6px;
      }
    "))
  )
}
