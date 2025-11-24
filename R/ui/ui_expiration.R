expiration_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "tab-panel",
    fluidRow(
      column(
        12,
        h3("CRP Expiration Data"),
        p("Placeholder for expiration data analysis (to be developed).")
      )
    )
  )
}